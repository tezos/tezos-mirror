(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Baking_state
open Baking_state_types
module Round = Protocol.Alpha_context.Round

module Events = struct
  include Baking_events.Forge_worker
  include Baking_events.Actions
end

(** [High_watermark] is an internal module that stores:
    - the highest seen level
    - the highest seen round for each level *)
module High_watermark = struct
  module LRU =
    Aches.Vache.Map (Aches.Vache.LRU_Precise) (Aches.Vache.Strong)
      (struct
        type t = int32

        let equal = Int32.equal

        let hash = Int32.hash
      end)

  type t = {mutable highest_level : int32; round_per_level : Round.t LRU.t}

  (** [create ()] returns a fresh highwatermark *)
  let create () = {highest_level = Int32.zero; round_per_level = LRU.create 5}

  (* [check t level round] checks that the level and round are not lower
     than the ones already seen *)
  let check {highest_level; round_per_level} level round =
    (* levels below [highest_level - 2] are discarded as too old *)
    if level < Int32.(pred (pred highest_level)) then false
    else
      (* for the current level, check that the last seen round is not greater
         than the current round *)
      match LRU.find_opt round_per_level level with
      | Some round_at_level -> round > round_at_level
      | None -> true

  (** [push t level round] stores [level] and [level -> round] if each is higher
      than the currently stored one *)
  let push t level round =
    t.highest_level <- Int32.max level t.highest_level ;
    LRU.replace t.round_per_level level round
end

module Delegate_high_watermarks = struct
  type delegate_highwatermarks = {
    blocks : High_watermark.t;
    preattestations : High_watermark.t;
    attestations : High_watermark.t;
  }

  let create_delegate_highwatermarks () =
    {
      blocks = High_watermark.create ();
      preattestations = High_watermark.create ();
      attestations = High_watermark.create ();
    }

  type t = delegate_highwatermarks Key_id.Table.t

  let create_highwatermarks () = Key_id.Table.create 13

  let get_delegate_highwatermarks t (delegate : Baking_state_types.Delegate.t) =
    let key = delegate.consensus_key.id in
    match Key_id.Table.find_opt t key with
    | Some h -> h
    | None ->
        let h = create_delegate_highwatermarks () in
        Key_id.Table.replace t key h ;
        h
end

module Delegate_signing_queue = struct
  type t = {
    delegate : Baking_state_types.Delegate.t;
    task_stream : (unit -> unit Lwt.t) Lwt_stream.t;
    push : (unit -> unit Lwt.t) option -> unit;
    worker : unit Lwt.t;
  }

  let start_delegate_worker_queue task_stream =
    let open Lwt_syntax in
    let rec loop () =
      let* task = Lwt_stream.get task_stream in
      match task with
      | None -> (* End of stream *) return_unit
      | Some task ->
          let* () = task () in
          loop ()
    in
    loop ()

  let create delegate =
    let task_stream, push = Lwt_stream.create () in
    let worker = start_delegate_worker_queue task_stream in
    {delegate; task_stream; push; worker}

  let cancel_pending_tasks state = Lwt_stream.junk_old state.task_stream

  let wait_all_tasks_and_close state =
    state.push None ;
    state.worker

  let cancel_all_tasks_and_close state =
    let open Lwt_syntax in
    let* () = cancel_pending_tasks state in
    wait_all_tasks_and_close state

  let push_task ~(on_error : tztrace -> unit Lwt.t)
      (f : unit -> unit tzresult Lwt.t) state =
    let open Lwt_result_syntax in
    let task () =
      let*! r =
        protect
          ~on_error:(fun trace ->
            let*! () = on_error trace in
            return_unit)
          (fun () -> f ())
      in
      match r with Error _err -> assert false | Ok () -> Lwt.return_unit
    in
    state.push (Some task)
end

module Name = struct
  type t = Chain_id.t

  let encoding = Chain_id.encoding

  (* The base needs to contain the protocol name as it is used to derive events.
     Without it, the events will be duplicated between different protocols. *)
  let base = [Protocol.name; "forge"; "worker"]

  let pp = Chain_id.pp_short

  let equal = Chain_id.equal
end

module Request = struct
  type (_, _) content =
    (* The Empty.t type is used to ensure the fact that this case can not be
       associated to any error *)
    | Forge_and_sign_block : block_to_bake -> (unit, Empty.t) content
    | Forge_and_sign_preattestations : {
        unsigned_preattestations : unsigned_consensus_vote_batch;
      }
        -> (unit, error trace) content
    | Forge_and_sign_attestations : {
        unsigned_attestations : unsigned_consensus_vote_batch;
      }
        -> (unit, error trace) content

  type ('res, 'error) t = {
    automaton_state : automaton_state;
    content : ('res, 'error) content;
  }

  type view =
    | Forge_and_sign_block of Block_hash.t
    | Forge_and_sign_preattestations of Block_hash.t
    | Forge_and_sign_attestations of Block_hash.t

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Forge_and_sign_block"
          (obj2
             (req "request" (constant "forge_and_sign_block"))
             (req "predecessor" Block_hash.encoding))
          (function Forge_and_sign_block h -> Some ((), h) | _ -> None)
          (fun ((), h) -> Forge_and_sign_block h);
        case
          (Tag 1)
          ~title:"Forge_and_sign_preattestations"
          (obj2
             (req "request" (constant "Forge_and_sign_preattestations"))
             (req "block" Block_hash.encoding))
          (function
            | Forge_and_sign_preattestations h -> Some ((), h) | _ -> None)
          (fun ((), h) -> Forge_and_sign_preattestations h);
        case
          (Tag 2)
          ~title:"Forge_and_sign_attestations"
          (obj2
             (req "request" (constant "Forge_and_sign_attestations"))
             (req "block" Block_hash.encoding))
          (function Forge_and_sign_attestations h -> Some ((), h) | _ -> None)
          (fun ((), h) -> Forge_and_sign_attestations h);
      ]

  let view (type a b) (req : (a, b) t) : view =
    match req.content with
    | Forge_and_sign_block block_to_bake ->
        Forge_and_sign_block block_to_bake.predecessor.hash
    | Forge_and_sign_preattestations {unsigned_preattestations} ->
        Forge_and_sign_preattestations unsigned_preattestations.batch_branch
    | Forge_and_sign_attestations {unsigned_attestations} ->
        Forge_and_sign_attestations unsigned_attestations.batch_branch

  let pp ppf = function
    | Forge_and_sign_block hash ->
        Format.fprintf ppf "Forge_and_sign_block %a" Block_hash.pp hash
    | Forge_and_sign_preattestations hash ->
        Format.fprintf
          ppf
          "Forge_and_sign_preattestations %a"
          Block_hash.pp
          hash
    | Forge_and_sign_attestations hash ->
        Format.fprintf ppf "Forge_and_sign_attestations %a" Block_hash.pp hash
end

module Types = struct
  type state = {
    delegate_signing_queues : Delegate_signing_queue.t Key_id.Table.t;
    baking_state : Baking_state.global_state;
    forge_consensus_vote_hook : (unit -> unit Lwt.t) option;
    delegate_highwatermarks : Delegate_high_watermarks.t;
  }

  type parameters = {
    baking_state : Baking_state.global_state;
    forge_consensus_vote_hook : (unit -> unit Lwt.t) option;
  }
end

module Worker = Tezos_workers.Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

let cancel_all_pending_tasks worker =
  let state = Worker.state worker in
  Lwt.dont_wait
    (fun () ->
      Key_id.Table.iter_p
        (fun _ queue -> Delegate_signing_queue.cancel_pending_tasks queue)
        state.delegate_signing_queues)
    (fun _exn -> ())

let shutdown worker =
  let state = Worker.state worker in
  let open Lwt_syntax in
  let* () =
    Key_id.Table.iter_p
      (fun _ queue -> Delegate_signing_queue.cancel_all_tasks_and_close queue)
      state.delegate_signing_queues
  in
  Worker.trigger_shutdown worker ;
  return_unit

let get_or_create_queue state delegate =
  match
    Key_id.Table.find_opt
      state.Types.delegate_signing_queues
      delegate.Baking_state_types.Delegate.consensus_key.id
  with
  | None ->
      let queue = Delegate_signing_queue.create delegate in
      Key_id.Table.add
        state.delegate_signing_queues
        delegate.consensus_key.id
        queue ;
      queue
  | Some queue -> queue

let handle_forge_block (state : Types.state) automaton_state
    (block_to_bake : block_to_bake) =
  let open Lwt_result_syntax in
  let level = Int32.succ block_to_bake.predecessor.shell.level in
  let round = block_to_bake.round in
  let delegate_hwms =
    Delegate_high_watermarks.get_delegate_highwatermarks
      state.delegate_highwatermarks
      block_to_bake.delegate
  in
  let block_highwatermark = delegate_hwms.blocks in
  if High_watermark.check block_highwatermark level round then (
    let queue = get_or_create_queue state block_to_bake.delegate in
    High_watermark.push block_highwatermark level round ;
    let task () =
      let* prepared_block =
        Baking_actions.prepare_block
          automaton_state
          state.baking_state
          block_to_bake
      in
      automaton_state.push_forge_event (Block_ready prepared_block) ;
      return_unit
    in
    Delegate_signing_queue.push_task
      ~on_error:(fun err ->
        let open Lwt_syntax in
        let* () =
          Events.(emit failed_to_forge_block (block_to_bake.delegate, err))
        in
        Lwt.return_unit)
      task
      queue)

let handle_forge_consensus_votes (state : Types.state) automaton_state
    (unsigned_consensus_votes : unsigned_consensus_vote_batch) =
  let open Lwt_result_syntax in
  let batch_branch = unsigned_consensus_votes.batch_branch in
  let level =
    unsigned_consensus_votes.batch_content.level
    |> Protocol.Alpha_context.Raw_level.to_int32
  in
  let round = unsigned_consensus_votes.batch_content.round in
  let task unsigned_consensus_vote =
    (* Execute test hook if present *)
    let*! () =
      match state.forge_consensus_vote_hook with
      | None -> Lwt.return_unit
      | Some hook_fn -> hook_fn ()
    in
    let*! signed_consensus_vote_r =
      Baking_actions.forge_and_sign_consensus_vote
        automaton_state
        state.baking_state
        ~branch:batch_branch
        unsigned_consensus_vote
    in
    match signed_consensus_vote_r with
    | Error err ->
        let*! () =
          Events.(emit skipping_consensus_vote (unsigned_consensus_vote, err))
        in
        fail err
    | Ok signed_consensus_vote -> (
        match unsigned_consensus_vote.vote_kind with
        | Preattestation ->
            automaton_state.push_forge_event
              (Preattestation_ready signed_consensus_vote) ;
            return_unit
        | Attestation ->
            automaton_state.push_forge_event
              (Attestation_ready signed_consensus_vote) ;
            return_unit)
  in
  let* (authorized_consensus_votes : unsigned_consensus_vote list) =
    protect
      ~on_error:(fun err ->
        let*! () = Events.(emit error_while_authorizing_consensus_votes err) in
        return_nil)
      (fun () ->
        Baking_actions.authorized_consensus_votes
          automaton_state
          state.baking_state
          unsigned_consensus_votes)
  in
  List.iter
    (fun (unsigned_consensus_vote : unsigned_consensus_vote) ->
      let queue = get_or_create_queue state unsigned_consensus_vote.delegate in
      Delegate_signing_queue.push_task
        ~on_error:(fun _err -> Lwt.return_unit)
        (fun () ->
          (* Check and push the highwatermarks when executing the task *)
          let delegate_hwms =
            Delegate_high_watermarks.get_delegate_highwatermarks
              state.delegate_highwatermarks
              unsigned_consensus_vote.delegate
          in
          let highwatermark =
            match unsigned_consensus_vote.vote_kind with
            | Preattestation -> delegate_hwms.preattestations
            | Attestation -> delegate_hwms.attestations
          in
          if High_watermark.check highwatermark level round then (
            High_watermark.push highwatermark level round ;
            task unsigned_consensus_vote)
          else return_unit)
        queue)
    authorized_consensus_votes ;
  return_unit

module Handlers = struct
  type self = worker

  let on_launch _self _name (parameters : Types.parameters) =
    let delegate_signing_queues = Key_id.Table.create 13 in
    let delegate_highwatermarks =
      Delegate_high_watermarks.create_highwatermarks ()
    in
    Lwt.return_ok
      Types.
        {
          delegate_signing_queues;
          baking_state = parameters.baking_state;
          forge_consensus_vote_hook = parameters.forge_consensus_vote_hook;
          delegate_highwatermarks;
        }

  let on_no_request _ = Lwt.return_unit

  let on_request : type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun worker request ->
    let state = Worker.state worker in
    match request.content with
    | Forge_and_sign_block block_to_bake ->
        handle_forge_block state request.automaton_state block_to_bake ;
        Lwt.return (Ok ())
    | Forge_and_sign_preattestations {unsigned_preattestations} ->
        handle_forge_consensus_votes
          state
          request.automaton_state
          unsigned_preattestations
    | Forge_and_sign_attestations {unsigned_attestations} ->
        handle_forge_consensus_votes
          state
          request.automaton_state
          unsigned_attestations

  let on_close _worker = Lwt.return_unit

  let on_error : type resp err.
      self ->
      Tezos_base.Worker_types.request_status ->
      (resp, err) Request.t ->
      err ->
      [`Continue | `Shutdown] tzresult Lwt.t =
   fun _worker _state request errs ->
    let open Lwt_result_syntax in
    let emit_and_return_errors errs =
      (* Errors do not stop the worker but emit an entry in the log. *)
      let*! () = Events.(emit error_while_processing_forge_request errs) in
      return `Continue
    in
    match request.content with
    | Forge_and_sign_preattestations _ -> emit_and_return_errors errs
    | Forge_and_sign_attestations _ -> emit_and_return_errors errs
    | Forge_and_sign_block _ -> assert false

  type launch_error = tztrace

  let on_completion : type resp err.
      self ->
      (resp, err) Request.t ->
      resp ->
      Tezos_base.Worker_types.request_status ->
      unit Lwt.t =
   fun _ _ _ _ -> Lwt.return_unit
end

let start global_state =
  Worker.(
    launch
      (create_table Worker.Queue)
      global_state.chain_id
      {baking_state = global_state; forge_consensus_vote_hook = None}
      (module Handlers))

let push_request (worker : worker) {automaton_state; request} =
  let open Lwt_result_syntax in
  let*! accepted =
    match request with
    | Forge_and_sign_block block_to_bake ->
        Worker.Queue.push_request
          worker
          Request.
            {automaton_state; content = Forge_and_sign_block block_to_bake}
    | Forge_and_sign_preattestations {unsigned_preattestations} ->
        Worker.Queue.push_request
          worker
          Request.
            {
              automaton_state;
              content =
                Forge_and_sign_preattestations {unsigned_preattestations};
            }
    | Forge_and_sign_attestations {unsigned_attestations} ->
        Worker.Queue.push_request
          worker
          Request.
            {
              automaton_state;
              content = Forge_and_sign_attestations {unsigned_attestations};
            }
  in
  if accepted then return_unit
  else
    let*! () = Events.(emit forge_worker_unavailable) () in
    tzfail Baking_errors.Forge_worker_unavailable

module Internal_for_tests = struct
  module Delegate_signing_queue = Delegate_signing_queue
  module Types = Types

  type delegate_highwatermarks = Delegate_high_watermarks.t

  let get_or_create_queue = get_or_create_queue

  let create_test_state () =
    let delegate_signing_queues = Key_id.Table.create 13 in
    (* We use Obj.magic to create a dummy baking_state since we won't
       actually use it in the queue management tests. The tests only
       exercise get_or_create_queue which doesn't touch baking_state. *)
    let dummy_baking_state : Baking_state.global_state = Obj.magic () in
    Types.
      {
        delegate_signing_queues;
        baking_state = dummy_baking_state;
        forge_consensus_vote_hook = None;
        delegate_highwatermarks =
          Delegate_high_watermarks.create_highwatermarks ();
      }

  let queue_count state =
    Key_id.Table.length state.Types.delegate_signing_queues

  let has_queue state delegate =
    Key_id.Table.mem
      state.Types.delegate_signing_queues
      delegate.Baking_state_types.Delegate.consensus_key
        .Baking_state_types.Key.id

  let start ?forge_consensus_vote_hook global_state =
    Worker.(
      launch
        (create_table Worker.Queue)
        global_state.chain_id
        {baking_state = global_state; forge_consensus_vote_hook}
        (module Handlers))
end

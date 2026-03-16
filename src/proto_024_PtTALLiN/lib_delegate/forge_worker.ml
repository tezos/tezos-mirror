(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Baking_state
open Baking_state_types

module Events = struct
  include Baking_events.Forge_worker
  include Baking_events.Actions
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

  let base = ["forge"; "worker"]

  let pp = Chain_id.pp_short

  let equal = Chain_id.equal
end

module Request = struct
  type (_, _) t =
    (* The Empty.t type is used to ensure the fact that this case can not be
       associated to any error *)
    | Forge_and_sign_block : block_to_bake -> (unit, Empty.t) t
    | Forge_and_sign_preattestations : {
        unsigned_preattestations : unsigned_consensus_vote_batch;
      }
        -> (unit, error trace) t
    | Forge_and_sign_attestations : {
        unsigned_attestations : unsigned_consensus_vote_batch;
      }
        -> (unit, error trace) t

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
          (function
            | Forge_and_sign_preattestations h -> Some ((), h) | _ -> None)
          (fun ((), h) -> Forge_and_sign_preattestations h);
      ]

  let view (type a b) (req : (a, b) t) : view =
    match req with
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
    push_event : forge_event option -> unit;
    event_stream : forge_event Lwt_stream.t;
  }

  type parameters = {baking_state : Baking_state.global_state}
end

module Worker = Tezos_workers.Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

let get_event_stream worker = (Worker.state worker).Types.event_stream

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

let handle_forge_block (state : Types.state) (block_to_bake : block_to_bake) =
  let open Lwt_result_syntax in
  let task () =
    let* prepared_block =
      Baking_actions.prepare_block state.baking_state block_to_bake
    in
    state.push_event (Some (Block_ready prepared_block)) ;
    return_unit
  in
  let queue = get_or_create_queue state block_to_bake.delegate in
  Delegate_signing_queue.push_task
    ~on_error:(fun err ->
      let*! () =
        Events.(emit failed_to_forge_block (block_to_bake.delegate, err))
      in
      Lwt.return_unit)
    task
    queue

let handle_forge_consensus_votes (state : Types.state)
    (unsigned_consensus_votes : unsigned_consensus_vote_batch) =
  let open Lwt_result_syntax in
  let batch_branch = unsigned_consensus_votes.batch_branch in
  let task unsigned_consensus_vote =
    let*! signed_consensus_vote_r =
      Baking_actions.forge_and_sign_consensus_vote
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
            state.push_event (Some (Preattestation_ready signed_consensus_vote)) ;
            return_unit
        | Attestation ->
            state.push_event (Some (Attestation_ready signed_consensus_vote)) ;
            return_unit)
  in
  let* (authorized_consensus_votes : unsigned_consensus_vote list) =
    protect
      ~on_error:(fun err ->
        let*! () = Events.(emit error_while_authorizing_consensus_votes err) in
        return_nil)
      (fun () ->
        Baking_actions.authorized_consensus_votes
          state.baking_state
          unsigned_consensus_votes)
  in
  List.iter
    (fun (unsigned_preattestation : unsigned_consensus_vote) ->
      let queue = get_or_create_queue state unsigned_preattestation.delegate in
      Delegate_signing_queue.push_task
        ~on_error:(fun _err -> Lwt.return_unit)
        (fun () -> task unsigned_preattestation)
        queue)
    authorized_consensus_votes ;
  return_unit

module Handlers = struct
  type self = worker

  let on_launch _self _name (parameters : Types.parameters) =
    let delegate_signing_queues = Key_id.Table.create 13 in
    let event_stream, push_event = Lwt_stream.create () in
    Lwt.return_ok
      Types.
        {
          delegate_signing_queues;
          baking_state = parameters.baking_state;
          event_stream;
          push_event;
        }

  let on_no_request _ = Lwt.return_unit

  let on_request : type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun worker request ->
    let state = Worker.state worker in
    match request with
    | Forge_and_sign_block block_to_bake ->
        handle_forge_block state block_to_bake ;
        Lwt.return (Ok ())
    | Forge_and_sign_preattestations {unsigned_preattestations} ->
        handle_forge_consensus_votes state unsigned_preattestations
    | Forge_and_sign_attestations {unsigned_attestations} ->
        handle_forge_consensus_votes state unsigned_attestations

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
    match request with
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
      {baking_state = global_state}
      (module Handlers))

let push_request (worker : worker) request =
  match request with
  | Forge_and_sign_block block_to_bake ->
      Worker.Queue.push_request
        worker
        (Request.Forge_and_sign_block block_to_bake)
  | Forge_and_sign_preattestations {unsigned_preattestations} ->
      Worker.Queue.push_request
        worker
        (Request.Forge_and_sign_preattestations {unsigned_preattestations})
  | Forge_and_sign_attestations {unsigned_attestations} ->
      Worker.Queue.push_request
        worker
        (Request.Forge_and_sign_attestations {unsigned_attestations})

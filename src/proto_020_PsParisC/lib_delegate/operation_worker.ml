(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Protocol_client_context
open Protocol
open Alpha_context

module Events = struct
  include Internal_event.Simple

  let section = [Protocol.name; "baker"; "operation_worker"]

  let pp_int = Format.pp_print_int

  let loop_failed =
    declare_1
      ~section
      ~name:"loop_failed"
      ~level:Error
      ~msg:"loop failed with {trace}"
      ~pp1:Error_monad.pp_print_trace
      ("trace", Error_monad.trace_encoding)

  let ended =
    declare_1
      ~section
      ~name:"ended"
      ~level:Error
      ~msg:"ended with error {stacktrace}"
      ("stacktrace", Data_encoding.string)

  let pqc_reached =
    declare_2
      ~section
      ~name:"pqc_reached"
      ~level:Debug
      ~msg:
        "prequorum reached (voting power: {voting_power}, {preattestations} \
         preattestations)"
      ~pp1:pp_int
      ("voting_power", Data_encoding.int31)
      ~pp2:pp_int
      ("preattestations", Data_encoding.int31)

  let preattestations_received =
    declare_4
      ~section
      ~name:"preattestations_received"
      ~level:Debug
      ~msg:
        "received {count} preattestations (power: {delta_power}) (total voting \
         power: {voting_power}, {preattestations} preattestations)"
      ~pp1:pp_int
      ("count", Data_encoding.int31)
      ~pp2:pp_int
      ("delta_power", Data_encoding.int31)
      ~pp3:pp_int
      ("voting_power", Data_encoding.int31)
      ~pp4:pp_int
      ("preattestations", Data_encoding.int31)

  let qc_reached =
    declare_2
      ~section
      ~name:"qc_reached"
      ~level:Debug
      ~msg:
        "quorum reached (voting power: {voting_power}, {attestations} \
         attestations)"
      ~pp1:pp_int
      ("voting_power", Data_encoding.int31)
      ~pp2:pp_int
      ("attestations", Data_encoding.int31)

  let attestations_received =
    declare_4
      ~section
      ~name:"attestations_received"
      ~level:Debug
      ~msg:
        "received {count} attestations (power: {delta_power}) (total voting \
         power: {voting_power}, {attestations} attestations)"
      ~pp1:pp_int
      ("count", Data_encoding.int31)
      ~pp2:pp_int
      ("delta_power", Data_encoding.int31)
      ~pp3:pp_int
      ("voting_power", Data_encoding.int31)
      ~pp4:pp_int
      ("attestations", Data_encoding.int31)

  let starting_new_monitoring =
    declare_0
      ~section
      ~name:"starting_new_monitoring"
      ~level:Debug
      ~msg:"starting new monitoring"
      ()

  let end_of_stream =
    declare_0
      ~section
      ~name:"end_of_stream"
      ~level:Debug
      ~msg:"end of stream"
      ()

  (* info messages *)
  let shutting_down =
    declare_0
      ~section
      ~name:"shutting_down"
      ~level:Info
      ~msg:"shutting down operation worker"
      ()
end

type candidate = {
  hash : Block_hash.t;
  round_watched : Round.t;
  payload_hash_watched : Block_payload_hash.t;
}

let candidate_encoding =
  let open Data_encoding in
  conv
    (fun {hash; round_watched; payload_hash_watched} ->
      (hash, round_watched, payload_hash_watched))
    (fun (hash, round_watched, payload_hash_watched) ->
      {hash; round_watched; payload_hash_watched})
    (obj3
       (req "hash" Block_hash.encoding)
       (req "round_watched" Round.encoding)
       (req "payload_hash_watched" Block_payload_hash.encoding))

type event =
  | Prequorum_reached of candidate * Kind.preattestation operation list
  | Quorum_reached of candidate * Kind.attestation operation list

let compare_consensus_contents (op1 : consensus_content)
    (op2 : consensus_content) =
  Compare.or_else (Raw_level.compare op1.level op2.level) @@ fun () ->
  Compare.or_else (Round.compare op1.round op2.round) @@ fun () ->
  Compare.or_else (Slot.compare op1.slot op2.slot) @@ fun () ->
  Block_payload_hash.compare op1.block_payload_hash op2.block_payload_hash

module Preattestation_set = Set.Make (struct
  type t = Kind.preattestation operation

  let compare
      ({protocol_data = {contents = Single (Preattestation op1); _}; shell = _} :
        t)
      ({protocol_data = {contents = Single (Preattestation op2); _}; shell = _} :
        t) =
    compare_consensus_contents op1 op2
end)

module Attestation_set = Set.Make (struct
  type t = Kind.attestation operation

  let compare
      ({
         protocol_data =
           {
             contents =
               Single (Attestation {consensus_content = op1; dal_content = _});
             _;
           };
         shell = _;
       } :
        t)
      ({
         protocol_data =
           {
             contents =
               Single (Attestation {consensus_content = op2; dal_content = _});
             _;
           };
         shell = _;
       } :
        t) =
    (* We do not consider the DAL content (therefore two attestations with the
       same consensus content but different DAL content are considered equal),
       in order to correctly count the voting power. Note however that there
       should be no such operations in the mempool in the first place. *)
    compare_consensus_contents op1 op2
end)

type pqc_watched = {
  candidate_watched : candidate;
  get_slot_voting_power : slot:Slot.t -> int option;
  consensus_threshold : int;
  mutable current_voting_power : int;
  mutable preattestations_received : Preattestation_set.t;
  mutable preattestations_count : int;
}

type qc_watched = {
  candidate_watched : candidate;
  get_slot_voting_power : slot:Slot.t -> int option;
  consensus_threshold : int;
  mutable current_voting_power : int;
  mutable attestations_received : Attestation_set.t;
  mutable attestations_count : int;
}

type watch_kind = Pqc_watch of pqc_watched | Qc_watch of qc_watched

type quorum_event_stream = {
  stream : event Lwt_stream.t;
  push : event option -> unit;
}

type t = {
  mutable operation_pool : Operation_pool.pool;
  mutable canceler : Lwt_canceler.t;
  mutable proposal_watched : watch_kind option;
  qc_event_stream : quorum_event_stream;
  lock : Lwt_mutex.t;
  monitor_node_operations : bool; (* Keep on monitoring node operations *)
}

let monitor_operations (cctxt : #Protocol_client_context.full) =
  let open Lwt_result_syntax in
  let* operation_stream, stream_stopper =
    Alpha_block_services.Mempool.monitor_operations
      cctxt
      ~chain:cctxt#chain
      ~validated:true
      ~branch_delayed:true
      ~branch_refused:false
      ~refused:false
      ()
  in
  let operation_stream =
    Lwt_stream.map
      (fun ops -> List.map (fun ((_, op), _) -> op) ops)
      operation_stream
  in
  let* shell_header =
    Shell_services.Blocks.Header.shell_header
      cctxt
      ~chain:cctxt#chain
      ~block:(`Head 0)
      ()
  in
  let round =
    match Fitness.(round_from_raw shell_header.fitness) with
    | Ok r -> r
    | Error _ -> Round.zero
  in
  return ((shell_header.level, round), operation_stream, stream_stopper)

let make_initial_state ?(monitor_node_operations = true) () =
  let qc_event_stream =
    let stream, push = Lwt_stream.create () in
    {stream; push}
  in
  let canceler = Lwt_canceler.create () in
  let operation_pool = Operation_pool.empty in
  let lock = Lwt_mutex.create () in
  {
    operation_pool;
    canceler;
    proposal_watched = None;
    qc_event_stream;
    lock;
    monitor_node_operations;
  }

let is_valid_consensus_content (candidate : candidate) consensus_content =
  let {hash = _; round_watched; payload_hash_watched} = candidate in
  Round.equal consensus_content.round round_watched
  && Block_payload_hash.equal
       consensus_content.block_payload_hash
       payload_hash_watched

let cancel_monitoring state = state.proposal_watched <- None

let reset_monitoring state =
  Lwt_mutex.with_lock state.lock @@ fun () ->
  match state.proposal_watched with
  | None -> Lwt.return_unit
  | Some (Pqc_watch pqc_watched) ->
      pqc_watched.current_voting_power <- 0 ;
      pqc_watched.preattestations_count <- 0 ;
      pqc_watched.preattestations_received <- Preattestation_set.empty ;
      Lwt.return_unit
  | Some (Qc_watch qc_watched) ->
      qc_watched.current_voting_power <- 0 ;
      qc_watched.attestations_count <- 0 ;
      qc_watched.attestations_received <- Attestation_set.empty ;
      Lwt.return_unit

let update_monitoring ?(should_lock = true) state ops =
  let open Lwt_syntax in
  (if should_lock then Lwt_mutex.with_lock state.lock else fun f -> f ())
  @@ fun () ->
  (* If no block is watched, don't do anything *)
  match state.proposal_watched with
  | None -> return_unit
  | Some
      (Pqc_watch
        ({
           candidate_watched;
           get_slot_voting_power;
           consensus_threshold;
           preattestations_received;
           _;
         } as proposal_watched)) ->
      let preattestations = Operation_pool.filter_preattestations ops in
      let preattestations =
        List.filter
          (fun new_preattestation ->
            not
              (Preattestation_set.mem
                 new_preattestation
                 preattestations_received))
          preattestations
      in
      let preattestations_count, voting_power =
        List.fold_left
          (fun (count, power) (op : Kind.preattestation Operation.t) ->
            let {
              shell = _;
              protocol_data =
                {contents = Single (Preattestation consensus_content); _};
              _;
            } =
              op
            in
            if is_valid_consensus_content candidate_watched consensus_content
            then
              match get_slot_voting_power ~slot:consensus_content.slot with
              | Some op_power ->
                  proposal_watched.preattestations_received <-
                    Preattestation_set.add
                      op
                      proposal_watched.preattestations_received ;
                  (succ count, power + op_power)
              | None ->
                  (* preattestations that do not use the first slot of a
                     delegate are not added to the quorum *)
                  (count, power)
            else (count, power))
          (0, 0)
          preattestations
      in
      proposal_watched.current_voting_power <-
        proposal_watched.current_voting_power + voting_power ;
      proposal_watched.preattestations_count <-
        proposal_watched.preattestations_count + preattestations_count ;
      if proposal_watched.current_voting_power >= consensus_threshold then (
        let* () =
          Events.(
            emit
              pqc_reached
              ( proposal_watched.current_voting_power,
                proposal_watched.preattestations_count ))
        in
        state.qc_event_stream.push
          (Some
             (Prequorum_reached
                ( candidate_watched,
                  Preattestation_set.elements
                    proposal_watched.preattestations_received ))) ;
        (* Once the event has been emitted, we cancel the monitoring *)
        cancel_monitoring state ;
        return_unit)
      else
        Events.(
          emit
            preattestations_received
            ( preattestations_count,
              voting_power,
              proposal_watched.current_voting_power,
              proposal_watched.preattestations_count ))
  | Some
      (Qc_watch
        ({
           candidate_watched;
           get_slot_voting_power;
           consensus_threshold;
           attestations_received;
           _;
         } as proposal_watched)) ->
      let attestations = Operation_pool.filter_attestations ops in
      let attestations =
        List.filter
          (fun new_attestation ->
            not (Attestation_set.mem new_attestation attestations_received))
          attestations
      in
      let attestations_count, voting_power =
        List.fold_left
          (fun (count, power) (op : Kind.attestation Operation.t) ->
            let {
              shell = _;
              protocol_data =
                {contents = Single (Attestation {consensus_content; _}); _};
              _;
            } =
              op
            in
            if is_valid_consensus_content candidate_watched consensus_content
            then
              match get_slot_voting_power ~slot:consensus_content.slot with
              | Some op_power ->
                  proposal_watched.attestations_received <-
                    Attestation_set.add
                      op
                      proposal_watched.attestations_received ;
                  (succ count, power + op_power)
              | None ->
                  (* attestations that do not use the first slot of a delegate
                     are not added to the quorum *)
                  (count, power)
            else (count, power))
          (0, 0)
          attestations
      in
      proposal_watched.current_voting_power <-
        proposal_watched.current_voting_power + voting_power ;
      proposal_watched.attestations_count <-
        proposal_watched.attestations_count + attestations_count ;
      if proposal_watched.current_voting_power >= consensus_threshold then (
        let* () =
          Events.(
            emit
              qc_reached
              ( proposal_watched.current_voting_power,
                proposal_watched.attestations_count ))
        in
        state.qc_event_stream.push
          (Some
             (Quorum_reached
                ( candidate_watched,
                  Attestation_set.elements
                    proposal_watched.attestations_received ))) ;
        (* Once the event has been emitted, we cancel the monitoring *)
        cancel_monitoring state ;
        return_unit)
      else
        Events.(
          emit
            attestations_received
            ( attestations_count,
              voting_power,
              proposal_watched.current_voting_power,
              proposal_watched.attestations_count ))

let monitor_quorum state new_proposal_watched =
  Lwt_mutex.with_lock state.lock @@ fun () ->
  (* if a previous monitoring was registered, we cancel it *)
  if state.proposal_watched <> None then cancel_monitoring state ;
  state.proposal_watched <- new_proposal_watched ;
  let current_consensus_operations =
    Operation_pool.Operation_set.elements state.operation_pool.consensus
  in
  (* initialize with the currently present consensus operations *)
  update_monitoring ~should_lock:false state current_consensus_operations

let monitor_preattestation_quorum state ~consensus_threshold
    ~get_slot_voting_power candidate_watched =
  let new_proposal =
    Some
      (Pqc_watch
         {
           candidate_watched;
           get_slot_voting_power;
           consensus_threshold;
           current_voting_power = 0;
           preattestations_received = Preattestation_set.empty;
           preattestations_count = 0;
         })
  in
  monitor_quorum state new_proposal

let monitor_attestation_quorum state ~consensus_threshold ~get_slot_voting_power
    candidate_watched =
  let new_proposal =
    Some
      (Qc_watch
         {
           candidate_watched;
           get_slot_voting_power;
           consensus_threshold;
           current_voting_power = 0;
           attestations_received = Attestation_set.empty;
           attestations_count = 0;
         })
  in
  monitor_quorum state new_proposal

let shutdown_worker state =
  let open Lwt_result_syntax in
  let*! () = Events.(emit shutting_down ()) in
  Lwt_canceler.cancel state.canceler

(* Each time a new head is received, the operation_pool field of the state is
   cleaned/reset by this function. Instead of emptying it completely, we keep
   the attestations of at most 5 rounds and 1 level in the past, to be able to
   include as much attestations as possible in the next block if this baker is
   the proposer. This allows to handle the following situations:

   - The baker observes an EQC for (L, R), but a proposal arrived for (L, R+1).
   After the flush, extra attestations on top of (L, R) are 'Branch_refused',
   and are not re-sent by the node. If the baker proposes at (L+1, 1), he should
   be able to include these extra attestations. Hence the cache for old rounds.

   - The baker receives a head at (L+1, 0) on top of (L, 0), but this head
   didn't reach consensus. If the baker who proposes at (L+1, 1) observed some
   extra attestations for (L, 0) that are not included in (L+1, 0), he may want
   to add them. But these attestations become 'Outdated' in the mempool once
   (L+1, 0) is received. Hence the cache for previous level.
*)
let update_operations_pool state (head_level, head_round) =
  let attestations =
    let head_round_i32 = Round.to_int32 head_round in
    let head_level_i32 = head_level in
    Operation_pool.Operation_set.filter
      (function
        | {
            protocol_data =
              Operation_data
                {
                  contents =
                    Single
                      (Attestation {consensus_content = {round; level; _}; _});
                  _;
                };
            _;
          } ->
            let round_i32 = Round.to_int32 round in
            let level_i32 = Raw_level.to_int32 level in
            let delta_round = Int32.sub head_round_i32 round_i32 in
            let delta_level = Int32.sub head_level_i32 level_i32 in
            (* Only retain attestations that are maximum 5 rounds old and
               1 level in the last *)
            Compare.Int32.(delta_round <= 5l && delta_level <= 1l)
        | _ -> false)
      state.operation_pool.consensus
  in
  let operation_pool = {Operation_pool.empty with consensus = attestations} in
  state.operation_pool <- operation_pool

let create ?(monitor_node_operations = true)
    (cctxt : #Protocol_client_context.full) =
  let open Lwt_syntax in
  let state = make_initial_state ~monitor_node_operations () in
  (* TODO should we continue forever ? *)
  let rec worker_loop () =
    let* result = monitor_operations cctxt in
    match result with
    | Error err -> Events.(emit loop_failed err)
    | Ok (head, operation_stream, op_stream_stopper) ->
        let* () = Events.(emit starting_new_monitoring ()) in
        state.canceler <- Lwt_canceler.create () ;
        Lwt_canceler.on_cancel state.canceler (fun () ->
            op_stream_stopper () ;
            cancel_monitoring state ;
            return_unit) ;
        update_operations_pool state head ;
        let rec loop () =
          let* ops = Lwt_stream.get operation_stream in
          match ops with
          | None ->
              (* When the stream closes, it means a new head has been set,
                 we reset the monitoring and flush current operations *)
              let* () = Events.(emit end_of_stream ()) in
              op_stream_stopper () ;
              let* () = reset_monitoring state in
              worker_loop ()
          | Some ops ->
              state.operation_pool <-
                Operation_pool.add_operations state.operation_pool ops ;
              let* () = update_monitoring state ops in
              loop ()
        in
        loop ()
  in
  Lwt.dont_wait
    (fun () ->
      Lwt.finalize
        (fun () ->
          if state.monitor_node_operations then worker_loop () else return_unit)
        (fun () ->
          let* _ = shutdown_worker state in
          return_unit))
    (fun exn ->
      Events.(emit__dont_wait__use_with_care ended (Printexc.to_string exn))) ;
  return state

let retrieve_pending_operations cctxt state =
  let open Lwt_result_syntax in
  let open Protocol_client_context in
  let* pending_mempool =
    Alpha_block_services.Mempool.pending_operations
      cctxt
      ~chain:cctxt#chain
      ~validated:true
      ~branch_delayed:true
      ~branch_refused:false
      ~refused:false
      ~outdated:false
      ()
  in
  state.operation_pool <-
    Operation_pool.add_operations state.operation_pool
    @@ List.rev_map snd pending_mempool.validated ;
  state.operation_pool <-
    Operation_pool.add_operations
      state.operation_pool
      (List.rev_map
         (fun (_, (op, _)) -> op)
         (Operation_hash.Map.bindings pending_mempool.branch_delayed)) ;
  return_unit

let get_current_operations state = state.operation_pool

let get_quorum_event_stream state = state.qc_event_stream.stream

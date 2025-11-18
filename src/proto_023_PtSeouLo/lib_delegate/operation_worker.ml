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

module Profiler = (val Profiler.wrap Baking_profiler.operation_worker_profiler)

module Events = struct
  include Internal_event.Simple

  let section = [Protocol.name; "baker"; "operation_worker"]

  let pp_int = Format.pp_print_int

  let node_unreachable_crash =
    declare_0
      ~section
      ~name:"node_unreachable_crash"
      ~level:Error
      ~msg:
        "Node unreachable via the monitor_operations RPC. Unable to monitor \
         quorum. Shutting down baker..."
      ()

  let monitor_operations_retry =
    declare_1
      ~section
      ~name:"monitor_operations_retry"
      ~level:Warning
      ~msg:"{msg}"
      ~pp1:Format.pp_print_string
      ("msg", Data_encoding.string)

  let monitor_operations_stream_timeout =
    declare_1
      ~section
      ~name:"monitor_operations_stream_timeout"
      ~level:Warning
      ~msg:
        "No data received from monitor_operations RPC for {timeout} seconds. \
         Assuming the stream is stalled and refreshing it."
      ~pp1:Format.pp_print_float
      ("timeout", Data_encoding.float)

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
      ~level:Info
      ~msg:
        "prequorum reached (voting power: {voting_power}, {preattestations} \
         preattestations)"
      ~pp1:pp_int
      ("voting_power", Data_encoding.int31)
      ~pp2:pp_int
      ("preattestations", Data_encoding.int31)

  let non_relevant_operation_received =
    declare_0
      ~section
      ~name:"non_relevant_operation_received"
      ~level:Debug
      ~msg:"received a non relevant operation"
      ()

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

  let pqc_progression =
    declare_2
      ~section
      ~name:"pqc_progression"
      ~level:Info
      ~msg:
        "preattesting voting power at {quorum_progression}% of the stake with \
         {preattestations} preattestations"
      ~pp1:pp_int
      ("quorum_progression", Data_encoding.int31)
      ~pp2:pp_int
      ("preattestations", Data_encoding.int31)

  let qc_reached =
    declare_2
      ~section
      ~name:"qc_reached"
      ~level:Info
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

  let qc_progression =
    declare_2
      ~section
      ~name:"qc_progression"
      ~level:Info
      ~msg:
        "attesting voting power at {quorum_progression}% of the stake with \
         {attestations} attestations"
      ~pp1:pp_int
      ("quorum_progression", Data_encoding.int31)
      ~pp2:pp_int
      ("attestations", Data_encoding.int31)

  let starting_new_monitoring =
    declare_0
      ~section
      ~name:"starting_new_monitoring"
      ~level:Debug
      ~msg:"starting new monitoring"
      ()

  let resetting_monitoring =
    declare_0
      ~section
      ~name:"resetting_monitoring"
      ~level:Debug
      ~msg:"resetting monitoring after a mempool flush"
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
  level_watched : Int32.t;
  round_watched : Round.t;
  payload_hash_watched : Block_payload_hash.t;
  branch_watched : Block_hash.t option;
}

let candidate_encoding =
  let open Data_encoding in
  conv
    (fun {
           hash;
           level_watched;
           round_watched;
           payload_hash_watched;
           branch_watched;
         }
       ->
      (hash, level_watched, round_watched, payload_hash_watched, branch_watched))
    (fun ( hash,
           level_watched,
           round_watched,
           payload_hash_watched,
           branch_watched )
       ->
      {hash; level_watched; round_watched; payload_hash_watched; branch_watched})
    (obj5
       (req "hash" Block_hash.encoding)
       (req "level_watched" int32)
       (req "round_watched" Round.encoding)
       (req "payload_hash_watched" Block_payload_hash.encoding)
       (req "branch_watched" (option Block_hash.encoding)))

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
  mutable previous_prequorum_progression : int;
}

type qc_watched = {
  candidate_watched : candidate;
  get_slot_voting_power : slot:Slot.t -> int option;
  consensus_threshold : int;
  mutable current_voting_power : int;
  mutable attestations_received : Attestation_set.t;
  mutable attestations_count : int;
  mutable previous_quorum_progression : int;
}

(* [quorum_progression_increment] is a constant used to output an event only if
   the quorum progression has progressed of at least
   [quorum_progression_increment] since the last output *)
let quorum_progression_increment = 10

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
  committee_size : int;
}

let monitor_operations (cctxt : #Protocol_client_context.full) =
  let open Lwt_result_syntax in
  let* operation_stream, stream_stopper =
    (Alpha_block_services.Mempool.monitor_operations
       cctxt
       ~chain:cctxt#chain
       ~validated:true
       ~branch_delayed:true
       ~branch_refused:false
       ~refused:false
       () [@profiler.record_s {verbosity = Info} "monitor_operations RPC"])
  in
  let operation_stream =
    Lwt_stream.map
      (fun ops -> List.map (fun ((_, op), _) -> op) ops)
      operation_stream
  in
  let* shell_header =
    (Node_rpc.shell_header
       cctxt
       ~chain:cctxt#chain
       ~block:(`Head 0)
       () [@profiler.record_s {verbosity = Info} "shell_header RPC"])
  in
  let round =
    match Fitness.(round_from_raw shell_header.fitness) with
    | Ok r -> r
    | Error _ -> Round.zero
  in
  return ((shell_header.level, round), operation_stream, stream_stopper)

let make_initial_state ?(monitor_node_operations = true) ~constants () =
  let qc_event_stream =
    let stream, push = Lwt_stream.create () in
    {stream; push}
  in
  let committee_size =
    constants.Constants.parametric.consensus_committee_size
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
    committee_size;
  }

let is_eligible (candidate : candidate) branch consensus_content =
  let {
    hash = _;
    level_watched;
    round_watched;
    payload_hash_watched;
    branch_watched;
  } =
    candidate
  in
  Int32.equal (Raw_level.to_int32 consensus_content.level) level_watched
  && Round.equal consensus_content.round round_watched
  && Block_payload_hash.equal
       consensus_content.block_payload_hash
       payload_hash_watched
  && Option.fold
       ~none:true
       ~some:(fun branch' -> Block_hash.(branch = branch'))
       branch_watched

let cancel_monitoring state = state.proposal_watched <- None

let reset_monitoring state =
  let open Lwt_syntax in
  Lwt_mutex.with_lock state.lock @@ fun () ->
  let* () = Events.(emit resetting_monitoring ()) in
  match state.proposal_watched with
  | None -> return_unit
  | Some (Pqc_watch pqc_watched) ->
      pqc_watched.current_voting_power <- 0 ;
      pqc_watched.preattestations_count <- 0 ;
      pqc_watched.preattestations_received <- Preattestation_set.empty ;
      pqc_watched.previous_prequorum_progression <- 0 ;
      return_unit
  | Some (Qc_watch qc_watched) ->
      qc_watched.current_voting_power <- 0 ;
      qc_watched.attestations_count <- 0 ;
      qc_watched.attestations_received <- Attestation_set.empty ;
      qc_watched.previous_quorum_progression <- 0 ;
      return_unit

(** [update_pqc_monitoring ~pqc_watched ops] incorporates fresh preattestation
    found in [ops] into [pqc_watched].

    @return [(voting_power, count)], where [count] is the number of new
    preattestations added, and [voting_power] is the amount of additional voting
    power contributed by these.
*)
let update_pqc_monitoring ~pqc_watched ops =
  let {
    candidate_watched;
    get_slot_voting_power;
    current_voting_power;
    preattestations_received;
    preattestations_count;
    _;
  } =
    pqc_watched
  in
  let preattestations = Operation_pool.filter_preattestations ops in
  let fresh_preattestations =
    List.filter
      (fun op -> not (Preattestation_set.mem op preattestations_received))
      preattestations
  in
  List.iter
    (function
      | ({
           protocol_data =
             {contents = Single (Preattestation consensus_content); _};
           shell = {branch};
         } as op :
          Kind.preattestation Operation.t) -> (
          if is_eligible candidate_watched branch consensus_content then
            match get_slot_voting_power ~slot:consensus_content.slot with
            | Some op_power ->
                pqc_watched.preattestations_received <-
                  Preattestation_set.add op pqc_watched.preattestations_received ;
                pqc_watched.preattestations_count <-
                  succ pqc_watched.preattestations_count ;
                pqc_watched.current_voting_power <-
                  pqc_watched.current_voting_power + op_power
            | None ->
                (* preattestations that do not use the first slot of a
                   delegate are not added to the quorum *)
                ()))
    fresh_preattestations ;
  let additional_voting_power =
    pqc_watched.current_voting_power - current_voting_power
  in
  let additional_preattestations_count =
    pqc_watched.preattestations_count - preattestations_count
  in
  (additional_voting_power, additional_preattestations_count)

(** [update_qc_monitoring ~qc_watched ops] incorporates fresh attestation
    found in [ops] into [qc_watched].

    @return [(voting_power, count)], where [count] is the number of new
    attestations added, and [voting_power] is the amount of additional voting
    power contributed by these.
*)
let update_qc_monitoring ~qc_watched ops =
  let {
    candidate_watched;
    get_slot_voting_power;
    current_voting_power;
    attestations_received;
    attestations_count;
    _;
  } =
    qc_watched
  in
  let attestations = Operation_pool.filter_attestations ops in
  let fresh_attestations =
    List.filter
      (fun op -> not (Attestation_set.mem op attestations_received))
      attestations
  in
  List.iter
    (function
      | ({
           protocol_data =
             {contents = Single (Attestation {consensus_content; _}); _};
           shell = {branch};
         } as op :
          Kind.attestation Operation.t) -> (
          if is_eligible candidate_watched branch consensus_content then
            match get_slot_voting_power ~slot:consensus_content.slot with
            | Some op_power ->
                qc_watched.attestations_received <-
                  Attestation_set.add op qc_watched.attestations_received ;
                qc_watched.attestations_count <-
                  succ qc_watched.attestations_count ;
                qc_watched.current_voting_power <-
                  qc_watched.current_voting_power + op_power
            | None ->
                (* attestations that do not use the first slot of a delegate
                   are not added to the quorum *)
                ()))
    fresh_attestations ;
  let additional_voting_power =
    qc_watched.current_voting_power - current_voting_power
  in
  let additional_attestations_count =
    qc_watched.attestations_count - attestations_count
  in
  (additional_voting_power, additional_attestations_count)

(** [update_monitoring ?should_lock state ops] incorporates fresh
    (pre)attestations operations found in [ops] into [state.proposal_watched].
    If a (pre)quorum is reached, a new event is emitted through
    [state.quorum_event_stream].
    Does nothing if [state.proposal_watched] is None.
*)
let update_monitoring ?(should_lock = true) state ops =
  let open Lwt_syntax in
  (if should_lock then Lwt_mutex.with_lock state.lock else fun f -> f ())
  @@ fun () ->
  (* If no block is watched, don't do anything *)
  match state.proposal_watched with
  | None -> return_unit
  | Some (Pqc_watch pqc_watched) ->
      let additional_voting_power, additional_preattestations_count =
        update_pqc_monitoring ~pqc_watched ops
      in
      if pqc_watched.current_voting_power >= pqc_watched.consensus_threshold
      then (
        let* () =
          Events.(
            emit
              pqc_reached
              ( pqc_watched.current_voting_power,
                pqc_watched.preattestations_count ))
        in
        state.qc_event_stream.push
          (Some
             (Prequorum_reached
                ( pqc_watched.candidate_watched,
                  Preattestation_set.elements
                    pqc_watched.preattestations_received ))) ;
        (* Once the event has been emitted, we cancel the monitoring *)
        cancel_monitoring state ;
        return_unit)
      else
        let* () =
          let current_ratio =
            pqc_watched.current_voting_power * 100 / state.committee_size
          in
          (* We only want to output an event if the quorum progression has
             progressed of at least [quorum_progression_increment] *)
          if
            current_ratio
            > pqc_watched.previous_prequorum_progression
              + quorum_progression_increment
          then (
            pqc_watched.previous_prequorum_progression <- current_ratio ;
            Events.(
              emit
                pqc_progression
                (current_ratio, pqc_watched.preattestations_count)))
          else return_unit
        in
        if additional_preattestations_count = 0 then
          Events.(emit non_relevant_operation_received ())
        else
          Events.(
            emit
              preattestations_received
              ( additional_preattestations_count,
                additional_voting_power,
                pqc_watched.current_voting_power,
                pqc_watched.preattestations_count ))
  | Some (Qc_watch qc_watched) ->
      let additional_voting_power, additional_attestations_count =
        update_qc_monitoring ~qc_watched ops
      in
      if qc_watched.current_voting_power >= qc_watched.consensus_threshold then (
        let* () =
          Events.(
            emit
              qc_reached
              (qc_watched.current_voting_power, qc_watched.attestations_count))
        in
        state.qc_event_stream.push
          (Some
             (Quorum_reached
                ( qc_watched.candidate_watched,
                  Attestation_set.elements qc_watched.attestations_received ))) ;
        (* Once the event has been emitted, we cancel the monitoring *)
        cancel_monitoring state ;
        return_unit)
      else
        let* () =
          let current_ratio =
            qc_watched.current_voting_power * 100 / state.committee_size
          in
          (* We only want to output an event if the quorum progression has
             progressed of at least [quorum_progression_increment] *)
          if
            current_ratio
            > qc_watched.previous_quorum_progression
              + quorum_progression_increment
          then (
            qc_watched.previous_quorum_progression <- current_ratio ;
            Events.(
              emit qc_progression (current_ratio, qc_watched.attestations_count)))
          else return_unit
        in
        if additional_attestations_count = 0 then
          Events.(emit non_relevant_operation_received ())
        else
          Events.(
            emit
              attestations_received
              ( additional_attestations_count,
                additional_voting_power,
                qc_watched.current_voting_power,
                qc_watched.attestations_count ))

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
           previous_prequorum_progression = 0;
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
           previous_quorum_progression = 0;
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
let flush_operation_pool state (head_level, head_round) =
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

let run ?(monitor_node_operations = true) ~constants ~round_durations
    (cctxt : #Protocol_client_context.full) =
  let open Lwt_syntax in
  let state =
    (make_initial_state
       ~constants
       ~monitor_node_operations
       () [@profiler.record_f {verbosity = Notice} "make initial state"])
  in
  let rec worker_loop () =
    let* result =
      (* If the call to [monitor_operations] RPC fails, retry 5 times during 25
         seconds before crashing the worker . *)
      Utils.retry
        ~emit:Events.(emit monitor_operations_retry)
        ~max_delay:10.
        ~delay:1.
        ~factor:2.
        ~tries:10
        ~is_error:(function _ -> true)
        ~msg:(fun errs ->
          Format.asprintf
            "Failed to reach the node via the monitor_operations RPC@,%a"
            pp_print_trace
            errs)
        (fun () ->
          (monitor_operations
             cctxt
           [@profiler.record_s {verbosity = Notice} "monitor operations"]))
        ()
    in
    match result with
    | Error _ ->
        (* The baker failed to reach the node via the monitor_operations
           RPC after multiple retries. Because it can no longer monitor the
           consensus, it is unable to attest or bake. Rather than remain in this
           degraded state or retry indefinitely, we shut it down explicitly. *)
        let* () = Events.(emit node_unreachable_crash ()) in
        Lwt_exit.exit_and_raise (*ECONNREFUSED*) 111
    | Ok (((_, round) as head), operation_stream, op_stream_stopper) ->
        () [@profiler.stop] ;
        ()
        [@profiler.record
          {verbosity = Notice}
            (Format.sprintf
               "level : %ld, round : %s"
               (fst head)
               (Int32.to_string @@ Round.to_int32 @@ snd head))] ;
        let* () = Events.(emit starting_new_monitoring ()) in
        state.canceler <- Lwt_canceler.create () ;
        Lwt_canceler.on_cancel state.canceler (fun () ->
            op_stream_stopper
              () [@profiler.record_f {verbosity = Notice} "stream stopped"] ;
            cancel_monitoring
              state
            [@profiler.record_f {verbosity = Notice} "cancel monitoring state"] ;
            () [@profiler.stop] ;
            return_unit) ;
        flush_operation_pool
          state
          head
        [@profiler.record_f {verbosity = Notice} "update operations pool"] ;
        let stream_timeout =
          Round.round_duration round_durations (Round.succ round)
          |> Period.to_seconds |> Int64.to_float
        in
        let rec loop () =
          let* result =
            Lwt.pick
              [
                (let* ops = Lwt_stream.get operation_stream in
                 return (`Stream ops));
                (let* () = Lwt_unix.sleep stream_timeout in
                 return `Timeout);
              ]
          in
          match result with
          | `Timeout ->
              (* The monitor_operations RPC has neither produced new data nor
                 closed the stream for some time. This can occur naturally, but
                 may also indicate a stalled stream. Restarting it is
                 inexpensive and can prevent the baker from hanging
                 indefinitely. *)
              let* () =
                Events.(emit monitor_operations_stream_timeout stream_timeout)
              in
              op_stream_stopper () ;
              let* () = reset_monitoring state in
              worker_loop ()
          | `Stream None ->
              (* When the stream closes, it means a new head has been set,
                 we reset the monitoring and flush current operations *)
              let* () = Events.(emit end_of_stream ()) in
              op_stream_stopper
                () [@profiler.record_f {verbosity = Info} "stream stopped"] ;
              let* () =
                (reset_monitoring
                   state
                 [@profiler.record_s
                   {verbosity = Info} "reset monitoring state"])
              in
              () [@profiler.stop] ;
              worker_loop ()
          | `Stream (Some ops) ->
              (state.operation_pool <-
                Operation_pool.add_operations state.operation_pool ops)
              [@profiler.aggregate_f {verbosity = Info} "add operations"] ;
              let* () =
                (update_monitoring
                   state
                   ops
                 [@profiler.aggregate_f
                   {verbosity = Info} "update monitoring state"])
              in
              loop ()
        in
        (loop
           () [@profiler.record_s {verbosity = Notice} "operations processing"])
  in
  if state.monitor_node_operations then
    Lwt.dont_wait
      (fun () -> worker_loop ())
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

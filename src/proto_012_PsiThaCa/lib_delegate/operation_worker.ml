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

(* TODO:
   add events +
   running state introspection to recover/restart on failure

   Do we need a mutex ?
*)

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
        "pre-quorum reached (voting power: {voting_power}, {preendorsements} \
         preendorsements)"
      ~pp1:pp_int
      ("voting_power", Data_encoding.int31)
      ~pp2:pp_int
      ("preendorsements", Data_encoding.int31)

  let preendorsements_received =
    declare_4
      ~section
      ~name:"preendorsements_received"
      ~level:Debug
      ~msg:
        "received {count} preendorsements (power: {delta_power}) (total voting \
         power: {voting_power}, {preendorsements} preendorsements)"
      ~pp1:pp_int
      ("count", Data_encoding.int31)
      ~pp2:pp_int
      ("delta_power", Data_encoding.int31)
      ~pp3:pp_int
      ("voting_power", Data_encoding.int31)
      ~pp4:pp_int
      ("preendorsements", Data_encoding.int31)

  let qc_reached =
    declare_2
      ~section
      ~name:"qc_reached"
      ~level:Debug
      ~msg:
        "quorum reached (voting power: {voting_power}, {endorsements} \
         endorsements)"
      ~pp1:pp_int
      ("voting_power", Data_encoding.int31)
      ~pp2:pp_int
      ("endorsements", Data_encoding.int31)

  let endorsements_received =
    declare_4
      ~section
      ~name:"endorsements_received"
      ~level:Debug
      ~msg:
        "received {count} endorsements (power: {delta_power}) (total voting \
         power: {voting_power}, {endorsements} endorsements)"
      ~pp1:pp_int
      ("count", Data_encoding.int31)
      ~pp2:pp_int
      ("delta_power", Data_encoding.int31)
      ~pp3:pp_int
      ("voting_power", Data_encoding.int31)
      ~pp4:pp_int
      ("endorsements", Data_encoding.int31)

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

  let received_new_operations =
    declare_0
      ~section
      ~name:"received_new_operations"
      ~level:Debug
      ~msg:"received new operations"
      ()

  (* info messages *)
  let shutting_down =
    declare_0
      ~section
      ~name:"shutting_down"
      ~level:Info
      ~msg:"shutting down operation worker"
      ()

  let mempool_initial_additions =
    declare_1
      ~section
      ~name:"mempool_initial_additions"
      ~level:Info
      ~msg:"added {count} initial operations in baker mempool"
      ("count", Data_encoding.int31)

  let invalid_json_file =
    declare_1
      ~section
      ~name:"invalid_json_file"
      ~level:Warning
      ~msg:"{filename} is not a valid JSON file"
      ("filename", Data_encoding.string)

  let no_mempool_found_in_file =
    declare_1
      ~section
      ~name:"no_mempool_found_in_file"
      ~level:Warning
      ~msg:"no mempool found in file {filename}"
      ("filename", Data_encoding.string)

  let cannot_fetch_mempool =
    declare_1
      ~section
      ~name:"cannot_fetch_mempool"
      ~level:Error
      ~msg:"cannot fetch mempool: {errs}"
      ("errs", Error_monad.(TzTrace.encoding error_encoding))
end

module Mempool = struct
  type error +=
    | Failed_mempool_fetch of {
        path : string;
        reason : string;
        details : Data_encoding.json option;
      }

  let ops_of_mempool
      (ops : Protocol_client_context.Alpha_block_services.Mempool.t) =
    (* We only retain the applied, unprocessed and delayed operations *)
    List.rev
      (Operation_hash.Map.fold (fun _ op acc -> op :: acc) ops.unprocessed
      @@ Operation_hash.Map.fold
           (fun _ (op, _) acc -> op :: acc)
           ops.branch_delayed
      @@ List.rev_map (fun (_, op) -> op) ops.applied)

  let retrieve mempool =
    match mempool with
    | None -> Lwt.return_none
    | Some mempool -> (
        let fail reason details =
          let path =
            match mempool with
            | Baking_configuration.Mempool.Local {filename} -> filename
            | Baking_configuration.Mempool.Remote {uri; _} -> Uri.to_string uri
          in
          fail (Failed_mempool_fetch {path; reason; details})
        in
        let decode_mempool json =
          protect
            ~on_error:(fun _ ->
              fail "cannot decode the received JSON into mempool" (Some json))
            (fun () ->
              let mempool =
                Data_encoding.Json.destruct
                  Protocol_client_context.Alpha_block_services.S.Mempool
                  .encoding
                  json
              in
              return (ops_of_mempool mempool))
        in
        match mempool with
        | Baking_configuration.Mempool.Local {filename} ->
            if Sys.file_exists filename then
              Tezos_stdlib_unix.Lwt_utils_unix.Json.read_file filename
              >>= function
              | Error _ ->
                  Events.(emit invalid_json_file filename) >>= fun () ->
                  Lwt.return_none
              | Ok json -> (
                  decode_mempool json >>= function
                  | Ok mempool -> Lwt.return_some mempool
                  | Error errs ->
                      Events.(emit cannot_fetch_mempool errs) >>= fun () ->
                      Lwt.return_none)
            else
              Events.(emit no_mempool_found_in_file filename) >>= fun () ->
              Lwt.return_none
        | Baking_configuration.Mempool.Remote {uri; http_headers} -> (
            ( (with_timeout
                 (Systime_os.sleep (Time.System.Span.of_seconds_exn 5.))
                 (fun _ ->
                   Tezos_rpc_http_client_unix.RPC_client_unix
                   .generic_media_type_call
                     ?headers:http_headers
                     ~accept:[Media_type.json]
                     `GET
                     uri)
               >>=? function
               | `Json (`Ok json) -> return json
               | `Json (`Unauthorized json) -> fail "unauthorized request" json
               | `Json (`Gone json) -> fail "gone" json
               | `Json (`Error json) -> fail "error" json
               | `Json (`Not_found json) -> fail "not found" json
               | `Json (`Forbidden json) -> fail "forbidden" json
               | `Json (`Conflict json) -> fail "conflict" json
               | _ -> fail "unexpected media type" None)
            >>=? fun json -> decode_mempool json )
            >>= function
            | Ok mempool -> Lwt.return_some mempool
            | Error errs ->
                Events.(emit cannot_fetch_mempool errs) >>= fun () ->
                Lwt.return_none))
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

type voting_power = int

type event =
  | Prequorum_reached of
      candidate * voting_power * Kind.preendorsement operation list
  | Quorum_reached of candidate * voting_power * Kind.endorsement operation list

type pqc_watched = {
  candidate_watched : candidate;
  get_preendorsement_voting_power : slot:Slot.t -> int;
  consensus_threshold : int;
  mutable current_voting_power : int;
  mutable preendorsements_received : Kind.preendorsement operation list;
  mutable preendorsements_count : int;
}

type qc_watched = {
  candidate_watched : candidate;
  get_endorsement_voting_power : slot:Slot.t -> int;
  consensus_threshold : int;
  mutable current_voting_power : int;
  mutable endorsements_received : Kind.endorsement operation list;
  mutable endorsements_count : int;
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
  Alpha_block_services.Mempool.monitor_operations
    cctxt
    ~chain:cctxt#chain
    ~applied:true
    ~branch_delayed:true
    ~branch_refused:false
    ~refused:false
    ()
  >>=? fun (operation_stream, stream_stopper) ->
  let operation_stream =
    Lwt_stream.map
      (fun ops -> List.map (fun ((_, op), _) -> op) ops)
      operation_stream
  in
  Shell_services.Blocks.Header.shell_header
    cctxt
    ~chain:cctxt#chain
    ~block:(`Head 0)
    ()
  >>=? fun shell_header ->
  let round =
    match Fitness.(round_from_raw shell_header.fitness) with
    | Ok r -> r
    | Error _ -> Round.zero
  in
  return ((shell_header.level, round), operation_stream, stream_stopper)

let make_initial_state ?initial_mempool ?(monitor_node_operations = true) () =
  let qc_event_stream =
    let (stream, push) = Lwt_stream.create () in
    {stream; push}
  in
  let canceler = Lwt_canceler.create () in
  let operation_pool =
    Option.fold
      ~none:Operation_pool.empty
      ~some:(Operation_pool.add_operations Operation_pool.empty)
      initial_mempool
  in
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

let update_monitoring ?(should_lock = true) state ops =
  (if should_lock then Lwt_mutex.with_lock state.lock else fun f -> f ())
  @@ fun () ->
  (* If no block is watched, don't do anything *)
  match state.proposal_watched with
  | None -> Lwt.return_unit
  | Some
      (Pqc_watch
        ({
           candidate_watched;
           get_preendorsement_voting_power;
           consensus_threshold;
           _;
         } as proposal_watched)) ->
      let preendorsements = Operation_pool.filter_preendorsements ops in
      let (preendorsements_count, voting_power) =
        List.fold_left
          (fun (count, power) (op : Kind.preendorsement Operation.t) ->
            let {
              shell = _;
              protocol_data =
                {contents = Single (Preendorsement consensus_content); _};
              _;
            } =
              op
            in
            if is_valid_consensus_content candidate_watched consensus_content
            then (
              let op_power =
                get_preendorsement_voting_power ~slot:consensus_content.slot
              in
              proposal_watched.current_voting_power <-
                proposal_watched.current_voting_power + op_power ;
              proposal_watched.preendorsements_received <-
                op :: proposal_watched.preendorsements_received ;
              proposal_watched.preendorsements_count <-
                proposal_watched.preendorsements_count + 1 ;
              (count + 1, power + op_power))
            else (count, power))
          (0, 0)
          preendorsements
      in
      if proposal_watched.current_voting_power >= consensus_threshold then (
        Events.(
          emit
            pqc_reached
            ( proposal_watched.current_voting_power,
              proposal_watched.preendorsements_count ))
        >>= fun () ->
        state.qc_event_stream.push
          (Some
             (Prequorum_reached
                ( candidate_watched,
                  proposal_watched.current_voting_power,
                  List.rev proposal_watched.preendorsements_received ))) ;
        (* Once the event has been emitted, we cancel the monitoring *)
        cancel_monitoring state ;
        Lwt.return_unit)
      else
        Events.(
          emit
            preendorsements_received
            ( preendorsements_count,
              voting_power,
              proposal_watched.current_voting_power,
              proposal_watched.preendorsements_count ))
  | Some
      (Qc_watch
        ({
           candidate_watched;
           get_endorsement_voting_power;
           consensus_threshold;
           _;
         } as proposal_watched)) ->
      let endorsements = Operation_pool.filter_endorsements ops in
      let (endorsements_count, voting_power) =
        List.fold_left
          (fun (count, power) (op : Kind.endorsement Operation.t) ->
            let {
              shell = _;
              protocol_data =
                {contents = Single (Endorsement consensus_content); _};
              _;
            } =
              op
            in
            if is_valid_consensus_content candidate_watched consensus_content
            then (
              let op_power =
                get_endorsement_voting_power ~slot:consensus_content.slot
              in
              proposal_watched.current_voting_power <-
                proposal_watched.current_voting_power + op_power ;
              proposal_watched.endorsements_received <-
                op :: proposal_watched.endorsements_received ;
              proposal_watched.endorsements_count <-
                proposal_watched.endorsements_count + 1 ;
              (count + 1, power + op_power))
            else (count, power))
          (0, 0)
          endorsements
      in
      if proposal_watched.current_voting_power >= consensus_threshold then (
        Events.(
          emit
            qc_reached
            ( proposal_watched.current_voting_power,
              proposal_watched.endorsements_count ))
        >>= fun () ->
        state.qc_event_stream.push
          (Some
             (Quorum_reached
                ( candidate_watched,
                  proposal_watched.current_voting_power,
                  List.rev proposal_watched.endorsements_received ))) ;
        (* Once the event has been emitted, we cancel the monitoring *)
        cancel_monitoring state ;
        Lwt.return_unit)
      else
        Events.(
          emit
            endorsements_received
            ( endorsements_count,
              voting_power,
              proposal_watched.current_voting_power,
              proposal_watched.endorsements_count ))

let monitor_quorum state new_proposal_watched =
  Lwt_mutex.with_lock state.lock @@ fun () ->
  (* if a previous monitoring was registered, we cancel it *)
  if state.proposal_watched <> None then cancel_monitoring state ;
  state.proposal_watched <- new_proposal_watched ;
  let current_consensus_operations =
    Operation_pool.OpSet.elements state.operation_pool.consensus
  in
  (* initialize with the currently present consensus operations *)
  update_monitoring ~should_lock:false state current_consensus_operations

let monitor_preendorsement_quorum state ~consensus_threshold
    ~get_preendorsement_voting_power candidate_watched =
  let new_proposal =
    Some
      (Pqc_watch
         {
           candidate_watched;
           get_preendorsement_voting_power;
           consensus_threshold;
           current_voting_power = 0;
           preendorsements_received = [];
           preendorsements_count = 0;
         })
  in
  monitor_quorum state new_proposal

let monitor_endorsement_quorum state ~consensus_threshold
    ~get_endorsement_voting_power candidate_watched =
  let new_proposal =
    Some
      (Qc_watch
         {
           candidate_watched;
           get_endorsement_voting_power;
           consensus_threshold;
           current_voting_power = 0;
           endorsements_received = [];
           endorsements_count = 0;
         })
  in
  monitor_quorum state new_proposal

let shutdown_worker state =
  Events.(emit shutting_down ()) >>= fun () ->
  Lwt_canceler.cancel state.canceler

(* Each time a new head is received, the operation_pool field of the state is
   cleaned/reset by this function. Instead of emptying it completely, we keep
   the endorsements of at most 5 rounds and 1 level in the past, to be able to
   include as much endorsements as possible in the next block if this baker is
   the proposer. This allows to handle the following situations:

   - The baker observes an EQC for (L, R), but a proposal arrived for (L, R+1).
   After the flush, extra endorsements on top of (L, R) are 'Branch_refused',
   and are not re-sent by the node. If the baker proposes at (L+1, 1), he should
   be able to include these extra endorsements. Hence the cache for old rounds.

   - The baker receives a head at (L+1, 0) on top of (L, 0), but this head
   didn't reach consensus. If the baker who proposes at (L+1, 1) observed some
   extra endorsements for (L, 0) that are not included in (L+1, 0), he may want
   to add them. But these endorsements become 'Outdated' in the mempool once
   (L+1, 0) is received. Hence the cache for previous level.
*)
let may_update_operations_pool state (head_level, head_round) =
  let endorsements =
    let head_round_i32 = Round.to_int32 head_round in
    let head_level_i32 = head_level in
    Operation_pool.OpSet.filter
      (function
        | {
            protocol_data =
              Operation_data
                {contents = Single (Endorsement {round; level; _}); _};
            _;
          } ->
            let round_i32 = Round.to_int32 round in
            let level_i32 = Raw_level.to_int32 level in
            let delta_round = Int32.sub head_round_i32 round_i32 in
            let delta_level = Int32.sub head_level_i32 level_i32 in
            (* Only retain endorsements that are maximum 5 rounds old and
               1 level in the last *)
            Compare.Int32.(delta_round <= 5l && delta_level <= 1l)
        | _ -> false)
      state.operation_pool.consensus
  in
  let operation_pool =
    {Operation_pool.empty with Operation_pool.consensus = endorsements}
  in
  state.operation_pool <- operation_pool

let create ?initial_mempool ?(monitor_node_operations = true)
    (cctxt : #Protocol_client_context.full) =
  Mempool.retrieve initial_mempool >>= fun initial_mempool ->
  let state = make_initial_state ?initial_mempool ~monitor_node_operations () in
  (* TODO should we continue forever ? *)
  let rec worker_loop () =
    monitor_operations cctxt >>= function
    | Error err -> Events.(emit loop_failed err)
    | Ok (head, operation_stream, op_stream_stopper) ->
        Events.(emit starting_new_monitoring ()) >>= fun () ->
        state.canceler <- Lwt_canceler.create () ;
        Lwt_canceler.on_cancel state.canceler (fun () ->
            op_stream_stopper () ;
            cancel_monitoring state ;
            Lwt.return_unit) ;
        may_update_operations_pool state head ;
        let rec loop () =
          Lwt_stream.get operation_stream >>= function
          | None ->
              (* When the stream closes, it means a new head has been set,
                 we cancel the monitoring and flush current operations *)
              Events.(emit end_of_stream ()) >>= fun () ->
              op_stream_stopper () ;
              cancel_monitoring state ;
              worker_loop ()
          | Some ops ->
              Events.(emit received_new_operations ()) >>= fun () ->
              state.operation_pool <-
                Operation_pool.add_operations state.operation_pool ops ;
              update_monitoring state ops >>= fun () -> loop ()
        in
        loop ()
  in
  let worker_loop () =
    (match initial_mempool with
    | None -> Lwt.return_unit
    | Some ops -> Events.(emit mempool_initial_additions (List.length ops)))
    >>= fun () ->
    if state.monitor_node_operations then worker_loop () else Lwt.return_unit
  in
  Lwt.dont_wait
    (fun () ->
      Lwt.finalize
        (fun () -> worker_loop ())
        (fun () -> shutdown_worker state >>= fun _ -> Lwt.return_unit))
    (fun exn ->
      Events.(emit__dont_wait__use_with_care ended (Printexc.to_string exn))) ;
  Lwt.return state

let get_current_operations state = state.operation_pool

let get_quorum_event_stream state = state.qc_event_stream.stream

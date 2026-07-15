(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Baking_state_types
open Tezos_dal_node_services

module Profiler =
  (val Profiler.wrap Baking_profiler.dal_attestable_slots_worker_profiler)

module Events = struct
  include Internal_event.Simple

  let section = [Protocol.name; "baker"; "dal_attestable_slots_worker"]

  let consumed_backfill_stream =
    declare_1
      ~section
      ~name:"consumed_backfill_stream"
      ~level:Info
      ~msg:"Consumed backfill stream for {delegate_id}"
      ("delegate_id", Delegate_id.encoding)

  let monitor_attestable_slots_failed =
    declare_2
      ~section
      ~name:"monitor_attestable_slots_failed"
      ~level:Error
      ~msg:
        "Unable to subscribe to monitor attestable slots stream for {delegate} \
         -- {trace}"
      ("delegate", Delegate_id.encoding)
      ~pp2:Error_monad.pp_print_trace
      ("trace", Error_monad.trace_encoding)

  let stream_ended =
    declare_1
      ~section
      ~name:"dal_stream_ended"
      ~level:Notice
      ~msg:"DAL monitor stream ended for {delegate_id}"
      ("delegate_id", Delegate_id.encoding)

  let stale_stream_reconnect =
    declare_1
      ~section
      ~name:"dal_stream_stale_reconnect"
      ~level:Warning
      ~msg:
        "DAL monitor stream for {delegate_id} was silent past the heartbeat \
         threshold; forcing reconnection"
      ("delegate_id", Delegate_id.encoding)

  let consume_streams_ended =
    declare_1
      ~section
      ~name:"consume_streams_ended"
      ~level:Error
      ~msg:"consume_streams ended with error {stacktrace}"
      ("stacktrace", Data_encoding.string)

  let dal_worker_started =
    declare_2
      ~section
      ~name:"dal_worker_started"
      ~level:Info
      ~msg:
        "DAL worker started for automaton {automaton_name} with DAL endpoint \
         {dal_endpoint}"
      ("automaton_name", Data_encoding.string)
      ("dal_endpoint", Data_encoding.string)
end

module DelegateSet = Set.Make (Delegate_id)

(** A handle to a single delegate’s DAL monitoring subscription. *)
type stream_handle = {
  stream : Types.Attestable_event.t Lwt_stream.t;
  stopper : Tezos_rpc.Context.stopper;
  mutable silent_levels : int;
      (** Number of consecutive [update_streams_subscriptions] calls (i.e. new
          levels) during which no event was received on this stream. Reset to 0
          whenever an event (of any kind, including [Heartbeat]) is received, and
          incremented by the watchdog on each new level. Used to detect a dead
          stream. *)
}

(** A monitoring stream is considered dead once it has been silent for strictly
    more than this many consecutive levels. The DAL node emits one [Heartbeat]
    per level, so in steady state [silent_levels] oscillates between 0 and 1; a
    threshold of 3 leaves margin for jitter without triggering spurious
    reconnections. *)
let stale_stream_threshold = 3

type slot_attestation_status = Attestable | Trap | Unknown

type slots_by_delegate = slot_attestation_status array Delegate_id.Table.t

module Level_map =
  Aches.Vache.Map (Aches.Vache.FIFO_Precise) (Aches.Vache.Strong)
    (struct
      include Int32

      let hash = Hashtbl.hash
    end)

(** Set of levels for committee tracking. *)
module LevelSet = Set.Make (Int32)

type t = {
  automaton_name : string;
  attestation_lag : int;
  attestation_lags : int list;
  number_of_slots : int;
  streams : stream_handle Delegate_id.Table.t;
      (** Active per-delegate subscriptions. *)
  slots_cache : slots_by_delegate Level_map.t;
      (** Bounded FIFO cache of attestable slots, keyed by published levels.
          Attestable slots are represented by a list of booleans, with
          slots_cache[published_level][delegate][slot_index] = whether slot at
          [slot_index] is attestable by [delegate] for [published_level]. *)
  committee_cache : LevelSet.t Delegate_id.Table.t;
      (** Cache of committee_level where delegates are NOT in the committee. *)
  subscriptions_lock : Lwt_mutex.t;  (** Lock for streams subscriptions. *)
}

let create_delegate_table () = Delegate_id.Table.create 10

(** [get_or_create_bit_array state ~published_level ~delegate_id] returns
    the bitset for the given [~delegate_id] at the given [~published_level],
    creating empty entries (all false) if needed. *)
let get_or_create_bit_array state ~published_level ~delegate_id =
  let slots_by_delegate =
    match Level_map.find_opt state.slots_cache published_level with
    | Some slots_by_delegate -> slots_by_delegate
    | None ->
        let slots_by_delegate = create_delegate_table () in
        Level_map.replace state.slots_cache published_level slots_by_delegate ;
        slots_by_delegate
  in
  match Delegate_id.Table.find_opt slots_by_delegate delegate_id with
  | Some slots_array -> slots_array
  | None ->
      let slots_array = Array.make state.number_of_slots Unknown in
      Delegate_id.Table.replace slots_by_delegate delegate_id slots_array ;
      slots_array

(** [update_slots_cache state ~delegate_id ~slot_id ~is_trap] adds [~slot_id] to the
    cache, using the keys [published_level] = [slot_level] and [~delegate_id]. The bit
    associated to [~slot_id] and [~delegate_id] is set to the opposite of [~is_trap]. *)
let update_slots_cache state ~delegate_id ~slot_id ~is_trap =
  let Types.Slot_id.{slot_level; slot_index} = slot_id in
  let bitset =
    get_or_create_bit_array state ~published_level:slot_level ~delegate_id
  in
  bitset.(slot_index) <- (if is_trap then Trap else Attestable)

(** [update_committee_cache state ~delegate_id ~committee_level] records that
    the [~delegate_id] is NOT in the committee at the given [~committee_level]. *)
let update_committee_cache state ~delegate_id ~committee_level =
  let level_set =
    match Delegate_id.Table.find_opt state.committee_cache delegate_id with
    | Some set -> set
    | None -> LevelSet.empty
  in
  let level_set = LevelSet.add committee_level level_set in
  Delegate_id.Table.replace state.committee_cache delegate_id level_set

(** [update_cache_backfill_payload state ~delegate_id ~backfill_payload]
    merges a DAL [Backfill] event for [~delegate_id] into the in-memory cache. *)
let update_cache_backfill_payload state ~delegate_id ~backfill_payload =
  let module E = Types.Attestable_event in
  let E.{slot_ids; trap_slot_ids; no_shards_committee_levels} =
    backfill_payload
  in
  List.iter
    (fun slot_id ->
      (update_slots_cache
         state
         ~delegate_id
         ~slot_id
         ~is_trap:false
       [@profiler.record_f {verbosity = Debug} "update_slots_cache"]))
    slot_ids ;
  List.iter
    (fun slot_id ->
      (update_slots_cache
         state
         ~delegate_id
         ~slot_id
         ~is_trap:true
       [@profiler.record_f {verbosity = Debug} "update_slots_cache"]))
    trap_slot_ids ;
  List.iter
    (fun committee_level ->
      update_committee_cache state ~delegate_id ~committee_level)
    no_shards_committee_levels

(** [remove_stream_if_current state ~delegate_id stream_handle] removes the
    subscription entry for [~delegate_id] only if it still points to
    [stream_handle]. This guards against a late [None] delivered on a handle that
    the watchdog has already force-closed and replaced by a fresh subscription:
    such a stale [None] must not evict the new handle. Must be called while
    holding [state.subscriptions_lock]. *)
let remove_stream_if_current state ~delegate_id stream_handle =
  match Delegate_id.Table.find_opt state.streams delegate_id with
  | Some handle when handle == stream_handle ->
      Delegate_id.Table.remove state.streams delegate_id
  | _ -> ()

(** [consume_backfill_stream state stream_handle ~delegate_id] consumes the initial [Backfill]
    event from a freshly opened DAL monitoring stream. This function is meant to be called
    immediatelly after subscribing to the DAL node stream for a [~delegate_id]
    and before spawning the asynchronous consumer that handles live events. *)
let consume_backfill_stream state stream_handle ~delegate_id =
  let open Lwt_syntax in
  let module E = Types.Attestable_event in
  let* attestable_event_opt = Lwt_stream.get stream_handle.stream in
  match attestable_event_opt with
  | Some (E.Backfill {backfill_payload}) ->
      stream_handle.silent_levels <- 0 ;
      update_cache_backfill_payload state ~delegate_id ~backfill_payload ;
      let* () = Events.(emit consumed_backfill_stream delegate_id) in
      return_unit
  | None ->
      let* () = Events.(emit stream_ended delegate_id) in
      Lwt_mutex.with_lock state.subscriptions_lock @@ fun () ->
      remove_stream_if_current state ~delegate_id stream_handle ;
      return_unit
  | _ ->
      Lwt.fail_with
        (Format.asprintf
           "The stream must always start properly with a Backfill event for \
            delegate id: %a."
           Delegate_id.pp
           delegate_id)

(** [consume_stream state stream_handler ~delegate_id] consumes [~delegate_id]'s stream
    continuously, updating the [state] cache accordingly. *)
let rec consume_stream state stream_handle ~delegate_id =
  let open Lwt_syntax in
  let module E = Types.Attestable_event in
  let* attestable_event_opt = Lwt_stream.get stream_handle.stream in
  match attestable_event_opt with
  | None ->
      (* Stream gets closed, as we deliberately drop the subscription here
         (guarding against a late [None] on a handle the watchdog already
         replaced); the consumer will detect (e.g. via
         [update_streams_subscriptions]) the missing entry and re-subscribe with
         backfill on the next level. *)
      let* () = Events.(emit stream_ended delegate_id) in
      Lwt_mutex.with_lock state.subscriptions_lock @@ fun () ->
      remove_stream_if_current state ~delegate_id stream_handle ;
      return_unit
  | Some event -> (
      (* Any received event, [Heartbeat] included, resets the silence counter
         used by the watchdog in [update_streams_subscriptions]. *)
      stream_handle.silent_levels <- 0 ;
      match event with
      | E.Attestable_slot {slot_id} ->
          update_slots_cache
            state
            ~delegate_id
            ~slot_id
            ~is_trap:false
          [@profiler.aggregate_f {verbosity = Debug} "update_slots_cache"] ;
          consume_stream state ~delegate_id stream_handle
      | No_shards_assigned {committee_level} ->
          update_committee_cache state ~delegate_id ~committee_level ;
          consume_stream state ~delegate_id stream_handle
      | Slot_has_trap {slot_id} ->
          update_slots_cache state ~delegate_id ~slot_id ~is_trap:true ;
          consume_stream state ~delegate_id stream_handle
      | Heartbeat -> consume_stream state ~delegate_id stream_handle
      | Backfill _backfill_payload ->
          (* This case should never be reached as the [Backfill] is always the first element of the
             stream, and is to be consumed in [consume_backfill_stream]. *)
          Lwt.fail_with
            "Backfill events should always be consumed synchronously at the \
             beginning of a subscription.")

(** [subscribe_to_new_streams state dal_node_rpc_ctxt ~delegate_ids_to_add]
    opens new monitoring DAL streams for each entry in [~delegate_ids_to_add],
    and starts consuming from them, to populate the internal [state] cache. *)
let subscribe_to_new_streams state dal_node_rpc_ctxt ~delegate_ids_to_add =
  let open Lwt_syntax in
  let* new_streams =
    List.filter_map_p
      (fun delegate_id ->
        let* res =
          (Node_rpc.monitor_attestable_slots
             dal_node_rpc_ctxt
             ~delegate_id
           [@profiler.record_s
             {verbosity = Info}
               (Format.asprintf
                  "monitor_attestable_slots : %a"
                  Delegate_id.pp
                  delegate_id)])
        in
        match res with
        | Ok (stream, stopper) ->
            return_some (delegate_id, {stream; stopper; silent_levels = 0})
        | Error trace ->
            let* () =
              Events.(emit monitor_attestable_slots_failed (delegate_id, trace))
            in
            return_none)
      delegate_ids_to_add
  in
  let* () =
    Lwt_mutex.with_lock state.subscriptions_lock @@ fun () ->
    List.iter
      (fun (delegate_id, stream_handle) ->
        Delegate_id.Table.replace state.streams delegate_id stream_handle)
      new_streams ;
    return_unit
  in
  List.iter
    (fun (delegate_id, stream_handle) ->
      Lwt.dont_wait
        (fun () ->
          let* () =
            (consume_backfill_stream
               state
               ~delegate_id
               stream_handle
             [@profiler.record_s
               {verbosity = Info}
                 (Format.asprintf
                    "consume_backfill_stream : %a"
                    Delegate_id.pp
                    delegate_id)])
          in
          consume_stream state ~delegate_id stream_handle)
        (fun exn ->
          Events.(
            emit__dont_wait__use_with_care
              consume_streams_ended
              (Printexc.to_string exn))))
    new_streams ;
  return_unit

let update_streams_subscriptions state dal_node_rpc_ctxt ~delegate_ids =
  let open Lwt_syntax in
  let* stale_delegates, delegate_ids_to_add =
    Lwt_mutex.with_lock state.subscriptions_lock @@ fun () ->
    (* Watchdog: age every stream by one level and drop those that have been
       silent for too long. The DAL node emits a [Heartbeat] on every level, so a
       stream with no event for more than [stale_stream_threshold] levels is
       considered dead (e.g. the DAL node crashed and the connection is half-open
       or was reset). Dropping it here makes the [diff] below re-subscribe it in
       the same pass. The whole snapshot is computed synchronously (no [Lwt]
       await between reading and mutating [silent_levels]), so it cannot race
       with the consumer fibers that reset the counter. *)
    let stale =
      Delegate_id.Table.fold
        (fun delegate_id handle acc ->
          handle.silent_levels <- handle.silent_levels + 1 ;
          if handle.silent_levels > stale_stream_threshold then
            (delegate_id, handle) :: acc
          else acc)
        state.streams
        []
    in
    List.iter
      (fun (delegate_id, handle) ->
        (* Tear down the (possibly hung) connection; this pushes [None] to the
           consumer stream and wakes up the blocked [consume_stream] fiber. *)
        handle.stopper () ;
        Delegate_id.Table.remove state.streams delegate_id)
      stale ;
    let new_delegate_ids = DelegateSet.of_list delegate_ids in
    let current_delegate_ids =
      DelegateSet.of_seq @@ Delegate_id.Table.to_seq_keys state.streams
    in
    return
      ( List.map fst stale,
        DelegateSet.(elements (diff new_delegate_ids current_delegate_ids)) )
  in
  let* () =
    List.iter_s
      (fun delegate_id -> Events.(emit stale_stream_reconnect delegate_id))
      stale_delegates
  in
  subscribe_to_new_streams state dal_node_rpc_ctxt ~delegate_ids_to_add

let get_dal_attestable_slots state ~delegate_id ~published_level =
  let open Lwt_syntax in
  () [@profiler.stop] ;
  ()
  [@profiler.record
    {verbosity = Notice}
      (Format.sprintf "published_level : %ld" published_level)] ;
  match Level_map.find_opt state.slots_cache published_level with
  | None -> return_none
  | Some slots_by_delegate -> (
      match Delegate_id.Table.find_opt slots_by_delegate delegate_id with
      | None -> return_none
      | Some slots -> return_some @@ Array.to_list slots)

let is_not_in_committee state ~delegate_id ~committee_level =
  match Delegate_id.Table.find_opt state.committee_cache delegate_id with
  | None -> false
  | Some level_set -> LevelSet.mem committee_level level_set

let create ~automaton_name ~attestation_lag ~attestation_lags ~number_of_slots =
  {
    automaton_name;
    attestation_lag;
    attestation_lags;
    number_of_slots;
    streams = create_delegate_table ();
    slots_cache =
      (* a [2 * lag] size should be enough; we use more for safety *)
      Level_map.create (3 * attestation_lag);
    committee_cache = create_delegate_table ();
    subscriptions_lock = Lwt_mutex.create ();
  }

let shutdown_worker state =
  let open Lwt_syntax in
  let* stoppers =
    let stoppers =
      Delegate_id.Table.to_seq state.streams
      |> Seq.map (fun (_delegate_id, {stopper; _}) -> stopper)
      |> List.of_seq
    in
    Delegate_id.Table.clear state.streams ;
    Level_map.clear state.slots_cache ;
    Delegate_id.Table.clear state.committee_cache ;
    return stoppers
  in
  List.iter (fun stopper -> stopper ()) stoppers ;
  return_unit

let get_automaton_name state = state.automaton_name

let emit_dal_worker_started automaton_name dal_endpoint =
  Events.(emit dal_worker_started (automaton_name, dal_endpoint))

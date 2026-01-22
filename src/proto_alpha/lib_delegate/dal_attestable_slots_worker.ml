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

  let no_attestable_slot_at_level =
    declare_1
      ~section
      ~name:"no_attestable_slot_at_level"
      ~level:Warning
      ~msg:
        "No attestable slots found for attestation level {attestation_level} \
         in cache"
      ("attestation_level", Data_encoding.int32)

  let no_attestable_slot_at_level_for_delegate =
    declare_2
      ~section
      ~name:"no_attestable_slot_at_level_for_delegate"
      ~level:Warning
      ~msg:
        "No attestable slots found for attestation level {attestation_level} \
         and {delegate_id} in cache"
      ("attestation_level", Data_encoding.int32)
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

  let consume_streams_ended =
    declare_1
      ~section
      ~name:"consume_streams_ended"
      ~level:Error
      ~msg:"consume_streams ended with error {stacktrace}"
      ("stacktrace", Data_encoding.string)
end

module DelegateSet = Set.Make (Delegate_id)

(** A handle to a single delegate’s DAL monitoring subscription. *)
type stream_handle = {
  stream : Types.Attestable_event.t Lwt_stream.t;
  stopper : Tezos_rpc.Context.stopper;
}

type slots_by_delegate = Types.attestable_slots Delegate_id.Table.t

module Level_map =
  Aches.Vache.Map (Aches.Vache.FIFO_Precise) (Aches.Vache.Strong)
    (struct
      include Int32

      let hash = Hashtbl.hash
    end)

type t = {
  attestation_lag : int;
  attestation_lags : int list;
  number_of_slots : int;
  streams : stream_handle Delegate_id.Table.t;
      (** Active per-delegate subscriptions. *)
  cache : slots_by_delegate Level_map.t;
      (** Bounded FIFO cache of attestable slots, keyed by attestation levels. *)
  subscriptions_lock : Lwt_mutex.t;  (** Lock for streams subscriptions. *)
}

let create_delegate_table () = Delegate_id.Table.create 10

(** [get_slots_by_delegate state ~attestation_level] returns the per-delegate
    cache bucket for the given [~attestation_level]. If none exists yet, it
    creates an empty one, stores it in [state.cache], and returns it. *)
let get_slots_by_delegate state ~attestation_level =
  match Level_map.find_opt state.cache attestation_level with
  | Some slots_by_delegate -> slots_by_delegate
  | None ->
      let slots_by_delegate = create_delegate_table () in
      Level_map.replace state.cache attestation_level slots_by_delegate ;
      slots_by_delegate

(** [update_cache_with_attestable_slot state ?is_trap ~delegate_id ~slot_id] adds [~slot_id]
    to the cache, using the keys [attestation_level] = [slot_level] + [attestation_lag] - 1
    and [~delegate_id]. The bit associated to [~slot_id] and [~delegate_id] is set to the
    opposite of [?is_trap]. *)
let update_cache_with_attestable_slot ?(is_trap = false) state ~delegate_id
    ~slot_id =
  let Types.Slot_id.{slot_level; slot_index} = slot_id in
  let attestation_level =
    Int32.(pred @@ add slot_level (of_int state.attestation_lag))
  in
  let slots_by_delegate = get_slots_by_delegate state ~attestation_level in
  let value = not is_trap in
  let attestable_slots =
    match Delegate_id.Table.find_opt slots_by_delegate delegate_id with
    | Some (Types.Attestable_slots {slots; published_level}) ->
        let slots_array = Array.of_list slots in
        slots_array.(slot_index) <- value ;
        let slots = Array.to_list slots_array in
        Types.Attestable_slots {slots; published_level}
    | Some Not_in_committee ->
        (* We should never reach this point, as the delegate should not have any attestable
           slot, as they are not in the committee. *)
        Types.Not_in_committee
    | None ->
        let slots = Array.make state.number_of_slots false in
        slots.(slot_index) <- value ;
        Types.Attestable_slots
          {slots = Array.to_list slots; published_level = slot_level}
  in
  Delegate_id.Table.replace slots_by_delegate delegate_id attestable_slots

(** [update_cache_no_shards_assigned state ~delegate_id ~attestation_level] adds a
    [Not_in_committee] element into the cache, using [~delegate_id] and [~attestation_level]
    as keys. *)
let update_cache_no_shards_assigned state ~delegate_id ~attestation_level =
  let slots_by_delegate = get_slots_by_delegate state ~attestation_level in
  Delegate_id.Table.replace slots_by_delegate delegate_id Types.Not_in_committee

(** [update_cache_backfill_payload state ~delegate_id ~backfill_payload]
    merges a DAL [Backfill] event for [~delegate_id] into the in-memory cache. *)
let update_cache_backfill_payload state ~delegate_id ~backfill_payload =
  let module E = Types.Attestable_event in
  let E.{slot_ids; trap_slot_ids; no_shards_committee_levels} =
    backfill_payload
  in
  List.iter
    (fun slot_id ->
      (update_cache_with_attestable_slot
         state
         ~delegate_id
         ~slot_id
       [@profiler.record_f
         {verbosity = Debug} "update_cache_with_attestable_slot"]))
    slot_ids ;
  List.iter
    (fun slot_id ->
      (update_cache_with_attestable_slot
         state
         ~is_trap:true
         ~delegate_id
         ~slot_id
       [@profiler.record_f
         {verbosity = Debug} "update_cache_with_attestable_slot"]))
    trap_slot_ids ;
  List.iter
    (fun attestation_level ->
      update_cache_no_shards_assigned state ~delegate_id ~attestation_level)
    no_shards_committee_levels

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
      update_cache_backfill_payload state ~delegate_id ~backfill_payload ;
      let* () = Events.(emit consumed_backfill_stream delegate_id) in
      return_unit
  | None ->
      let* () = Events.(emit stream_ended delegate_id) in
      Lwt_mutex.with_lock state.subscriptions_lock @@ fun () ->
      Delegate_id.Table.remove state.streams delegate_id ;
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
      (* Stream gets closed, as we deliberately drop the subscription here; the consumer
         will detect (e.g. via [update_streams_subscriptions]) the missing entry and
         re-subscribe with backfill on the next level. *)
      let* () = Events.(emit stream_ended delegate_id) in
      Lwt_mutex.with_lock state.subscriptions_lock @@ fun () ->
      Delegate_id.Table.remove state.streams delegate_id ;
      return_unit
  | Some (E.Attestable_slot {slot_id}) ->
      update_cache_with_attestable_slot
        state
        ~delegate_id
        ~slot_id
      [@profiler.aggregate_f
        {verbosity = Debug} "update_cache_with_attestable_slot"] ;
      consume_stream state ~delegate_id stream_handle
  | Some (No_shards_assigned {committee_level}) ->
      update_cache_no_shards_assigned
        state
        ~delegate_id
        ~attestation_level:committee_level ;
      consume_stream state ~delegate_id stream_handle
  | Some (Slot_has_trap {slot_id}) ->
      (* In case of a trap, we know the slot is not attestable, so we record an explicit [false] bit. *)
      update_cache_with_attestable_slot
        state
        ~is_trap:true
        ~delegate_id
        ~slot_id ;
      consume_stream state ~delegate_id stream_handle
  | Some (Backfill _backfill_payload) ->
      (* This case should never be reached as the [Backfill] is always the first element of the
         stream, and is to be consumed in [consume_backfill_stream]. *)
      Lwt.fail_with
        "Backfill events should always be consumed synchronously at the \
         beginning of a subscription."

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
        | Ok (stream, stopper) -> return_some (delegate_id, {stream; stopper})
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
  let* delegate_ids_to_add =
    Lwt_mutex.with_lock state.subscriptions_lock @@ fun () ->
    let new_delegate_ids = DelegateSet.of_list delegate_ids in
    let current_delegate_ids =
      DelegateSet.of_seq @@ Delegate_id.Table.to_seq_keys state.streams
    in
    return
    @@ DelegateSet.(elements (diff new_delegate_ids current_delegate_ids))
  in
  subscribe_to_new_streams state dal_node_rpc_ctxt ~delegate_ids_to_add

let get_dal_attestable_slots state ~delegate_id ~attestation_level =
  let open Lwt_syntax in
  () [@profiler.stop] ;
  ()
  [@profiler.record
    {verbosity = Notice}
      (Format.sprintf "attestation_level : %ld" attestation_level)] ;
  match Level_map.find_opt state.cache attestation_level with
  | None ->
      let* () = Events.(emit no_attestable_slot_at_level attestation_level) in
      return_none
  | Some slots_by_delegate -> (
      match Delegate_id.Table.find_opt slots_by_delegate delegate_id with
      | None ->
          let* () =
            Events.(
              emit
                no_attestable_slot_at_level_for_delegate
                (attestation_level, delegate_id))
          in
          return_none
      | Some slots -> return_some slots)

let create ~attestation_lag ~attestation_lags ~number_of_slots =
  {
    attestation_lag;
    attestation_lags;
    number_of_slots;
    streams = create_delegate_table ();
    cache =
      (* a [2 * lag] size should be enough; we use more for safety *)
      Level_map.create (3 * attestation_lag);
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
    Level_map.clear state.cache ;
    return stoppers
  in
  List.iter (fun stopper -> stopper ()) stoppers ;
  return_unit

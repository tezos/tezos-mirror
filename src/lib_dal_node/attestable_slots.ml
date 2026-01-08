(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let is_attestable_slot_with_traps shards_store traps_fraction pkh
    assigned_shard_indexes slot_id =
  let open Lwt_result_syntax in
  List.for_all_es
    (fun shard_index ->
      let* {index = _; share} =
        Store.Shards.read shards_store slot_id shard_index
      in
      (* Note: here [pkh] should identify the baker using its delegate key
        (not the consensus key) *)
      let trap_res = Trap.share_is_trap pkh share ~traps_fraction in
      match trap_res with
      | Ok true ->
          let*! () =
            Event.emit_cannot_attest_slot_because_of_trap
              ~pkh
              ~published_level:slot_id.slot_level
              ~slot_index:slot_id.slot_index
              ~shard_index
          in
          return_false
      | Ok false -> return_true
      | Error _ ->
          (* assume the worst, that it is a trap *)
          let*! () =
            Event.emit_trap_check_failure
              ~published_level:slot_id.Types.Slot_id.slot_level
              ~slot_index:slot_id.slot_index
              ~shard_index
              ~delegate:pkh
          in
          return_false)
    assigned_shard_indexes

let published_just_before_migration ctxt ~published_level =
  let open Result_syntax in
  let open Node_context in
  let migration_level = get_last_migration_level ctxt in
  let* old_lag = get_attestation_lag ctxt ~level:published_level in
  let* new_lag =
    let attested_level = Int32.(add published_level old_lag) in
    get_attestation_lag ctxt ~level:attested_level
  in
  return
  @@ (old_lag > new_lag
     && Int32.sub migration_level old_lag < published_level
     && published_level <= migration_level)

let attested_just_after_migration ctxt ~attested_level =
  let open Result_syntax in
  let open Node_context in
  let migration_level = get_last_migration_level ctxt in
  let* new_lag = get_attestation_lag ctxt ~level:attested_level in
  let* old_lag =
    let published_level = Int32.(sub attested_level new_lag) in
    if published_level < 1l then
      (* This makes the condition below false; which is correct assuming there's
         no migration before level [new_lag + 1] (or so). *)
      return new_lag
    else get_attestation_lag ctxt ~level:published_level
  in
  return
    (old_lag > new_lag
    && migration_level < attested_level
    && attested_level <= Int32.add migration_level new_lag)

let check_is_attestable_slot_or_trap ~pkh ~(slot_id : Types.slot_id)
    ~last_known_parameters ~shard_indices ~shards_store
    ~number_of_assigned_shards =
  let open Lwt_result_syntax in
  if number_of_assigned_shards = 0 || slot_id.slot_level < 1l then return_none
  else
    let* number_stored_shards =
      Store.Shards.number_of_shards_available shards_store slot_id shard_indices
    in
    let all_stored = number_stored_shards = number_of_assigned_shards in
    if not all_stored then return_none
    else if not last_known_parameters.Types.incentives_enable then
      return_some `Attestable_slot
    else
      let* is_attestable_slot_with_traps =
        is_attestable_slot_with_traps
          shards_store
          last_known_parameters.traps_fraction
          pkh
          shard_indices
          slot_id
        |> Errors.to_option_tzresult
      in
      match is_attestable_slot_with_traps with
      | Some true -> return_some `Attestable_slot
      | Some false -> return_some `Trap
      | None -> return_none

(** [is_attestable_slot_or_trap ctxt ~pkh ~slot_id] decides whether [~slot_id] is
    attestable for delegate [pkh] at the time of calling, and when DAL incentives
    are enabled, whether it should be considered as a trap. *)
let is_attestable_slot_or_trap ctxt ~pkh ~(slot_id : Types.slot_id) =
  let open Lwt_result_syntax in
  let open Node_context in
  let published_level = slot_id.slot_level in
  let*? lag = get_attestation_lag ctxt ~level:published_level in
  let attested_level = Int32.(add published_level lag) in
  let attestation_level = Int32.pred attested_level in
  let*? should_drop = published_just_before_migration ctxt ~published_level in
  if should_drop then return_none
  else
    let*? last_known_parameters =
      get_proto_parameters ctxt ~level:(`Level attested_level)
    in
    let shards_store = Store.shards (get_store ctxt) in
    (* For retrieving the assigned shard indexes, we consider the committee
       at [attestation_level], because the (DAL) attestations in the blocks
       at level [attested_level] refer to the predecessor level. *)
    let* shard_indices =
      fetch_assigned_shard_indices ctxt ~pkh ~level:attestation_level
    in
    let number_of_assigned_shards = List.length shard_indices in
    check_is_attestable_slot_or_trap
      ~pkh
      ~slot_id
      ~last_known_parameters
      ~shard_indices
      ~shards_store
      ~number_of_assigned_shards

let may_notify_attestable_slot_or_trap ctxt ~(slot_id : Types.slot_id) =
  let open Lwt_result_syntax in
  let module T = Attestable_slots_watcher_table in
  let attestable_slots_watcher_table =
    Node_context.get_attestable_slots_watcher_table ctxt
  in
  let subscribers = T.elements attestable_slots_watcher_table in
  if Seq.is_empty subscribers then return_unit
  else
    let notify_slot_attestable_or_trap pkh =
      let* is_attestable_slot_or_trap =
        is_attestable_slot_or_trap ctxt ~pkh ~slot_id
      in
      (match is_attestable_slot_or_trap with
      | Some `Attestable_slot ->
          T.notify_attestable_slot attestable_slots_watcher_table pkh ~slot_id
      | Some `Trap ->
          T.notify_slot_has_trap attestable_slots_watcher_table pkh ~slot_id
      | None -> ()) ;
      return_unit
    in
    (* For each subscribed pkh, if it has assigned shards for that level,
       check if all those shards are available for [slot_id] and notify watcher,
       accordingly. *)
    Seq.iter_ep notify_slot_attestable_or_trap subscribers

let is_not_in_committee committee ~pkh =
  let assigned_shard_indices =
    match Signature.Public_key_hash.Map.find pkh committee with
    | None -> []
    | Some (indexes, _) -> indexes
  in
  List.is_empty assigned_shard_indices

let may_notify_not_in_committee ctxt committee ~attestation_level =
  let module T = Attestable_slots_watcher_table in
  let attestable_slots_watcher_table =
    Node_context.get_attestable_slots_watcher_table ctxt
  in
  let subscribers = T.elements attestable_slots_watcher_table in
  Seq.iter
    (fun pkh ->
      if is_not_in_committee committee ~pkh then
        T.notify_no_shards_assigned
          attestable_slots_watcher_table
          pkh
          ~attestation_level)
    subscribers

(** [get_backfill_payload ctxt ~pkh] computes a compact “backfill” payload for
    the freshly subscribed delegate [~pkh].

    The payload can be used to pre-populate a structure that requires information from
    the past of the creation of the stream corresponding to [~pkh] with:
      - all attestable [slot_id]'s observed in a recent window;
      - all [slot_id]'s with traps;
      - attestation levels where [pkh] has no assigned shards.

    The "backfill" window is calculated in the following way:
      - Let [L] be the current last-finalized level at subscription time;
      - `start = max (1, L - attestation_lag + 1)`, as this is the oldest level where
      slots can be published and attested in the near future;
      - `stop = L`, as this is the newest level where we did not have time to obtain
      the information about the published slots.
    
    We include [L + 1] in backfill to cover possible races between updating the
    last-finalized level and stream subscription. This keeps the
    client's cache consistent even if the first slot was published before the stream
    was fully established.

    For each level in [start .. stop + 1] (inclusively), we accumulate the attestation status
    information about each slot id. *)
let get_backfill_payload ctxt ~pkh =
  let open Lwt_result_syntax in
  let open Node_context in
  let module E = Types.Attestable_event in
  let last_finalized_level = get_last_finalized_level ctxt in
  let*? attestation_lag =
    get_attestation_lag ctxt ~level:last_finalized_level
  in
  let published_levels =
    let count =
      Int32.(to_int @@ min last_finalized_level attestation_lag) + 1
    in
    Stdlib.List.init count (fun i ->
        Int32.(sub (succ last_finalized_level) (of_int i)))
  in
  List.fold_left_es
    (fun acc published_level ->
      let attestation_level =
        Int32.(pred (add published_level attestation_lag))
      in
      let* committee =
        Node_context.fetch_committees ctxt ~level:attestation_level
      in
      if is_not_in_committee committee ~pkh then
        (* If not in committee, record and skip per-slot checks. *)
        return
          E.
            {
              acc with
              no_shards_attestation_levels =
                attestation_level :: acc.no_shards_attestation_levels;
            }
      else
        let*? proto_params =
          get_proto_parameters ctxt ~level:(`Level published_level)
        in
        let slot_indices =
          Stdlib.List.init proto_params.number_of_slots Fun.id
        in
        let store = get_store ctxt in
        let candidate_slot_indices =
          let statuses_cache = Store.statuses_cache store in
          List.filter_map
            (fun index ->
              let status_opt =
                Store.Statuses_cache.get_slot_status
                  statuses_cache
                  {slot_level = published_level; slot_index = index}
              in
              match status_opt with
              | Some `Unpublished -> None
              | _ -> Some index)
            slot_indices
        in
        if candidate_slot_indices = [] then return acc
        else
          (* Check each slot for attestability. *)
          let*? lag = get_attestation_lag ctxt ~level:published_level in
          let*? last_known_parameters =
            let attested_level = Int32.(add published_level lag) in
            get_proto_parameters ctxt ~level:(`Level attested_level)
          in
          let shards_store = Store.shards store in
          let* shard_indices =
            fetch_assigned_shard_indices ctxt ~pkh ~level:attestation_level
          in
          let number_of_assigned_shards = List.length shard_indices in
          let* new_slot_ids, new_trap_slot_ids =
            List.fold_left_es
              (fun (slot_ids_acc, trap_slot_ids_acc) slot_index ->
                let slot_id =
                  Types.Slot_id.{slot_level = published_level; slot_index}
                in
                let* is_attestable_slot_or_trap =
                  check_is_attestable_slot_or_trap
                    ~pkh
                    ~slot_id
                    ~last_known_parameters
                    ~shard_indices
                    ~shards_store
                    ~number_of_assigned_shards
                in
                match is_attestable_slot_or_trap with
                | Some `Attestable_slot ->
                    return (slot_id :: slot_ids_acc, trap_slot_ids_acc)
                | Some `Trap ->
                    return (slot_ids_acc, slot_id :: trap_slot_ids_acc)
                | None -> return (slot_ids_acc, trap_slot_ids_acc))
              ([], [])
              candidate_slot_indices
          in
          return
            E.
              {
                slot_ids = List.append new_slot_ids acc.slot_ids;
                trap_slot_ids = List.append new_trap_slot_ids acc.trap_slot_ids;
                no_shards_attestation_levels = acc.no_shards_attestation_levels;
              })
    {slot_ids = []; trap_slot_ids = []; no_shards_attestation_levels = []}
    published_levels

(** [create_new_stream_with_backfill stream backfill_payload_opt] builds a new
    event stream that delivers at most one [Backfill] event before any live items,
    then transparently forwards all events from the provided live [stream]. *)
let create_new_stream_with_backfill stream backfill_payload_opt =
  let open Lwt_syntax in
  let* backfill_stream =
    match backfill_payload_opt with
    | Ok backfill_payload ->
        let stream, push = Lwt_stream.create () in
        (* Start with a [Backfill] event *)
        push (Some (Types.Attestable_event.Backfill {backfill_payload})) ;
        push None ;
        return stream
    | Error error ->
        let* () = Event.emit_backfill_error ~error in
        return (Lwt_stream.of_list [])
  in
  return @@ Lwt_stream.append backfill_stream stream

let subscribe ctxt ~pkh =
  let open Lwt_syntax in
  let open Node_context in
  let module T = Attestable_slots_watcher_table in
  let proto_params =
    Result.to_option
    @@ get_proto_parameters ctxt ~level:(`Level (get_last_finalized_level ctxt))
  in
  let attestable_slots_watcher_table =
    get_attestable_slots_watcher_table ctxt
  in
  let watcher = T.get_or_init attestable_slots_watcher_table pkh proto_params in
  let stream, stopper = Lwt_watcher.create_stream (T.get_stream watcher) in
  let* backfill_stream =
    let* backfill_payload_opt = get_backfill_payload ctxt ~pkh in
    create_new_stream_with_backfill stream backfill_payload_opt
  in
  let next () = Lwt_stream.get backfill_stream in
  let shutdown () =
    (* stop this stream, then possibly remove the whole watcher if last subscriber *)
    Lwt_watcher.shutdown stopper ;
    T.set_num_subscribers watcher (T.get_num_subscribers watcher - 1) ;
    if T.get_num_subscribers watcher <= 0 then
      T.remove attestable_slots_watcher_table pkh
  in
  return Resto_directory.Answer.{next; shutdown}

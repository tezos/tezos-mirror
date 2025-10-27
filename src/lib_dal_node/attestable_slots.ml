(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let is_slot_attestable_with_traps shards_store traps_fraction pkh
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

let subscribe ctxt ~pkh =
  let open Node_context in
  let module T = Types.Attestable_slots_watcher_table in
  let proto_params =
    Result.to_option
    @@ get_proto_parameters ctxt ~level:(`Level (get_last_finalized_level ctxt))
  in
  let attestable_slots_watcher_table =
    get_attestable_slots_watcher_table ctxt
  in
  let watcher = T.get_or_init attestable_slots_watcher_table pkh proto_params in
  let stream, stopper = Lwt_watcher.create_stream (T.get_stream watcher) in
  let next () = Lwt_stream.get stream in
  let shutdown () =
    (* stop this stream, then possibly remove the whole watcher if last subscriber *)
    Lwt_watcher.shutdown stopper ;
    T.set_num_subscribers watcher (T.get_num_subscribers watcher - 1) ;
    if T.get_num_subscribers watcher <= 0 then
      T.remove attestable_slots_watcher_table pkh
  in
  Resto_directory.Answer.{next; shutdown}

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
    get_attestation_lag ctxt ~level:published_level
  in
  return
    (old_lag > new_lag
    && migration_level < attested_level
    && attested_level <= Int32.add migration_level new_lag)

(** [is_attestable_slot ctxt ~pkh ~slot_id] decides whether [~slot_id] is
    attestable for delegate [pkh] at the time of calling. *)
let is_attestable_slot ctxt ~pkh ~(slot_id : Types.slot_id) =
  let open Lwt_result_syntax in
  let open Node_context in
  let published_level = slot_id.slot_level in
  let*? lag = get_attestation_lag ctxt ~level:published_level in
  let attested_level = Int32.(add published_level lag) in
  let attestation_level = Int32.pred attested_level in
  let*? should_drop = published_just_before_migration ctxt ~published_level in
  if should_drop then return_false
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
    if number_of_assigned_shards = 0 || published_level < 1l then return_false
    else
      let* number_stored_shards =
        Store.Shards.number_of_shards_available
          shards_store
          slot_id
          shard_indices
      in
      let all_stored = number_stored_shards = number_of_assigned_shards in
      if not last_known_parameters.incentives_enable then return all_stored
      else if not all_stored then return_false
      else
        let* is_slot_attestable_with_traps =
          is_slot_attestable_with_traps
            shards_store
            last_known_parameters.traps_fraction
            pkh
            shard_indices
            slot_id
          |> Errors.to_option_tzresult
        in
        match is_slot_attestable_with_traps with
        | Some true -> return_true
        | _ -> return_false

let may_notify ctxt ~(slot_id : Types.slot_id) =
  let open Lwt_result_syntax in
  let module T = Types.Attestable_slots_watcher_table in
  let attestable_slots_watcher_table =
    Node_context.get_attestable_slots_watcher_table ctxt
  in
  let subscribers = T.elements attestable_slots_watcher_table in
  if Seq.is_empty subscribers then return_unit
  else
    let notify_attestable_slot pkh =
      let* is_attestable_slot = is_attestable_slot ctxt ~pkh ~slot_id in
      if is_attestable_slot then
        T.notify_attestable_slot attestable_slots_watcher_table pkh ~slot_id
      else () ;
      return_unit
    in
    (* For each subscribed pkh, if it has assigned shards for that level,
       check if all those shards are available for [slot_id] and notify watcher,
       accordingly. *)
    Seq.iter_ep notify_attestable_slot subscribers

let may_notify_not_in_committee ctxt committee ~attestation_level =
  let module T = Types.Attestable_slots_watcher_table in
  let attestable_slots_watcher_table =
    Node_context.get_attestable_slots_watcher_table ctxt
  in
  let subscribers = T.elements attestable_slots_watcher_table in
  Seq.iter
    (fun pkh ->
      let assigned_shard_indices =
        match Signature.Public_key_hash.Map.find pkh committee with
        | None -> []
        | Some (indexes, _) -> indexes
      in
      if List.is_empty assigned_shard_indices then
        T.notify_no_shards_assigned
          attestable_slots_watcher_table
          pkh
          ~attestation_level)
    subscribers

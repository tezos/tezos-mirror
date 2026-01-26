(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Profiler = (val Profiler.wrap Dal_profiler.dal_profiler)

(* This function removes from the store the given slot and its
   shards. In case of error, this function emits a warning instead
   of failing. *)
let remove_slots_and_shards ~slot_size (store : Store.t)
    (slot_id : Types.slot_id) =
  let open Lwt_syntax in
  let* () =
    let shards_store = Store.shards store in
    let* res = Store.Shards.remove shards_store slot_id in
    match res with
    | Ok () ->
        Event.emit_removed_slot_shards
          ~published_level:slot_id.slot_level
          ~slot_index:slot_id.slot_index
    | Error error ->
        Event.emit_removing_shards_failed
          ~published_level:slot_id.slot_level
          ~slot_index:slot_id.slot_index
          ~error
  in
  let* () =
    let slots_store = Store.slots store in
    let* res = Store.Slots.remove_slot slots_store ~slot_size slot_id in
    match res with
    | Ok () ->
        Event.emit_removed_slot
          ~published_level:slot_id.slot_level
          ~slot_index:slot_id.slot_index
    | Error error ->
        Event.emit_removing_slot_failed
          ~published_level:slot_id.slot_level
          ~slot_index:slot_id.slot_index
          ~error
  in
  return_unit

(* This function removes, from the Store, slot data (slots, their shards, and
   their status) for commitments published at level exactly
   {!Node_context.level_to_gc ~current_level}. It also removes skip list cells
   attested at that level. *)
let remove_old_level_stored_data proto_parameters ctxt current_level =
  let open Lwt_result_syntax in
  let store = Node_context.get_store ctxt in
  let* level_to_gc =
    Node_context.level_to_gc ctxt proto_parameters ~current_level
  in
  match level_to_gc with
  | None -> return_unit
  | Some oldest_level ->
      (* The protocol parameters to consider when cleaning are the ones at the
         time of the level we are cleaning. *)
      let*? proto_parameters =
        Node_context.get_proto_parameters ctxt ~level:(`Level oldest_level)
      in
      let current_lag = proto_parameters.attestation_lag in
      (* This function removes from the skip-list all the cells for slots
         published [lag] levels before the [oldest_level]. *)
      let clean_skip_list_cells lag =
        let published_level = Int32.(sub oldest_level (of_int lag)) in
        let*! res = Store.Skip_list_cells.remove store ~published_level in
        let*! () =
          match res with
          | Ok () -> Event.emit_removed_skip_list_cells ~level:oldest_level
          | Error error ->
              Event.emit_removing_skip_list_cells_failed
                ~level:oldest_level
                ~error
        in
        return_unit
      in
      let* () =
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/7258
           We may want to remove this check. *)
        if Node_context.supports_refutations ctxt then
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/8065
             Remove after dynamic lag is active.
             This code cleans the skip-list for the "orphan" levels which are
             guaranteed to never be attested when a protocol migration reduces
             the attestation lag. *)
          let* () =
            if oldest_level > 1l then
              let*? prev_proto_parameters =
                Node_context.get_proto_parameters
                  ctxt
                  ~level:(`Level (Int32.pred oldest_level))
              in
              let previous_lag = prev_proto_parameters.Types.attestation_lag in
              if previous_lag > current_lag then
                let rec loop lag =
                  if lag = current_lag then return_unit
                  else
                    let* () = clean_skip_list_cells lag in
                    loop (lag - 1)
                in
                loop previous_lag
              else return_unit
            else return_unit
          in
          clean_skip_list_cells current_lag
        else return_unit
      in
      let number_of_slots = proto_parameters.Types.number_of_slots in
      Lwt_result.ok
      @@ List.iter_s
           (fun slot_index ->
             let slot_id : Types.slot_id =
               {slot_level = oldest_level; slot_index}
             in
             remove_slots_and_shards
               ~slot_size:proto_parameters.cryptobox_parameters.slot_size
               store
               slot_id)
           (WithExceptions.List.init ~loc:__LOC__ number_of_slots Fun.id)

(* [attestation_lag] levels after the publication of a commitment,
   if it has not been attested it will never be so we can safely
   remove it from the store. *)
let remove_unattested_slots_and_shards ~prev_proto_parameters proto_parameters
    ctxt ~attested_level attested =
  let open Lwt_syntax in
  let number_of_slots = proto_parameters.Types.number_of_slots in
  let slot_size = proto_parameters.cryptobox_parameters.slot_size in
  let store = Node_context.get_store ctxt in
  let previous_lag = prev_proto_parameters.Types.attestation_lag in
  let current_lag = proto_parameters.attestation_lag in
  (* This function removes from the store all the slots (and their shards)
     published [lag] levels before the [attested_level] and which are not
     listed in the [attested]. *)
  let remove_slots_and_shards lag attested =
    let published_level = Int32.(sub attested_level (of_int lag)) in
    List.iter_s
      (fun slot_index ->
        if attested slot_index then return_unit
        else
          let slot_id : Types.slot_id =
            {slot_level = published_level; slot_index}
          in
          remove_slots_and_shards ~slot_size store slot_id)
      (0 -- (number_of_slots - 1))
  in
  let* () =
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/8065
       Remove after dynamic lag is active.
       This code removes all slots and shards associated to the "orphan" levels
       which are guaranteed to never be attested when a protocol migration
       reduces the attestation lag. *)
    if previous_lag > current_lag then
      let rec loop lag =
        if lag = current_lag then return_unit
        else
          let* () = remove_slots_and_shards lag (fun _ -> false) in
          loop (lag - 1)
      in
      loop previous_lag
    else return_unit
  in
  remove_slots_and_shards current_lag attested

(* Here [block_level] is the same as in [new_finalized_payload_level]. When the
   DAL node is up-to-date and the current L1 head is at level L, we call this
   function with [block_level = L - 1]. *)
let may_update_topics ctxt proto_parameters ~block_level =
  let open Lwt_result_syntax in
  (* If a slot is published in a block at some level [n], it is important to
     be in the mesh for the corresponding topics when the block is
     processed. The topic of a message with level [n] is given by committee at
     level [n + attestation_lag - 1]. To have time to connect, subscribe and
     get new peers for these (possibly new) topics, this must be done in
     advance.

     We do it [additional_levels] levels in advance, where [additional_levels]
     is computed based on an estimation on the actual time needed.

     Note that we have [block_level = n - 1].

     Note that this does not affect processing messages for levels before [n +
     attestation_lag - 1], because the node does not unsubscribe, and message
     validation does not depend on the subscribed topics. *)
  let additional_levels =
    let minimal_block_delay =
      Int64.to_int proto_parameters.Types.minimal_block_delay
    in
    Constants.time_to_join_new_topics_in_levels ~minimal_block_delay
  in
  let+ committee =
    let level =
      Int32.(
        add (of_int additional_levels)
        @@ add block_level (of_int proto_parameters.Types.attestation_lag))
    in
    Node_context.fetch_committees ctxt ~level
  in
  Profile_manager.on_new_head
    (Node_context.get_profile_ctxt ctxt)
    ~number_of_slots:proto_parameters.number_of_slots
    (Node_context.get_gs_worker ctxt)
    committee

(* Here [Plugin] and [dal_constants] should be the ones for the predecessor of
   the [attested_level]. This is because if [attested_level] is the migration
   level, then skip-list cells were build using the previous protocol's encoding
   and parameters; while the plugin and parameters at [attested_level] are the
   ones for the new protocol (given that the migration already occurred). *)
let fetch_skip_list_cells ctxt cctxt dal_constants ~attested_level
    (module Plugin : Dal_plugin.T) =
  let open Lwt_result_syntax in
  let* cells_of_level =
    let pred_published_level =
      Int32.sub
        attested_level
        (Int32.of_int (1 + dal_constants.Types.attestation_lag))
    in
    Plugin.Skip_list.cells_of_level
      ~attested_level
      cctxt
      ~dal_constants
      ~pred_publication_level_dal_constants:
        (lazy
          (Lwt.return
          @@ Node_context.get_proto_parameters
               ctxt
               ~level:(`Level pred_published_level)))
  in
  List.map
    (fun (hash, cell, slot_index, cell_attestation_lag) ->
      ( Dal_proto_types.Skip_list_hash.of_proto
          Plugin.Skip_list.hash_encoding
          hash,
        Dal_proto_types.Skip_list_cell.of_proto
          Plugin.Skip_list.cell_encoding
          cell,
        slot_index,
        cell_attestation_lag,
        Plugin.Skip_list.proto_attestation_status cell
        |> Option.map (fun s -> (s :> Types.header_status)) ))
    cells_of_level
  |> return

let store_skip_list_cells ctxt ~attested_level skip_list_cells =
  let store = Node_context.get_store ctxt in
  Store.Skip_list_cells.insert
    store
    ~attested_level
    skip_list_cells
    (fun (hash, cell, slot_index, cell_attestation_lag, _status) ->
      (hash, cell, slot_index, cell_attestation_lag))

let fetch_and_store_skip_list_cells ctxt cctxt proto_params ~attested_level
    (module Plugin : Dal_plugin.T) =
  let open Lwt_result_syntax in
  let* skip_list_cells =
    fetch_skip_list_cells
      ctxt
      cctxt
      proto_params
      ~attested_level
      (module Plugin : Dal_plugin.T)
  in
  store_skip_list_cells ctxt ~attested_level skip_list_cells

(* This functions counts, for each slot, the number of shards attested by the bakers. *)
let attested_shards_per_slot attestations slot_to_committee ~number_of_slots
    is_attested tb_slot_to_int =
  let count_per_slot = Array.make number_of_slots 0 in
  List.iter
    (fun (tb_slot, _, dal_attestation_opt) ->
      match dal_attestation_opt with
      | Some dal_attestation -> (
          match
            List.find
              (fun (s, _) -> s = tb_slot_to_int tb_slot)
              slot_to_committee
          with
          | Some (_, (_, shard_indexes)) ->
              let num_shards = List.length shard_indexes in
              for i = 0 to number_of_slots - 1 do
                if is_attested dal_attestation i then
                  count_per_slot.(i) <- count_per_slot.(i) + num_shards
              done
          | None -> ())
      | None -> ())
    attestations ;
  count_per_slot

let check_attesters_attested node_ctxt committee slot_to_committee parameters
    ~block_level attestations is_attested tb_slot_to_int =
  let open Lwt_result_syntax in
  let tracked_attesters =
    match
      Profile_manager.get_profiles @@ Node_context.get_profile_ctxt node_ctxt
    with
    | Controller profile -> Controller_profiles.attesters profile
    | _ -> Signature.Public_key_hash.Set.empty
  in
  if Signature.Public_key_hash.Set.is_empty tracked_attesters then return_unit
  else
    let attested_shards_per_slot =
      attested_shards_per_slot
        attestations
        slot_to_committee
        ~number_of_slots:parameters.Types.number_of_slots
        is_attested
        tb_slot_to_int
    in
    let threshold =
      parameters.cryptobox_parameters.number_of_shards
      / parameters.cryptobox_parameters.redundancy_factor
    in
    let are_slots_protocol_attested =
      Array.map
        (fun num_attested_shards -> num_attested_shards >= threshold)
        attested_shards_per_slot
    in
    let should_be_attested index = are_slots_protocol_attested.(index) in
    let number_of_attested_slots =
      Array.fold_left
        (fun counter is_attested ->
          if is_attested then counter + 1 else counter)
        0
        are_slots_protocol_attested
    in
    let contains_traps =
      let store = Node_context.get_store node_ctxt in
      let traps_store = Store.traps store in
      let published_level =
        Int32.(sub block_level (of_int parameters.attestation_lag))
      in
      fun pkh index ->
        if published_level <= 1l then false
        else
          Store.Traps.find traps_store ~level:published_level
          |> List.exists (fun Types.{delegate; slot_index; _} ->
                 index = slot_index
                 && Signature.Public_key_hash.equal delegate pkh)
    in
    let check_attester attester proto_tb_slot =
      let attestation_opt =
        List.find
          (fun (tb_slot, _attestation_op, _dal_attestation_opt) ->
            tb_slot_to_int tb_slot = proto_tb_slot)
          attestations
      in
      match attestation_opt with
      | None ->
          Dal_metrics.attested_slots_for_baker_per_level_ratio
            ~delegate:attester
            0. ;
          Event.emit_warn_no_attestation ~attester ~attested_level:block_level
      | Some (_tb_slot, _attestation_op, dal_attestation_opt) -> (
          match dal_attestation_opt with
          | None ->
              Dal_metrics.attested_slots_for_baker_per_level_ratio
                ~delegate:attester
                0. ;
              Event.emit_warn_attester_not_dal_attesting
                ~attester
                ~attested_level:block_level
          | Some bitset ->
              let attested, not_attested, not_attested_with_traps =
                List.fold_left
                  (fun (attested, not_attested, not_attested_with_traps)
                       index
                     ->
                    if should_be_attested index then
                      if is_attested bitset index then
                        ( index :: attested,
                          not_attested,
                          not_attested_with_traps )
                      else if
                        parameters.incentives_enable
                        && contains_traps attester index
                      then
                        ( attested,
                          not_attested,
                          index :: not_attested_with_traps )
                      else
                        ( attested,
                          index :: not_attested,
                          not_attested_with_traps )
                    else (attested, not_attested, not_attested_with_traps))
                  ([], [], [])
                  (parameters.number_of_slots - 1 --- 0)
              in
              let baker_attested_slot =
                List.length attested + List.length not_attested_with_traps
              in
              let ratio =
                try
                  float_of_int baker_attested_slot
                  /. float_of_int number_of_attested_slots
                with _ -> 1.
              in
              Dal_metrics.attested_slots_for_baker_per_level_ratio
                ~delegate:attester
                ratio ;
              let*! () =
                if attested <> [] then
                  Event.emit_attester_attested
                    ~attester
                    ~attested_level:block_level
                    ~slot_indexes:attested
                else Lwt.return_unit
              in
              let*! () =
                if not_attested <> [] then
                  Event.emit_warn_attester_did_not_attest
                    ~attester
                    ~attested_level:block_level
                    ~slot_indexes:not_attested
                else Lwt.return_unit
              in
              if not_attested_with_traps <> [] then
                Event.emit_attester_did_not_attest_because_of_traps
                  ~attester
                  ~attested_level:block_level
                  ~slot_indexes:not_attested_with_traps
              else Lwt.return_unit)
    in
    let*! () =
      Signature.Public_key_hash.Set.iter_s
        (fun attester ->
          match Signature.Public_key_hash.Map.find attester committee with
          | None -> Lwt.return_unit
          | Some (_dal_slots, tb_slot) -> check_attester attester tb_slot)
        tracked_attesters
    in
    return_unit

let process_commitments ctxt cctxt store proto_parameters block_level
    (module Plugin : Dal_plugin.T) =
  let open Lwt_result_syntax in
  let* slot_headers =
    (Plugin.get_published_slot_headers
       ~block_level
       cctxt [@profiler.record_s {verbosity = Notice} "slot_headers"])
  in
  let*! () =
    (Slot_manager.store_slot_headers
       ~number_of_slots:proto_parameters.Types.number_of_slots
       ~block_level
       slot_headers
       store [@profiler.record_s {verbosity = Notice} "store_slot_headers"])
  in
  (* If a slot header was posted to the L1 and we have the corresponding
     data, post it to gossipsub.  Note that this is done independently
     of the profile. *)
  let level_committee ~level =
    let* res =
      (Node_context.fetch_committees
         ctxt
         ~level [@profiler.record_f {verbosity = Notice} "fetch_committee"])
    in
    return (Signature.Public_key_hash.Map.map fst res)
  in
  let slot_size = proto_parameters.cryptobox_parameters.slot_size in
  let gs_worker = Node_context.get_gs_worker ctxt in
  List.iter_es
    (fun Dal_plugin.{slot_index; commitment; published_level} ->
      let slot_id : Types.slot_id =
        {slot_level = published_level; slot_index}
      in
      (Slot_manager.publish_slot_data
         ctxt
         ~level_committee
         ~slot_size
         gs_worker
         proto_parameters
         commitment
         slot_id
       [@profiler.aggregate_s {verbosity = Notice} "publish_slot_data"]))
    slot_headers

let update_slot_headers_statuses store ~attested_level skip_list_cells =
  let open Result_syntax in
  List.iter_e
    (fun (_hash, _cell, slot_index, cell_attestation_lag, status_opt) ->
      let slot_level =
        Int32.(sub attested_level (of_int cell_attestation_lag))
      in
      let slot_id = Types.Slot_id.{slot_level; slot_index} in
      match status_opt with
      | None -> return_unit
      | Some status ->
          Slot_manager.update_slot_header_status store slot_id status)
    skip_list_cells

(* The [Plugin] is that for the predecessor of the block level, it corresponds
   to [prev_proto_parameters]. *)
let process_finalized_block_data ctxt cctxt store ~prev_proto_parameters
    ~proto_parameters block_level (module Plugin : Dal_plugin.T) =
  let open Lwt_result_syntax in
  let* block_info =
    (Plugin.block_info
       cctxt
       ~block:(`Level block_level)
       ~operations_metadata:`Never
     [@profiler.record_s {verbosity = Notice} "block_info"])
  in
  let* skip_list_cells =
    (fetch_skip_list_cells
       ctxt
       cctxt
       prev_proto_parameters
       ~attested_level:block_level
       (module Plugin : Dal_plugin.T)
     [@profiler.record_s {verbosity = Notice} "fetch_skip_list_cells"])
  in
  let* () =
    if Node_context.supports_refutations ctxt then
      store_skip_list_cells
        ctxt
        ~attested_level:block_level
        skip_list_cells
      [@profiler.record_s {verbosity = Notice} "store_skip_list_cells"]
    else return_unit
  in
  let*? slot_availability =
    (Plugin.slot_availability
       block_info [@profiler.record_f {verbosity = Notice} "slot_availability"])
  in
  let*? () =
    update_slot_headers_statuses
      store
      ~attested_level:block_level
      skip_list_cells
  in
  let*! () =
    (remove_unattested_slots_and_shards
       ~prev_proto_parameters
       proto_parameters
       ctxt
       ~attested_level:block_level
       (Plugin.is_protocol_attested slot_availability)
     [@profiler.record_s
       {verbosity = Notice} "remove_unattested_slots_and_shards"])
  in
  let* attestations =
    (Plugin.get_attestations
       ~block_level
       cctxt [@profiler.record_s {verbosity = Notice} "get_attestations"])
  in
  let* committees =
    let committee_level = Int32.pred block_level in
    Node_context.fetch_committees ctxt ~level:committee_level
  in
  (* [slot_to_committee] associates a Tenderbake attestation slot index to an
     attester public key hash and its list of DAL shards indices. *)
  let slot_to_committee =
    Signature.Public_key_hash.Map.fold
      (fun pkh (dal_slots, tb_slot) l -> (tb_slot, (pkh, dal_slots)) :: l)
      committees
      []
  in
  let* () =
    (check_attesters_attested
       ctxt
       committees
       slot_to_committee
       proto_parameters
       ~block_level
       attestations
       Plugin.is_baker_attested
       Plugin.tb_slot_to_int
     [@profiler.record_s {verbosity = Notice} "check_attesters_attested"])
  in
  let* () =
    Option.fold
      ~none:return_unit
      ~some:(fun Configuration_file.{frequency; slot_index; secret_key} ->
        if Int32.rem block_level (Int32.of_int frequency) = Int32.zero then
          Slot_production.Tests.publish_slot_using_client
            ctxt
            cctxt
            block_level
            slot_index
            secret_key
            (Format.asprintf
               "%d:%d:%a"
               (Int32.to_int block_level)
               slot_index
               P2p_peer.Id.pp
               (Node_context.get_identity ctxt).peer_id)
            (module Plugin : Dal_plugin.T)
        else return_unit)
      (Node_context.get_config ctxt).publish_slots_regularly
    |> Errors.to_tzresult
  in
  (Accuser.inject_entrapment_evidences
     (module Plugin)
     attestations
     slot_to_committee
     ctxt
     cctxt
     ~attested_level:block_level
     Plugin.tb_slot_to_int
   [@profiler.record_s {verbosity = Notice} "inject_entrapment_evidences"])

let process_block ctxt cctxt l1_crawler proto_parameters finalized_shell_header
    finalized_block_hash =
  let open Lwt_result_syntax in
  let store = Node_context.get_store ctxt in
  let block_level = finalized_shell_header.Block_header.level in
  let pred_level = Int32.pred block_level in
  let*? (module Plugin), prev_proto_parameters =
    Node_context.get_plugin_and_parameters_for_level ctxt ~level:pred_level
  in
  let* () =
    if proto_parameters.Types.feature_enable then
      if Node_context.is_bootstrap_node ctxt then return_unit
      else
        process_finalized_block_data
          ctxt
          cctxt
          store
          ~prev_proto_parameters
          ~proto_parameters
          block_level
          (module Plugin)
        [@profiler.record_s {verbosity = Notice} "process_finalized_block_data"]
    else return_unit
  in
  let*? block_round = Plugin.get_round finalized_shell_header.fitness in
  Dal_metrics.layer1_block_finalized ~block_level ;
  Dal_metrics.layer1_block_finalized_round ~block_round ;
  let*! () =
    Event.emit_layer1_node_final_block
      ~hash:finalized_block_hash
      ~level:block_level
      ~round:block_round
  in
  let () =
    match Crawler.last_seen_head l1_crawler with
    | None -> assert false (* Not reachable *)
    | Some (_hash, head) ->
        L1_crawler_status.lagging_or_synced_status
          ~head_level:head.Block_header.level
          ~last_processed_level:block_level
        |> Node_context.set_l1_crawler_status ctxt
  in
  (* This should be done at the end of the function. *)
  let last_processed_level_store = Store.last_processed_level store in
  Store.Last_processed_level.save last_processed_level_store block_level

let rec try_process_block ~retries ctxt cctxt l1_crawler proto_parameters
    finalized_shell_header finalized_block_hash =
  let open Lwt_syntax in
  let* res =
    (process_block
       ctxt
       cctxt
       l1_crawler
       proto_parameters
       finalized_shell_header
       finalized_block_hash
     [@profiler.record_s {verbosity = Notice} "process_block"])
  in
  match res with
  | Error e when Layer_1.is_connection_error e && retries > 0 ->
      Node_context.set_l1_crawler_status ctxt L1_crawler_status.L1_unreachable ;
      let* () = Lwt_unix.sleep Constants.crawler_re_processing_delay in
      try_process_block
        ~retries:(retries - 1)
        ctxt
        cctxt
        l1_crawler
        proto_parameters
        finalized_shell_header
        finalized_block_hash
  | _ -> return res

(** [new_finalized_payload_level ctxt cctxt block_level] processes a new finalized
    payload level. It performs only slot (header) publication tasks: store
    published DAL slot headers and publish shards to Gossipsub. It does NOT run
    slot attestation tasks (like cleanup or skip-list updates). *)
let new_finalized_payload_level ctxt cctxt block_level =
  let open Lwt_result_syntax in
  if Int32.equal block_level 1l then return_unit
  else
    let*? (module Plugin), proto_parameters =
      Node_context.get_plugin_and_parameters_for_level ctxt ~level:block_level
    in
    let store = Node_context.get_store ctxt in
    let* () =
      if proto_parameters.Types.feature_enable then
        may_update_topics ctxt proto_parameters ~block_level
      else return_unit
    in
    (process_commitments
       ctxt
       cctxt
       store
       proto_parameters
       block_level
       (module Plugin)
     [@profiler.record_s {verbosity = Notice} "process_commitments"])

(* Process a finalized head and store *finalized* published slot headers
   indexed by block hash. A slot header is considered finalized when it is in
   a block with at least two other blocks on top of it, as guaranteed by
   Tenderbake. However, plugin registration is based on the latest L1 head not
   on the finalized block. This ensures new plugins are registered
   immediately after migration, rather than waiting for finalization. *)
let new_finalized_head ctxt cctxt l1_crawler cryptobox finalized_block_hash
    finalized_shell_header ~launch_time =
  let open Lwt_result_syntax in
  let level = finalized_shell_header.Block_header.level in
  let () = Node_context.set_last_finalized_level ctxt level in
  let* () =
    let block_level, proto_level =
      match Crawler.last_seen_head l1_crawler with
      | Some (_hash, head) -> Block_header.(head.level, head.proto_level)
      | None -> assert false (* Not reachable *)
    in
    Node_context.may_add_plugin ctxt cctxt ~proto_level ~block_level
  in

  (* If L = HEAD~2, then HEAD~1 is payload final. *)
  let finalized_payload_level = Int32.succ level in
  let* () = new_finalized_payload_level ctxt cctxt finalized_payload_level in

  let*? proto_parameters =
    Node_context.get_proto_parameters ctxt ~level:(`Level level)
  in
  (* At each potential published_level [level], we prefetch the
     committee for its corresponding committee_level (that is:
     level + attestation_lag - 1). This is in particular used by GS
     messages ids validation that cannot depend on Lwt. *)
  let* () =
    if not proto_parameters.feature_enable then return_unit
    else
      let committee_level =
        Int32.(
          pred @@ add level (of_int proto_parameters.Types.attestation_lag))
      in
      (* Note that, when the baker and DAL node are synchronized, then if
         the baker is at level L, then in this function `block_level = L - 2`.
         We therefore need the committee at `block_level + 2`. So, as long as
         `attestation_lag > 2`, there should be no issue. *)
      let* (committee : (int trace * int) Signature.Public_key_hash.Map.t) =
        Node_context.fetch_committees ctxt ~level:committee_level
      in
      Attestable_slots.may_notify_not_in_committee
        ctxt
        committee
        ~committee_level ;
      return_unit
  in
  Gossipsub.Worker.Validate_message_hook.set_batch
    (Message_validation.gossipsub_batch_validation
       ctxt
       cryptobox
       ~head_level:level
       proto_parameters) ;
  Gossipsub.Worker.Validate_message_hook.set
    (Message_validation.gossipsub_app_messages_validation
       ctxt
       cryptobox
       ~head_level:level
       proto_parameters) ;
  let* () = remove_old_level_stored_data proto_parameters ctxt level in
  let* () =
    if level = 1l then
      (* We do not process the block at level 1, as it will not
         contain DAL information, and it has no round. *)
      return_unit
    else
      try_process_block
        ~retries:Constants.crawler_retries_on_disconnection
        ctxt
        cctxt
        l1_crawler
        proto_parameters
        finalized_shell_header
        finalized_block_hash
      [@profiler.record_s {verbosity = Notice} "try_process_block"]
  in
  let end_time = Unix.gettimeofday () in
  Dal_metrics.per_level_processing_time (end_time -. launch_time) ;
  return_unit

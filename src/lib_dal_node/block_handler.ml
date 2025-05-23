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

(* This function removes from the store slot data (slots, their shards, and
   their status) for commitments published at level exactly
   {!Node_context.level_to_gc ~current_level}. It also removes skip list
   cells attested at that level. *)
let remove_old_level_stored_data proto_parameters ctxt current_level =
  let open Lwt_syntax in
  let store = Node_context.get_store ctxt in
  Node_context.level_to_gc ctxt proto_parameters ~current_level
  |> Option.iter_s (fun oldest_level ->
         let* () =
           (* TODO: https://gitlab.com/tezos/tezos/-/issues/7258
              We may want to remove this check. *)
           if Node_context.supports_refutations ctxt then
             let* res =
               Store.Skip_list_cells.remove store ~attested_level:oldest_level
             in
             match res with
             | Ok () -> Event.emit_removed_skip_list_cells ~level:oldest_level
             | Error error ->
                 Event.emit_removing_skip_list_cells_failed
                   ~level:oldest_level
                   ~error
           else return_unit
         in
         let number_of_slots = proto_parameters.Types.number_of_slots in
         let* () =
           let* res =
             Store.Statuses.remove_level_status
               ~level:oldest_level
               (Store.slot_header_statuses store)
           in
           match res with
           | Ok () -> Event.emit_removed_status ~level:oldest_level
           | Error error ->
               Event.emit_removing_status_failed ~level:oldest_level ~error
         in
         List.iter_s
           (fun slot_index ->
             let slot_id : Types.slot_id =
               {slot_level = oldest_level; slot_index}
             in
             remove_slots_and_shards
               ~slot_size:proto_parameters.cryptobox_parameters.slot_size
               store
               slot_id)
           (WithExceptions.List.init ~loc:__LOC__ number_of_slots Fun.id))

(* [attestation_lag] levels after the publication of a commitment,
   if it has not been attested it will never be so we can safely
   remove it from the store. This function removes from the store
   all the slots (and their shards) published at the given level and
   which are not listed in the [attested] list. *)
let remove_unattested_slots_and_shards proto_parameters ctxt ~published_level
    attested =
  let open Lwt_syntax in
  let number_of_slots = proto_parameters.Types.number_of_slots in
  let slot_size = proto_parameters.cryptobox_parameters.slot_size in
  let store = Node_context.get_store ctxt in
  List.iter_s
    (fun slot_index ->
      if attested slot_index then return_unit
      else
        let slot_id : Types.slot_id =
          {slot_level = published_level; slot_index}
        in
        remove_slots_and_shards ~slot_size store slot_id)
    (0 -- (number_of_slots - 1))

(* Here [block_level] is the level of the currently processed block, that is,
   when the DAL node is up-to-date, the L1 head level minus 2. *)
let may_update_topics ctxt proto_parameters ~block_level =
  let open Lwt_result_syntax in
  (* If a slot is published in a block at some level [n], it is important to
     be in the mesh for the corresponding topics when the block is
     processed. The topic of a message with level [n] is given by committee at
     level [n + attestation_lag - 1]. To have time to connect, subscribe and
     get new peers for these (possibly new) topics, this must be done in
     advance.

     We do it [attestation_lag] levels in advance. This means the node has the
     current "block time" (so 5-10 seconds) to prepare. This should be
     sufficient.

     Note that this does not affect processing messages for levels before [n +
     attestation_lag], because the node does not unsubscribe, and message
     validation does not depend on the subscribed topics. *)
  let+ committee =
    let level =
      Int32.add
        block_level
        (Int32.of_int proto_parameters.Types.attestation_lag)
    in
    Node_context.fetch_committee ctxt ~level
  in
  Profile_manager.on_new_head
    (Node_context.get_profile_ctxt ctxt)
    ~number_of_slots:proto_parameters.number_of_slots
    (Node_context.get_gs_worker ctxt)
    committee

let store_skip_list_cells ctxt cctxt dal_constants ~attested_level
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
  let cells_of_level =
    List.map
      (fun (hash, cell) ->
        ( Dal_proto_types.Skip_list_hash.of_proto
            Plugin.Skip_list.hash_encoding
            hash,
          Dal_proto_types.Skip_list_cell.of_proto
            Plugin.Skip_list.cell_encoding
            cell ))
      cells_of_level
  in
  let store = Node_context.get_store ctxt in
  Store.Skip_list_cells.insert store ~attested_level cells_of_level

(* This functions counts, for each slot, the number of shards attested by the bakers. *)
let attested_shards_per_slot attestations committee ~number_of_slots is_attested
    =
  let count_per_slot = Array.make number_of_slots 0 in
  List.iter
    (fun (_tb_slot, delegate_opt, _attestation_op, dal_attestation_opt) ->
      match (delegate_opt, dal_attestation_opt) with
      | Some delegate, Some dal_attestation -> (
          match Signature.Public_key_hash.Map.find delegate committee with
          | None -> ()
          | Some shard_indexes ->
              let num_shards = List.length shard_indexes in
              for i = 0 to number_of_slots - 1 do
                if is_attested dal_attestation i then
                  count_per_slot.(i) <- count_per_slot.(i) + num_shards
              done)
      | _ -> ())
    attestations ;
  count_per_slot

let check_attesters_attested node_ctxt parameters ~block_level attestations
    is_attested =
  let open Lwt_result_syntax in
  let attesters =
    match
      Profile_manager.get_profiles @@ Node_context.get_profile_ctxt node_ctxt
    with
    | Controller profile -> Controller_profiles.attesters profile
    | _ -> Signature.Public_key_hash.Set.empty
  in
  if Signature.Public_key_hash.Set.is_empty attesters then return_unit
  else
    let attestation_level = Int32.pred block_level in
    let* committee =
      Node_context.fetch_committee node_ctxt ~level:attestation_level
    in
    let attested_shards_per_slot =
      attested_shards_per_slot
        attestations
        committee
        ~number_of_slots:parameters.Types.number_of_slots
        is_attested
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
    let check_attester delegate =
      let attestation_opt =
        List.find
          (fun (_tb_slot, delegate_opt, _attestation_op, _dal_attestation_opt) ->
            match delegate_opt with
            | Some pkh -> Signature.Public_key_hash.equal delegate pkh
            | None -> false)
          attestations
      in
      match attestation_opt with
      | None ->
          Dal_metrics.attested_slots_for_baker_per_level_ratio ~delegate 0. ;
          Event.emit_warn_no_attestation
            ~attester:delegate
            ~attested_level:block_level
      | Some (_tb_slot, _delegate_opt, _attestation_op, dal_attestation_opt)
        -> (
          match dal_attestation_opt with
          | None ->
              Dal_metrics.attested_slots_for_baker_per_level_ratio ~delegate 0. ;
              Event.emit_warn_attester_not_dal_attesting
                ~attester:delegate
                ~attested_level:block_level
          | Some bitset ->
              let attested, not_attested, not_attested_with_traps =
                List.fold_left
                  (fun (attested, not_attested, not_attested_with_traps) index ->
                    if should_be_attested index then
                      if is_attested bitset index then
                        ( index :: attested,
                          not_attested,
                          not_attested_with_traps )
                      else if
                        parameters.incentives_enable
                        && contains_traps delegate index
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
                ~delegate
                ratio ;
              let*! () =
                if attested <> [] then
                  Event.emit_attester_attested
                    ~attester:delegate
                    ~attested_level:block_level
                    ~slot_indexes:attested
                else Lwt.return_unit
              in
              let*! () =
                if not_attested <> [] then
                  Event.emit_warn_attester_did_not_attest
                    ~attester:delegate
                    ~attested_level:block_level
                    ~slot_indexes:not_attested
                else Lwt.return_unit
              in
              if not_attested_with_traps <> [] then
                Event.emit_attester_did_not_attest_because_of_traps
                  ~attester:delegate
                  ~attested_level:block_level
                  ~slot_indexes:not_attested_with_traps
              else Lwt.return_unit)
    in
    let*! () =
      Signature.Public_key_hash.Set.iter_s
        (fun delegate ->
          if Signature.Public_key_hash.Map.mem delegate committee then
            check_attester delegate
          else Lwt.return_unit)
        attesters
    in
    return_unit

let process_block_data ctxt cctxt store proto_parameters block_level
    (module Plugin : Dal_plugin.T) =
  let open Lwt_result_syntax in
  let* block_info =
    Plugin.block_info
      cctxt
      ~block:(`Level block_level)
      ~operations_metadata:`Never
  in
  let* () =
    if Node_context.supports_refutations ctxt then
      store_skip_list_cells
        ctxt
        cctxt
        proto_parameters
        ~attested_level:block_level
        (module Plugin : Dal_plugin.T)
    else return_unit
  in
  let* slot_headers = Plugin.get_published_slot_headers ~block_level cctxt in
  let* () =
    Slot_manager.store_slot_headers
      ~number_of_slots:proto_parameters.Types.number_of_slots
      ~block_level
      slot_headers
      store
  in
  let* () =
    (* If a slot header was posted to the L1 and we have the corresponding
       data, post it to gossipsub.  Note that this is done independently
       of the profile. *)
    let level_committee = Node_context.fetch_committee ctxt in
    let slot_size = proto_parameters.cryptobox_parameters.slot_size in
    let gs_worker = Node_context.get_gs_worker ctxt in
    List.iter_es
      (fun Dal_plugin.{slot_index; commitment; published_level} ->
        let slot_id : Types.slot_id =
          {slot_level = published_level; slot_index}
        in
        Slot_manager.publish_slot_data
          ctxt
          ~level_committee
          ~slot_size
          gs_worker
          proto_parameters
          commitment
          slot_id)
      slot_headers
  in
  let*? dal_attestation = Plugin.dal_attestation block_info in
  let* () =
    Slot_manager.update_selected_slot_headers_statuses
      ~block_level
      ~attestation_lag:proto_parameters.attestation_lag
      ~number_of_slots:proto_parameters.number_of_slots
      (Plugin.is_attested dal_attestation)
      store
  in
  let*! () =
    remove_unattested_slots_and_shards
      proto_parameters
      ctxt
      ~published_level:
        Int32.(sub block_level (of_int proto_parameters.attestation_lag))
      (Plugin.is_attested dal_attestation)
  in
  let* attestations = Plugin.get_attestations ~block_level cctxt in
  let* () =
    check_attesters_attested
      ctxt
      proto_parameters
      ~block_level
      attestations
      Plugin.is_attested
  in
  Accuser.inject_entrapment_evidences
    (module Plugin)
    attestations
    ctxt
    cctxt
    ~attested_level:block_level

let process_block ctxt cctxt l1_crawler proto_parameters finalized_shell_header
    finalized_block_hash =
  let open Lwt_result_syntax in
  let store = Node_context.get_store ctxt in
  let block_level = finalized_shell_header.Block_header.level in
  let pred_level = Int32.pred block_level in
  let*? (module Plugin) =
    Node_context.get_plugin_for_level ctxt ~level:pred_level
  in
  let* () =
    if proto_parameters.Types.feature_enable then
      let* () = may_update_topics ctxt proto_parameters ~block_level in
      if Node_context.is_bootstrap_node ctxt then return_unit
      else
        process_block_data
          ctxt
          cctxt
          store
          proto_parameters
          block_level
          (module Plugin)
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
    process_block
      ctxt
      cctxt
      l1_crawler
      proto_parameters
      finalized_shell_header
      finalized_block_hash
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

(* Process a finalized head and store *finalized* published slot headers
   indexed by block hash. A slot header is considered finalized when it is in
   a block with at least two other blocks on top of it, as guaranteed by
   Tenderbake. Note that this means that shard propagation is delayed by two
   levels with respect to the publication level of the corresponding slot
   header. *)
let new_finalized_head ctxt cctxt l1_crawler cryptobox finalized_block_hash
    finalized_shell_header ~launch_time =
  let open Lwt_result_syntax in
  let level = finalized_shell_header.Block_header.level in
  let () = Node_context.set_last_finalized_level ctxt level in
  let* () =
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/7291
       We should use the head level instead. *)
    Node_context.may_add_plugin
      ctxt
      cctxt
      ~proto_level:finalized_shell_header.proto_level
      ~block_level:level
  in
  let*? proto_parameters =
    Node_context.get_proto_parameters ctxt ~level:(`Level level)
  in
  (* At each potential published_level [level], we prefetch the
     committee for its corresponding attestation_level (that is:
     level + attestation_lag - 1). This is in particular used by GS
     messages ids validation that cannot depend on Lwt. *)
  let* () =
    if not proto_parameters.feature_enable then return_unit
    else
      let attestation_level =
        Int32.(
          pred @@ add level (of_int proto_parameters.Types.attestation_lag))
      in
      let* _committee =
        Node_context.fetch_committee ctxt ~level:attestation_level
      in
      return_unit
  in
  Gossipsub.Worker.Validate_message_hook.set
    (Message_validation.gossipsub_app_messages_validation
       ctxt
       cryptobox
       level
       proto_parameters) ;
  let*! () = remove_old_level_stored_data proto_parameters ctxt level in
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
  in
  let end_time = Unix.gettimeofday () in
  Dal_metrics.per_level_processing_time (end_time -. launch_time) ;
  return_unit

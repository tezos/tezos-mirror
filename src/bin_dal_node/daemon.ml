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

let fetch_info_from_l1 cctxt ~requested_info ~rpc =
  let open Lwt_syntax in
  let delay = 0.1 in
  let delay_max = 20.0 in
  let endpoint = Uri.to_string cctxt#base in
  let rec retry delay =
    let* r = rpc cctxt in
    match r with
    | Error
        [RPC_client_errors.(Request_failed {error = Connection_failed _; _})] ->
        let delay = min delay_max (delay *. 2.) in
        let* () =
          Event.emit_retry_fetching_info_from_l1
            ~endpoint
            ~delay
            ~requested_info
            ~event_level:(if delay < delay_max then `Notice else `Warning)
        in
        let* () = Lwt_unix.sleep delay in
        retry delay
    | Error err -> return_error err
    | Ok res ->
        let* () =
          Event.emit_fetched_l1_info_success ~endpoint ~requested_info
        in
        return_ok res
  in
  retry delay

let fetch_dal_config cctxt =
  fetch_info_from_l1
    cctxt
    ~rpc:Config_services.dal_config
    ~requested_info:"DAL config"

let fetch_l1_version_info cctxt =
  fetch_info_from_l1
    cctxt
    ~rpc:Version_services.version
    ~requested_info:"version info"

(* TODO: https://gitlab.com/tezos/tezos/-/issues/7851

   Remove the legacy case from this function once the migration to V23 is complete.

   The function below infers the DAL network name based on the L1 chain name and
   the DAL node version.

   - For DAL node versions <= V22, the legacy "dal-sandbox" network name is used.
   - For versions >= V23, the new naming scheme "DAL_<L1_CHAIN_NAME>" is used.

   This ensures a smooth transition during the migration period.

   For the new naming scheme, the function queries the L1 node to retrieve its
   chain name and constructs the corresponding DAL network name by prefixing
   it with "DAL_".
*)
let infer_dal_network_name cctxt =
  let open Lwt_result_syntax in
  let version = Tezos_version_value.Current_git_info.octez_version in
  if version.major <= 22 then
    return
      (Distributed_db_version.Name.of_string
         Configuration_file.legacy_network_name) (* legacy "dal-sandbox" *)
  else
    let+ l1_version = fetch_l1_version_info cctxt in
    Format.sprintf "DAL_%s" (l1_version.network_version.chain_name :> string)
    |> Distributed_db_version.Name.of_string

let init_cryptobox config proto_parameters profile =
  let open Lwt_result_syntax in
  let prover_srs = Profile_manager.is_prover_profile profile in
  let* () =
    if prover_srs then
      let find_srs_files () = Tezos_base.Dal_srs.find_trusted_setup_files () in
      Cryptobox.init_prover_dal
        ~find_srs_files
        ~fetch_trusted_setup:config.Configuration_file.fetch_trusted_setup
        ()
    else return_unit
  in
  match Cryptobox.make proto_parameters.Types.cryptobox_parameters with
  | Ok cryptobox ->
      if prover_srs then
        match Cryptobox.precompute_shards_proofs cryptobox with
        | Ok precomputation -> return (cryptobox, Some precomputation)
        | Error (`Invalid_degree_strictly_less_than_expected {given; expected})
          ->
            fail
              [
                Errors.Cryptobox_initialisation_failed
                  (Printf.sprintf
                     "Cryptobox.precompute_shards_proofs: SRS size (= %d) \
                      smaller than expected (= %d)"
                     given
                     expected);
              ]
      else return (cryptobox, None)
  | Error (`Fail msg) -> fail [Errors.Cryptobox_initialisation_failed msg]

module Handler = struct
  (* [gossipsub_app_message_payload_validation cryptobox message message_id]
     allows checking whether the given [message] identified by [message_id] is
     valid with the current [cryptobox] parameters. The validity check is done
     by verifying that the shard in the message effectively belongs to the
     commitment given by [message_id]. *)
  let gossipsub_app_message_payload_validation cryptobox message_id message =
    let Types.Message.{share; shard_proof} = message in
    let Types.Message_id.{commitment; shard_index; _} = message_id in
    let shard = Cryptobox.{share; index = shard_index} in
    let res =
      Dal_metrics.sample_time
        ~sampling_frequency:Constants.shards_verification_sampling_frequency
        ~metric_updater:Dal_metrics.update_shards_verification_time
        ~to_sample:(fun () ->
          Cryptobox.verify_shard cryptobox commitment shard shard_proof)
    in
    match res with
    | Ok () -> `Valid
    | Error err ->
        let validation_error =
          match err with
          | `Invalid_degree_strictly_less_than_expected {given; expected} ->
              Format.sprintf
                "Invalid_degree_strictly_less_than_expected. Given: %d, \
                 expected: %d"
                given
                expected
          | `Invalid_shard -> "Invalid_shard"
          | `Shard_index_out_of_range s ->
              Format.sprintf "Shard_index_out_of_range(%s)" s
          | `Shard_length_mismatch -> "Shard_length_mismatch"
          | `Prover_SRS_not_loaded -> "Prover_SRS_not_loaded"
        in
        Event.emit_dont_wait__message_validation_error
          ~message_id
          ~validation_error ;
        `Invalid
    | exception exn ->
        (* Don't crash if crypto raised an exception. *)
        let validation_error = Printexc.to_string exn in
        Event.emit_dont_wait__message_validation_error
          ~message_id
          ~validation_error ;
        `Invalid

  let is_bootstrap_node ctxt =
    Node_context.get_profile_ctxt ctxt |> Profile_manager.is_bootstrap_profile

  let gossipsub_message_id_commitment_validation ctxt proto_parameters
      message_id =
    let store = Node_context.get_store ctxt in
    let slot_index = message_id.Types.Message_id.slot_index in
    match
      Store.Slot_id_cache.find_opt
        (Store.finalized_commitments store)
        Types.Slot_id.
          {slot_level = message_id.Types.Message_id.level; slot_index}
    with
    | Some commitment ->
        if
          Cryptobox.Commitment.equal
            commitment
            message_id.Types.Message_id.commitment
        then `Valid
        else `Invalid
    | None ->
        if
          slot_index >= 0 && slot_index < proto_parameters.Types.number_of_slots
        then
          (* We know the message is not [Outdated], because this has already
             been checked in {!gossipsub_app_messages_validation}. *)
          `Unknown
        else `Invalid

  let gossipsub_message_id_topic_validation ctxt proto_parameters message_id =
    let attestation_level =
      Int32.(
        pred
        @@ add
             message_id.Types.Message_id.level
             (of_int proto_parameters.Types.attestation_lag))
    in
    let shard_indices_opt =
      Node_context.get_fetched_assigned_shard_indices
        ctxt
        ~pkh:message_id.Types.Message_id.pkh
        ~level:attestation_level
    in
    match shard_indices_opt with
    | None ->
        (* If DAL committees of [attestation_level] are fetched each time the
           corresponding published/finalized_level is processed, this should not
           happen. *)
        `Unknown
    | Some shard_indices ->
        if
          List.mem
            ~equal:( = )
            message_id.Types.Message_id.shard_index
            shard_indices
        then `Valid
        else `Invalid

  let gossipsub_message_id_validation ctxt proto_parameters message_id =
    match
      gossipsub_message_id_commitment_validation
        ctxt
        proto_parameters
        message_id
    with
    | `Valid ->
        gossipsub_message_id_topic_validation ctxt proto_parameters message_id
    | other -> other

  (* [gossipsub_app_messages_validation ctxt cryptobox head_level
     attestation_lag ?message ~message_id ()] checks for the validity of the
     given message (if any) and message id.

     First, the message id's validity is checked if the application cares about
     it and is not outdated (Otherwise `Unknown or `Outdated is returned,
     respectively). This is done thanks to
     {!gossipsub_message_id_validation}. Then, if a message is given,
     {!gossipsub_app_message_payload_validation} is used to check its
     validity. *)
  let gossipsub_app_messages_validation ctxt cryptobox head_level
      proto_parameters ?message ~message_id () =
    if is_bootstrap_node ctxt then
      (* 1. As bootstrap nodes advertise their profiles to attester and producer
         nodes, they shouldn't receive messages or messages ids. If this
         happens, received data are considered as spam (invalid), and the remote
         peer might be punished, depending on the Gossipsub implementation. *)
      `Invalid
    else
      (* Have some slack for outdated messages. *)
      let slack = 4 in
      if
        Int32.(
          sub head_level message_id.Types.Message_id.level
          > of_int (proto_parameters.Types.attestation_lag + slack))
      then
        (* 2. Nodes don't care about messages whose ids are too old.  Gossipsub
           should only be used for the dissemination of fresh data. Old data could
           be retrieved using another method. *)
        `Outdated
      else
        match
          gossipsub_message_id_validation ctxt proto_parameters message_id
        with
        | `Valid ->
            (* 3. Only check for message validity if the message_id is valid. *)
            let res =
              Option.fold
                message
                ~none:`Valid
                ~some:
                  (gossipsub_app_message_payload_validation
                     cryptobox
                     message_id)
            in
            (if res = `Valid then
               let store = Node_context.get_store ctxt in
               let traps_store = Store.traps store in
               (* TODO: https://gitlab.com/tezos/tezos/-/issues/7742
                  The [proto_parameters] are those for the last known finalized
                  level, which may differ from those of the slot level. This
                  will be an issue when the value of the [traps_fraction]
                  changes. (We cannot use {!Node_context.get_proto_parameters},
                  as it is not monad-free; we'll need to use mapping from levels
                  to parameters.) *)
               Option.iter
                 (Slot_manager.maybe_register_trap
                    traps_store
                    ~traps_fraction:proto_parameters.traps_fraction
                    message_id)
                 message) ;
            res
        | other ->
            (* 4. In the case the message id is not Valid. *)
            other

  let supports_refutations ctxt =
    let profile = Node_context.get_profile_ctxt ctxt in
    Profile_manager.supports_refutations profile

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
             if supports_refutations ctxt then
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

  let store_skip_list_cells (type block_info) ctxt cctxt dal_constants
      (block_info : block_info) block_level
      (module Plugin : Dal_plugin.T with type block_info = block_info) =
    let open Lwt_result_syntax in
    let* cells_of_level =
      let pred_published_level =
        Int32.sub
          block_level
          (Int32.of_int (1 + dal_constants.Types.attestation_lag))
      in
      Plugin.Skip_list.cells_of_level
        block_info
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
    Store.Skip_list_cells.insert
      store
      ~attested_level:block_level
      cells_of_level

  (* This functions counts, for each slot, the number of shards attested by the bakers. *)
  let attested_shards_per_slot attestations committee ~number_of_slots
      is_attested =
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
      | Operator profile -> Operator_profile.attesters profile
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
                Dal_metrics.attested_slots_for_baker_per_level_ratio
                  ~delegate
                  0. ;
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
      Plugin.block_info cctxt ~block:(`Level block_level) ~metadata:`Always
    in
    let* () =
      if supports_refutations ctxt then
        store_skip_list_cells
          ctxt
          cctxt
          proto_parameters
          block_info
          block_level
          (module Plugin : Dal_plugin.T with type block_info = Plugin.block_info)
      else return_unit
    in
    let* slot_headers = Plugin.get_published_slot_headers block_info in
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
      List.iter_es
        (fun (slot_header, status) ->
          match status with
          | Dal_plugin.Succeeded ->
              let Dal_plugin.{slot_index; commitment; published_level} =
                slot_header
              in
              let slot_id : Types.slot_id =
                {slot_level = published_level; slot_index}
              in
              Slot_manager.publish_slot_data
                ctxt
                ~level_committee:(Node_context.fetch_committee ctxt)
                ~slot_size:proto_parameters.cryptobox_parameters.slot_size
                (Node_context.get_gs_worker ctxt)
                proto_parameters
                commitment
                slot_id
          | Dal_plugin.Failed -> return_unit)
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
    let* attestations = Plugin.get_attestations block_level cctxt in
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
      block_info

  let process_block ctxt cctxt proto_parameters finalized_shell_header
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
        if is_bootstrap_node ctxt then return_unit
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
    (* This should be done at the end of the function. *)
    let last_processed_level_store = Store.last_processed_level store in
    Store.Last_processed_level.save last_processed_level_store block_level

  let rec try_process_block ~retries ctxt cctxt proto_parameters
      finalized_shell_header finalized_block_hash =
    let open Lwt_syntax in
    let* res =
      process_block
        ctxt
        cctxt
        proto_parameters
        finalized_shell_header
        finalized_block_hash
    in
    match res with
    | Error e when Layer_1.is_connection_error e && retries > 0 ->
        let* () = Lwt_unix.sleep Constants.crawler_re_processing_delay in
        try_process_block
          ~retries:(retries - 1)
          ctxt
          cctxt
          proto_parameters
          finalized_shell_header
          finalized_block_hash
    | _ -> return res

  (* Monitor finalized heads and store *finalized* published slot headers
     indexed by block hash. A slot header is considered finalized when it is in
     a block with at least two other blocks on top of it, as guaranteed by
     Tenderbake. Note that this means that shard propagation is delayed by two
     levels with respect to the publication level of the corresponding slot
     header. *)
  let new_finalized_head ctxt cctxt crawler =
    let open Lwt_result_syntax in
    let stream = Crawler.finalized_heads_stream crawler in
    let rec loop () =
      let cryptobox = Node_context.get_cryptobox ctxt in
      let*! next_final_head = Lwt_stream.get stream in
      let launch_time = Unix.gettimeofday () in
      match next_final_head with
      | None -> Lwt.fail_with "L1 crawler lib shut down"
      | Some (finalized_block_hash, finalized_shell_header) ->
          let level = finalized_shell_header.level in
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
                  pred
                  @@ add level (of_int proto_parameters.Types.attestation_lag))
              in
              let* _committee =
                Node_context.fetch_committee ctxt ~level:attestation_level
              in
              return_unit
          in
          Gossipsub.Worker.Validate_message_hook.set
            (gossipsub_app_messages_validation
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
                proto_parameters
                finalized_shell_header
                finalized_block_hash
          in
          let end_time = Unix.gettimeofday () in
          Dal_metrics.per_level_processing_time (end_time -. launch_time) ;
          loop ()
    in
    let*! () = Event.emit_layer1_node_tracking_started () in
    loop ()
end

let daemonize handlers =
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/3605
     Improve concurrent tasks by using workers *)
  let open Lwt_result_syntax in
  let* handlers = List.map_es (fun x -> x) handlers in
  let (_ : Lwt_exit.clean_up_callback_id) =
    (* close the stream when an exit signal is received *)
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _exit_status ->
        List.iter (fun (_, stopper) -> stopper ()) handlers ;
        Lwt.return_unit)
  in
  (let* _ = all (List.map fst handlers) in
   return_unit)
  |> lwt_map_error (List.fold_left (fun acc errs -> errs @ acc) [])

(** [update_timing_shards_received node_ctxt timing slot_id
   ~number_of_already_stored_shards ~number_of_shards] update the shards timing
   table values [timing] associated to slot_id [slot_id] and returns the
   corresponding slot_metrics.
   This function is intended to be called on each received shard, even
   duplicates, but uses the [number_of_already_stored_shards] to only update
   slot_metrics if the count of shards has been incremented.
   This function execution is expected to be relatively fast: no IO is
   involved, an update of Vache map is involved *)
let update_timing_shard_received node_ctxt shards_timing_table slot_id
    ~number_of_already_stored_shards ~number_of_shards =
  let now = Unix.gettimeofday () in
  let open Dal_metrics in
  let timing =
    match
      Dal_metrics.Slot_id_bounded_map.find_opt shards_timing_table slot_id
    with
    | None ->
        (* Note: we expect the entry is None only on the first received shard,
           while lwt might actually process this code after the second or third
           shard. This should be rare and the delta between values is pretty
           minimal *)
        {
          time_first_shard = now;
          duration_enough_shards = None;
          duration_all_shards = None;
        }
    | Some timing ->
        let is_all_shard_received =
          number_of_already_stored_shards = number_of_shards
        in
        if is_all_shard_received then (
          let duration = now -. timing.time_first_shard in
          Dal_metrics.update_amplification_all_shards_received_duration duration ;
          {timing with duration_all_shards = Some duration})
        else
          let cryptobox = Node_context.get_cryptobox node_ctxt in
          let redundancy_factor =
            Cryptobox.(parameters cryptobox).redundancy_factor
          in
          let is_enough_shard_received =
            Option.is_none timing.duration_enough_shards
            && number_of_already_stored_shards
               >= number_of_shards / redundancy_factor
          in
          if is_enough_shard_received then (
            let duration = now -. timing.time_first_shard in
            Dal_metrics.update_amplification_enough_shards_received_duration
              duration ;
            {timing with duration_enough_shards = Some duration})
          else timing
  in
  let () =
    Dal_metrics.Slot_id_bounded_map.replace shards_timing_table slot_id timing
  in
  timing

let connect_gossipsub_with_p2p proto_parameters gs_worker transport_layer
    node_store node_ctxt amplificator ~verbose =
  let open Gossipsub in
  let timing_table_size =
    2 * proto_parameters.Types.attestation_lag
    * proto_parameters.cryptobox_parameters.number_of_shards
    * proto_parameters.number_of_slots
  in
  let shards_timing_table =
    Dal_metrics.Slot_id_bounded_map.create timing_table_size
  in
  let shards_handler shards =
    let save_and_notify = Store.Shards.write_all shards in
    fun Types.Message.{share; _}
        Types.Message_id.{commitment; shard_index; level; slot_index; _} ->
      let open Lwt_result_syntax in
      let slot_id : Types.slot_id = {slot_level = level; slot_index} in
      let* () =
        Seq.return {Cryptobox.share; index = shard_index}
        |> save_and_notify slot_id |> Errors.to_tzresult
      in
      let number_of_shards =
        proto_parameters.cryptobox_parameters.number_of_shards
      in
      (* Introduce a new store read at each received shard. Not sure it can be
         a problem, though *)
      let* number_of_already_stored_shards =
        Store.Shards.count_values node_store slot_id
      in
      let slot_metrics =
        update_timing_shard_received
          node_ctxt
          shards_timing_table
          slot_id
          ~number_of_already_stored_shards
          ~number_of_shards
      in
      match
        Profile_manager.get_profiles @@ Node_context.get_profile_ctxt node_ctxt
      with
      | Operator profile
        when Operator_profile.is_observed_slot slot_index profile -> (
          match amplificator with
          | None ->
              let*! () = Event.emit_amplificator_uninitialized () in
              return_unit
          | Some amplificator ->
              Amplificator.try_amplification
                commitment
                slot_metrics
                slot_id
                amplificator)
      | _ -> return_unit
  in
  Lwt.dont_wait
    (fun () ->
      Transport_layer_hooks.activate
        gs_worker
        transport_layer
        ~app_messages_callback:(shards_handler node_store)
        ~verbose)
    (fun exn ->
      "[dal_node] error in Daemon.connect_gossipsub_with_p2p: "
      ^ Printexc.to_string exn
      |> Stdlib.failwith)

let resolve names =
  let open Lwt_result_syntax in
  (* Remove duplicates *)
  let names = List.sort_uniq String.compare names in
  (* Resolve the dns host names *)
  let* points =
    List.concat_map_es
      (fun name ->
        let* points =
          Tezos_base_unix.P2p_resolve.resolve_addr
            ~default_addr:"::"
            ~default_port:(Configuration_file.default.listen_addr |> snd)
            ~warn:false
            name
        in
        let*! () =
          Event.emit_resolved_bootstrap_points
            ~domainname:name
            ~number:(List.length points)
        in
        return points)
      names
  in
  let*! () =
    if points = [] then Event.emit_resolved_bootstrap_no_points ()
    else Event.emit_resolved_bootstrap_points_total ~number:(List.length points)
  in
  return points

let wait_for_l1_bootstrapped (cctxt : Rpc_context.t) =
  let open Lwt_result_syntax in
  let*! () = Event.emit_waiting_l1_node_bootstrapped () in
  let* stream, _stop = Monitor_services.bootstrapped cctxt in
  let*! () =
    Lwt_stream.iter_s (fun (_hash, _timestamp) -> Lwt.return_unit) stream
  in
  let*! () = Event.emit_l1_node_bootstrapped () in
  return_unit

let wait_for_block_with_plugin (cctxt : Rpc_context.t) =
  let open Lwt_result_syntax in
  let*! () = Event.emit_waiting_known_plugin () in
  let* stream, stop = Monitor_services.heads cctxt `Main in
  let rec wait_for_level () =
    let*! head_opt = Lwt_stream.get stream in
    match head_opt with
    | None -> failwith "Lost the connection with the L1 node"
    | Some (_hash, header) -> (
        let*! res =
          Proto_plugins.resolve_plugin_for_level
            cctxt
            ~level:header.Block_header.shell.level
        in
        match res with
        | Error [Proto_plugins.No_plugin_for_proto _] -> wait_for_level ()
        | Error err ->
            failwith "Unexpected error: %a" Error_monad.pp_print_trace err
        | Ok (module Plugin : Dal_plugin.T) ->
            let () = stop () in
            return (header, (module Plugin : Dal_plugin.T)))
  in
  wait_for_level ()

(* This function checks that in case the history mode is Rolling with a custom
   number of blocks, these number of blocks are sufficient. *)
let check_history_mode config profile_ctxt proto_parameters =
  let open Lwt_result_syntax in
  let supports_refutations =
    Profile_manager.supports_refutations profile_ctxt
  in
  let storage_period =
    Profile_manager.get_attested_data_default_store_period
      profile_ctxt
      proto_parameters
  in
  match config.Configuration_file.history_mode with
  | Rolling {blocks = `Some b} ->
      let minimal_levels =
        if supports_refutations then storage_period
        else proto_parameters.attestation_lag
      in
      if b < minimal_levels then
        tzfail @@ Errors.Not_enough_history {stored_levels = b; minimal_levels}
      else return_unit
  | Rolling {blocks = `Auto} | Full -> return_unit

(* We need more levels because [store_skip_list_cells level] needs the plugin
   for [attestation_lag + 1] levels in the past wrt to the target [level]. Note
   {!get_storage_period} refers to published levels (not attested levels). The
   plus one comes from the technical details of {!store_skip_list_cells}. *)
let skip_list_offset proto_parameters =
  proto_parameters.Types.attestation_lag + 1

(* This function determines the storage period taking into account the node's
   [first_seen_level]. Indeed, if the node started for the first time, we do not
   expect it to store skip list cells (even if it supports refutations) for the
   entire required period of 3 months, because it will anyway not have the slot
   pages for this period. Note that this could be further refined by taking into
   account when the corresponding rollup was originated. *)
let get_storage_period profile_ctxt proto_parameters head_level first_seen_level
    =
  let supports_refutations =
    Profile_manager.supports_refutations profile_ctxt
  in
  let default_storage_period =
    Profile_manager.get_attested_data_default_store_period
      profile_ctxt
      proto_parameters
  in
  if supports_refutations then
    (* This deliberately does not take into account the [last_processed_level]. *)
    let online_period =
      match first_seen_level with
      | None -> 0
      | Some first_seen_level ->
          Int32.sub head_level first_seen_level |> Int32.to_int
    in
    (* Even if the node was not online previously, or it was online only for a
       few levels, we still need to store data for the minimal period, defined
       in [get_attested_data_default_store_period]. TODO: refactor to expose
       this value. *)
    let max_period = max (2 * proto_parameters.attestation_lag) online_period in
    min max_period default_storage_period
  else default_storage_period

(* This function checks the L1 node stores enough block data for the DAL node to
   function correctly. *)
let check_l1_history_mode profile_ctxt cctxt proto_parameters head_level
    first_level =
  let open Lwt_result_syntax in
  let* l1_history_mode =
    let* l1_mode, blocks_preservation_cycles_opt =
      Config_services.history_mode cctxt
    in
    (* Note: For the DAL node it does not matter if the L1 node is in Full or
       Rolling mode, because the DAL node is not interested in blocks outside of
       a certain time window. *)
    return
    @@
    match (l1_mode, blocks_preservation_cycles_opt) with
    | Archive, _ -> `L1_archive
    | Full None, None
    | Rolling None, None
    | Full (Some _), None
    | Rolling (Some _), None
    | Full None, Some _
    | Rolling None, Some _ ->
        (* unreachable cases, at least for now *) assert false
    | Full (Some additional_cycles), Some blocks_preservation_cycles
    | Rolling (Some additional_cycles), Some blocks_preservation_cycles ->
        `L1_rolling (additional_cycles.offset + blocks_preservation_cycles)
  in
  let check ~dal_blocks ~l1_cycles =
    let blocks_per_cycle =
      Int32.to_int proto_parameters.Types.blocks_per_cycle
    in
    let dal_cycles = dal_blocks / blocks_per_cycle in
    let minimal_cycles =
      if dal_blocks mod blocks_per_cycle = 0 then dal_cycles else 1 + dal_cycles
    in
    if l1_cycles < minimal_cycles then
      tzfail
      @@ Errors.Not_enough_l1_history
           {stored_cycles = l1_cycles; minimal_cycles}
    else return_unit
  in
  match l1_history_mode with
  | `L1_archive -> return_unit
  | `L1_rolling l1_cycles ->
      (* For the non-"refutation supporting" profiles, we don't currently need
         that many levels in the past, because we don't for instance retrieve the
         protocol parameters for such past levels; though we should. *)
      let dal_blocks =
        get_storage_period profile_ctxt proto_parameters head_level first_level
        +
        if Profile_manager.supports_refutations profile_ctxt then
          skip_list_offset proto_parameters
        else 0
      in
      check ~dal_blocks ~l1_cycles

let build_profile_context config =
  let open Lwt_result_syntax in
  let base_dir = Configuration_file.store_path config in
  let*! res = Profile_manager.load_profile_ctxt ~base_dir in
  match res with
  | Ok loaded_profile ->
      (* The profiles from the loaded context are prioritized over the
         profiles provided in the config file. *)
      Profile_manager.merge_profiles
        ~lower_prio:config.Configuration_file.profile
        ~higher_prio:(Profile_manager.Profile loaded_profile)
      |> return
  | Error error ->
      let*! () = Event.emit_loading_profiles_failed ~error in
      return config.Configuration_file.profile

(* Registers the attester profile context once we have the protocol plugin. This is supposed
   to be called only once. *)
let update_and_register_profiles ctxt =
  let open Lwt_result_syntax in
  let profile_ctxt = Node_context.get_profile_ctxt ctxt in
  let gs_worker = Node_context.get_gs_worker ctxt in
  let*? proto_parameters =
    Node_context.get_proto_parameters ctxt ~level:`Last_proto
  in
  let profile_ctxt =
    Profile_manager.register_profile
      profile_ctxt
      ~number_of_slots:proto_parameters.number_of_slots
      gs_worker
  in
  let*! () = Node_context.set_profile_ctxt ctxt profile_ctxt in
  return_unit

(* This function fetches the protocol plugins for levels in the past for which
   the node may need a plugin, namely for adding skip list cells, or for
   obtaining the protocol parameters.

   Concerning the skip list, getting the plugin is (almost) necessary as skip
   list cells are stored in the storage for a certain period and
   [store_skip_list_cells] needs the L1 context for levels in this period. (It
   would actually not be necessary to go as far in the past, because the
   protocol parameters and the relevant encodings do not change for now, so the
   head plugin could be used). *)
let get_proto_plugins cctxt profile_ctxt ~last_processed_level ~first_seen_level
    head_level proto_parameters =
  let storage_period =
    get_storage_period profile_ctxt proto_parameters head_level first_seen_level
  in
  let first_level =
    Int32.max
      (match last_processed_level with None -> 1l | Some level -> level)
      Int32.(sub head_level (of_int storage_period))
  in
  let first_level =
    if Profile_manager.supports_refutations profile_ctxt then
      Int32.sub first_level (Int32.of_int (skip_list_offset proto_parameters))
    else
      (* The DAL node may need the protocol parameters [attestation_lag] in the
         past wrt to the head level. *)
      Int32.sub first_level (Int32.of_int proto_parameters.attestation_lag)
  in
  let first_level = Int32.(max 1l first_level) in
  Proto_plugins.initial_plugins cctxt ~first_level ~last_level:head_level

(* This function removes old data starting from [last_processed_level -
   storage_period] to [target_level - storage_period], where [storage_period] is
   the period for which the DAL node stores data related to attested slots and
   [target_level] is the level at which we connect the P2P and switch to
   processing blocks in sync with the L1. [target_level] is set to [head_level -
   3]. It also inserts skip list cells if needed in the period [head_level -
   storage_level].

   FIXME: https://gitlab.com/tezos/tezos/-/issues/7429
   We don't call [may_add_plugin], so there is a chance the plugin changes
   and we don't detect it if this code starts running just before the migration
   level, and the head changes meanwhile to be above the migration level.

   TODO: https://gitlab.com/tezos/tezos/-/issues/7779
   Improve the runtime of this function. It may be better to do the clean-up and
   the "catch-up" (that is, updating of the skip list store) separately. *)
let clean_up_store_and_catch_up_for_refutation_support ctxt cctxt
    ~last_processed_level ~first_seen_level head_level proto_parameters =
  let open Lwt_result_syntax in
  let store_skip_list_cells ~level =
    let*? (module Plugin) =
      Node_context.get_plugin_for_level ctxt ~level:(Int32.pred level)
    in
    let* block_info =
      Plugin.block_info cctxt ~block:(`Level level) ~metadata:`Always
    in
    let*? dal_constants =
      Node_context.get_proto_parameters ctxt ~level:(`Level level)
    in
    Handler.store_skip_list_cells
      ctxt
      cctxt
      dal_constants
      block_info
      level
      (module Plugin : Dal_plugin.T with type block_info = Plugin.block_info)
  in
  let store = Node_context.get_store ctxt in
  let last_processed_level_store = Store.last_processed_level store in
  (* [target_level] identifies the level wrt to head level at which we want to
     start the P2P and process blocks as usual. It's set to [head_level - 3]
     because the first level the DAL node should process should be a final
     one. *)
  let target_level head_level = Int32.(sub head_level 3l) in
  let first_level_for_skip_list_storage period level =
    (* Note that behind this first level we do not have the plugin. *)
    Int32.(sub level (of_int period))
  in
  let should_store_skip_list_cells ~head_level =
    let profile_ctxt = Node_context.get_profile_ctxt ctxt in
    let period =
      get_storage_period
        profile_ctxt
        proto_parameters
        head_level
        first_seen_level
      + skip_list_offset proto_parameters
    in
    let first_level = first_level_for_skip_list_storage period head_level in
    fun ~level -> level >= first_level
  in
  let rec do_clean_up last_processed_level head_level =
    let last_level = target_level head_level in
    let should_store_skip_list_cells =
      should_store_skip_list_cells ~head_level
    in
    let rec clean_up_at_level level =
      if level > last_level then return_unit
      else
        let*! () =
          Handler.remove_old_level_stored_data proto_parameters ctxt level
        in
        let* () =
          if should_store_skip_list_cells ~level then
            store_skip_list_cells ~level
          else return_unit
        in
        let* () =
          Store.Last_processed_level.save last_processed_level_store level
        in
        let*! () =
          if Int32.to_int level mod 1000 = 0 then
            Event.emit_catching_up ~current_level:level
          else Lwt.return_unit
        in
        clean_up_at_level (Int32.succ level)
    in
    (* Clean up from [last_processed_level] to [last_level]. *)
    let* () = clean_up_at_level (Int32.succ last_processed_level) in
    (* As this iteration may be slow, the head level might have advanced in the
       meanwhile. *)
    let* header =
      Shell_services.Blocks.Header.shell_header cctxt ~block:(`Head 0) ()
    in
    let new_head_level = header.Block_header.level in
    if new_head_level > head_level then do_clean_up last_level new_head_level
    else
      let*! () = Event.emit_end_catchup () in
      return_unit
  in
  let*! () =
    let levels_to_clean_up =
      Int32.(succ @@ sub head_level last_processed_level)
    in
    if levels_to_clean_up > 0l then
      Event.emit_start_catchup
        ~start_level:last_processed_level
        ~end_level:head_level
        ~levels_to_clean_up
    else Lwt.return_unit
  in
  let* () = do_clean_up last_processed_level head_level in
  return_unit

let clean_up_store_and_catch_up_for_no_refutation_support ctxt
    ~last_processed_level head_level proto_parameters =
  let open Lwt_result_syntax in
  let profile_ctxt = Node_context.get_profile_ctxt ctxt in
  let storage_period =
    Profile_manager.get_attested_data_default_store_period
      profile_ctxt
      proto_parameters
    |> Int32.of_int
  in
  (* We clean-up *for* (not at) levels between [last_processed_level + 1] and
     [finalized_level - 1], because [last_processed_level] was the last level
     for which there was already a clean-up, and [finalized_level] will be the
     first level to be processed after this restart. However, there is no need
     to clean-up for levels higher than [last_processed_level + storage_period]
     because there is no data corresponding to such levels.

     ("Level *for* cleaning" refers to the level passed to
     [Handler.remove_old_level_stored_data], not to the level at which there is
     data to be wiped.)

     Examples: Say [last_processed_level = 1000] and [storage_period =
     100]. Thus we have data stored for levels 901 to 1000.

     Example 1: Say [finalized_level = 1060]. We clean-up for levels 1001 up to
     1060, that is, we wipe data from level 901 up to level 960.

     Example 2: Say [finalized_level = 3000]. We clean-up for levels 1001 up to
     1100 (so at levels 901 up to 1000). *)
  let finalized_level = Int32.sub head_level 2l in
  let new_last_processed_level = Int32.(max 1l (pred finalized_level)) in
  let last_level_for_cleaning =
    let highest_level_with_data_for_cleaning =
      Int32.add last_processed_level storage_period
    in
    Int32.(min new_last_processed_level highest_level_with_data_for_cleaning)
  in
  let rec cleanup level =
    if level > last_level_for_cleaning then
      let store = Node_context.get_store ctxt in
      let last_processed_level_store = Store.last_processed_level store in
      let* () =
        Store.Last_processed_level.save
          last_processed_level_store
          new_last_processed_level
      in
      let*! () = Event.emit_end_catchup () in
      return_unit
    else
      let*! () =
        Handler.remove_old_level_stored_data proto_parameters ctxt level
      in
      cleanup @@ Int32.succ level
  in
  let*! () =
    Event.emit_start_catchup
      ~start_level:last_processed_level
      ~end_level:last_level_for_cleaning
      ~levels_to_clean_up:
        Int32.(sub last_level_for_cleaning last_processed_level)
  in
  cleanup (Int32.succ last_processed_level)

let clean_up_store_and_catch_up ctxt cctxt ~last_processed_level
    ~first_seen_level head_level proto_parameters =
  if Handler.supports_refutations ctxt then
    clean_up_store_and_catch_up_for_refutation_support
      ctxt
      cctxt
      ~last_processed_level
      ~first_seen_level
      head_level
      proto_parameters
  else
    clean_up_store_and_catch_up_for_no_refutation_support
      ctxt
      ~last_processed_level
      head_level
      proto_parameters

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/3605
   Improve general architecture, handle L1 disconnection etc
*)
let run ~data_dir ~configuration_override =
  let open Lwt_result_syntax in
  let log_cfg = Tezos_base_unix.Logs_simple_config.default_cfg in
  let internal_events =
    Tezos_base_unix.Internal_event_unix.make_with_defaults
      ~enable_default_daily_logs_at:Filename.Infix.(data_dir // "daily_logs")
      ~log_cfg
      ()
  in
  let*! () =
    Tezos_base_unix.Internal_event_unix.init ~config:internal_events ()
  in
  let*! () = Event.emit_starting_node () in
  let* ({
          rpc_addr;
          (* These are not the cryptographic identities of peers, but the points
             (IP addresses + ports) of the nodes we want to connect to at
             startup. *)
          peers = points;
          endpoint;
          profile;
          listen_addr;
          public_addr;
          _;
        } as config) =
    let*! result = Configuration_file.load ~data_dir in
    match result with
    | Ok configuration -> return (configuration_override configuration)
    | Error _ ->
        let*! () = Event.emit_data_dir_not_found ~path:data_dir in
        (* Store the default configuration if no configuration were found. *)
        let configuration = configuration_override Configuration_file.default in
        let* () = Configuration_file.save configuration in
        return configuration
  in
  let*! () = Event.emit_configuration_loaded () in
  let cctxt = Rpc_context.make endpoint in
  let* dal_config = fetch_dal_config cctxt in
  let* network_name = infer_dal_network_name cctxt in
  let bootstrap_names = points @ dal_config.bootstrap_peers in
  let*! () =
    if bootstrap_names = [] then Event.emit_config_error_no_bootstrap ()
    else Lwt.return_unit
  in
  (* Resolve:
     - [points] from DAL node config file and CLI.
     - [dal_config.bootstrap_peers] from the L1 network config. *)
  (* Update the list of bootstrap every 5 minutes *)
  let* get_bootstrap_points =
    let* current_points = resolve bootstrap_names in
    let bootstrap_points = ref current_points in
    let rec loop () =
      catch_es
        (fun () ->
          let*! () = Lwt_unix.sleep Constants.bootstrap_dns_refresh_delay in
          let* current_points = resolve bootstrap_names in
          bootstrap_points := current_points ;
          loop ())
        ~catch_only:(function Lwt.Canceled -> true | _ -> false)
    in
    let dns_job = loop () in
    let (_ : Lwt_exit.clean_up_callback_id) =
      Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _exit_status ->
          let () = Lwt.cancel dns_job in
          Lwt.return_unit)
    in
    return (fun () -> !bootstrap_points)
  in
  let* p2p_config = Transport_layer_parameters.p2p_config config in
  let p2p_limits = Transport_layer_parameters.p2p_limits in
  (* Get the current L1 head and its DAL plugin and parameters. *)
  let* header, (module Plugin : Dal_plugin.T) =
    wait_for_block_with_plugin cctxt
  in
  let head_level = header.Block_header.shell.level in
  let* proto_parameters =
    Plugin.get_constants `Main (`Level head_level) cctxt
  in
  let proto_plugins =
    Proto_plugins.singleton
      ~first_level:head_level
      ~proto_level:header.shell.proto_level
      (module Plugin)
      proto_parameters
  in
  (* Set proto number of slots hook. *)
  Value_size_hooks.set_number_of_slots proto_parameters.number_of_slots ;
  let* profile_ctxt =
    let+ profile_ctxt = build_profile_context config in
    Profile_manager.resolve_profile
      profile_ctxt
      ~number_of_slots:proto_parameters.number_of_slots
  in
  let*? () =
    Profile_manager.validate_slot_indexes
      profile_ctxt
      ~number_of_slots:proto_parameters.number_of_slots
  in
  (* Create and start a GS worker *)
  let gs_worker =
    let rng =
      let seed =
        Random.self_init () ;
        Random.bits ()
      in
      Random.State.make [|seed|]
    in
    let open Worker_parameters in
    let limits =
      if Profile_manager.is_bootstrap_profile profile_ctxt then
        (* Bootstrap nodes should always have a mesh size of zero.
           so all grafts are responded with prunes with PX. See:
           https://github.com/libp2p/specs/blob/f5c5829ef9753ef8b8a15d36725c59f0e9af897e/pubsub/gossipsub/gossipsub-v1.1.md#recommendations-for-network-operators

           Additionally, we set [max_sent_iwant_per_heartbeat = 0]
           so bootstrap nodes do not download any shards via IHave/IWant
           transfers.

           Also, we set [prune_backoff = 10] so that bootstrap nodes send PX
           peers quicker. In particular, we want to avoid the following
           scenario: a peer receives a Prune from the bootstrap node, then
           disconnects for some reason and reconnects within the backoff
           period; with a large backoff, its first Graft will be answered with
           a no PX Prune, and therefore the peer will have to wait for the new
           backoff timeout to be able to obtain PX peers. *)
        {
          limits with
          max_sent_iwant_per_heartbeat = 0;
          degree_low = 0;
          degree_high = 0;
          degree_out = 0;
          degree_optimal = 0;
          degree_score = 0;
          prune_backoff = Ptime.Span.of_int_s 10;
        }
      else limits
    in
    let identity = p2p_config.P2p.identity in
    let self =
      (* What matters is the identity, the reachable point is more like a placeholder here. *)
      Types.Peer.
        {peer_id = identity.peer_id; maybe_reachable_point = public_addr}
    in
    let gs_worker =
      Gossipsub.Worker.(
        make
          ~bootstrap_points:get_bootstrap_points
          ~events_logging:(Logging.event ~verbose:config.verbose)
          ~self
          rng
          limits
          peer_filter_parameters)
    in
    Gossipsub.Worker.start [] gs_worker ;
    gs_worker
  in
  let points = get_bootstrap_points () in
  (* Create a transport (P2P) layer instance. *)
  let* transport_layer =
    Gossipsub.Transport_layer.create
      ~public_addr
      ~is_bootstrap_peer:(profile = Profile_manager.bootstrap)
      p2p_config
      p2p_limits
      ~network_name
  in
  let (_ : Lwt_exit.clean_up_callback_id) =
    (* This is important to prevent stall connections. *)
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _exit_status ->
        Gossipsub.Transport_layer.shutdown transport_layer)
  in
  (* Initialize store *)
  let* store = Store.init config in
  let* last_processed_level =
    let last_processed_level_store = Store.last_processed_level store in
    Store.Last_processed_level.load last_processed_level_store
  in
  let first_seen_level_store = Store.first_seen_level store in
  let* first_seen_level = Store.First_seen_level.load first_seen_level_store in
  (* Check the DAL node's and L1 node's history mode. *)
  let* () = check_history_mode config profile_ctxt proto_parameters in
  let* () =
    match first_seen_level with
    | None -> Store.First_seen_level.save first_seen_level_store head_level
    | Some _ -> return_unit
  in
  let* () =
    check_l1_history_mode
      profile_ctxt
      cctxt
      proto_parameters
      head_level
      first_seen_level
  in
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5743
     Instead of recomputing these parameters, they could be stored
     (for a given cryptobox). *)
  let* cryptobox, shards_proofs_precomputation =
    init_cryptobox config proto_parameters profile_ctxt
  in
  (* Set crypto box share size hook. *)
  Value_size_hooks.set_share_size
    (Cryptobox.Internal_for_tests.encoded_share_size cryptobox) ;
  let ctxt =
    Node_context.init
      config
      profile_ctxt
      cryptobox
      shards_proofs_precomputation
      proto_plugins
      store
      gs_worker
      transport_layer
      cctxt
      ~last_finalized_level:head_level
      ~network_name
  in
  let* () =
    match Profile_manager.get_profiles profile_ctxt with
    | Operator profile ->
        Node_context.warn_if_attesters_not_delegates ctxt profile
    | _ -> return_unit
  in
  Gossipsub.Worker.Validate_message_hook.set
    (Handler.gossipsub_app_messages_validation
       ctxt
       cryptobox
       head_level
       proto_parameters) ;
  let is_prover_profile = Profile_manager.is_prover_profile profile_ctxt in
  (* Initialize amplificator if in prover profile.
     This forks a process and should be kept early to avoid copying opened file
     descriptors. *)
  let* amplificator =
    if is_prover_profile then
      let* amplificator = Amplificator.make ctxt in
      return_some amplificator
    else return_none
  in
  (* Starts the metrics *after* the amplificator fork, to avoid forked opened
     sockets *)
  let* () =
    match config.metrics_addr with
    | None ->
        let*! () = Event.emit_metrics_server_not_starting () in
        return_unit
    | Some metrics_addr ->
        let*! () = Event.emit_metrics_server_starting ~endpoint:metrics_addr in
        let*! _metrics_server = Metrics.launch metrics_addr in
        return_unit
  in
  (* Start RPC server. We do that before the waiting for the L1 node to be
     bootstrapped so that queries can already be issued. Note that that the node
     will thus already respond to the baker about shards status if queried. *)
  let* rpc_server = RPC_server.(start config ctxt) in
  let _ = RPC_server.install_finalizer rpc_server in
  let*! () = Event.emit_rpc_server_is_ready ~point:rpc_addr in
  (* Wait for the L1 node to be bootstrapped. *)
  let* () = wait_for_l1_bootstrapped cctxt in
  let* proto_plugins =
    get_proto_plugins
      cctxt
      profile_ctxt
      ~last_processed_level
      ~first_seen_level
      head_level
      proto_parameters
  in
  Node_context.set_proto_plugins ctxt proto_plugins ;
  let* () =
    match last_processed_level with
    | None -> (* there's nothing to clean up *) return_unit
    | Some last_processed_level ->
        clean_up_store_and_catch_up
          ctxt
          cctxt
          ~last_processed_level
          ~first_seen_level
          head_level
          proto_parameters
  in
  let* crawler =
    (* We reload the last processed level because [clean_up_store] has likely
       modified it. *)
    let* last_notified_level =
      let last_processed_level_store = Store.last_processed_level store in
      Store.Last_processed_level.load last_processed_level_store
    in
    let open Constants in
    let*! crawler =
      Crawler.start
        ~name:"dal_node_crawler"
        ~chain:`Main
        ~reconnection_delay:initial_l1_crawler_reconnection_delay
        ~l1_blocks_cache_size:crawler_l1_blocks_cache_size
        ?last_notified_level
        cctxt
    in
    return crawler
  in
  (* Activate the p2p instance. *)
  let shards_store = Store.shards store in
  connect_gossipsub_with_p2p
    proto_parameters
    gs_worker
    transport_layer
    shards_store
    ctxt
    amplificator
    ~verbose:config.verbose ;
  let*! () =
    Gossipsub.Transport_layer.activate ~additional_points:points transport_layer
  in
  let*! () = Event.emit_p2p_server_is_ready ~point:listen_addr in
  (* Start collecting stats related to the Gossipsub worker. *)
  Dal_metrics.collect_gossipsub_metrics gs_worker ;
  (* Register topics with gossipsub worker. *)
  let* () = update_and_register_profiles ctxt in
  (* Start never-ending monitoring daemons *)
  let version = Tezos_version_value.Bin_version.octez_version_string in
  let*! () = Event.emit_node_is_ready ~network_name ~version in
  let* () = daemonize [Handler.new_finalized_head ctxt cctxt crawler] in
  return_unit

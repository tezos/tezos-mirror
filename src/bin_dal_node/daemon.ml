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

let fetch_dal_config cctxt =
  let open Lwt_syntax in
  let delay = 0.1 in
  let delay_max = 20.0 in
  let rec retry delay =
    let* r = Config_services.dal_config cctxt in
    match r with
    | Error
        [RPC_client_errors.(Request_failed {error = Connection_failed _; _})] ->
        let delay = min delay_max (delay *. 2.) in
        let* () =
          if delay < delay_max then
            Event.(
              emit
                retry_fetching_node_config_notice
                (Uri.to_string cctxt#base, delay))
          else
            Event.(
              emit
                retry_fetching_node_config_warning
                (Uri.to_string cctxt#base, delay))
        in
        let* () = Lwt_unix.sleep delay in
        retry delay
    | Error err -> return_error err
    | Ok dal_config ->
        let* () =
          Event.(emit fetched_config_success (Uri.to_string cctxt#base))
        in
        return_ok dal_config
  in
  retry delay

let init_cryptobox config (proto_parameters : Dal_plugin.proto_parameters) =
  let open Lwt_result_syntax in
  let prover_srs =
    Profile_manager.is_prover_profile config.Configuration_file.profile
  in
  let* () =
    if prover_srs then
      let find_srs_files () = Tezos_base.Dal_srs.find_trusted_setup_files () in
      Cryptobox.init_prover_dal ~find_srs_files ()
    else return_unit
  in
  match Cryptobox.make proto_parameters.cryptobox_parameters with
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
        let err =
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
        Event.(
          emit__dont_wait__use_with_care
            message_validation_error
            (message_id, err)) ;
        `Invalid
    | exception exn ->
        (* Don't crash if crypto raised an exception. *)
        let err = Printexc.to_string exn in
        Event.(
          emit__dont_wait__use_with_care
            message_validation_error
            (message_id, err)) ;
        `Invalid

  let is_bootstrap_node ctxt =
    Node_context.get_profile_ctxt ctxt |> Profile_manager.is_bootstrap_profile

  let gossipsub_message_id_commitment_validation ctxt proto_parameters
      message_id =
    let store = Node_context.get_store ctxt in
    let slot_index = message_id.Types.Message_id.slot_index in
    match
      Store.Slot_id_cache.find_opt
        store.finalized_commitments
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
          slot_index >= 0
          && slot_index < proto_parameters.Dal_plugin.number_of_slots
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
             (of_int proto_parameters.Dal_plugin.attestation_lag))
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
          > of_int (proto_parameters.Dal_plugin.attestation_lag + slack))
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
            Option.fold
              message
              ~none:`Valid
              ~some:
                (gossipsub_app_message_payload_validation cryptobox message_id)
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
      let* res = Store.Shards.remove store.shards slot_id in
      match res with
      | Ok () ->
          Event.(
            emit removed_slot_shards (slot_id.slot_level, slot_id.slot_index))
      | Error err ->
          Event.(
            emit
              removing_shards_failed
              (slot_id.slot_level, slot_id.slot_index, err))
    in
    let* () =
      let* res = Store.Slots.remove_slot store.slots ~slot_size slot_id in
      match res with
      | Ok () ->
          Event.(emit removed_slot (slot_id.slot_level, slot_id.slot_index))
      | Error err ->
          Event.(
            emit
              removing_slot_failed
              (slot_id.slot_level, slot_id.slot_index, err))
    in
    return_unit

  (* This function removes from the store slot data (slots, their shards, and
     their status) for commitments published at level exactly
     {!Node_context.level_to_gc ~current_level}. It also removes skip list
     cells attested at that level. *)
  let remove_old_level_stored_data proto_parameters ctxt current_level =
    let open Lwt_syntax in
    Node_context.level_to_gc ctxt proto_parameters ~current_level
    |> Option.iter_s (fun oldest_level ->
           let store = Node_context.get_store ctxt in
           let* () =
             (* TODO: https://gitlab.com/tezos/tezos/-/issues/7258
                We may want to remove this check. *)
             if supports_refutations ctxt then
               let* res =
                 Skip_list_cells_store.remove
                   store.skip_list_cells
                   ~attested_level:oldest_level
               in
               match res with
               | Ok () -> Event.(emit removed_skip_list_cells oldest_level)
               | Error err ->
                   Event.(
                     emit removing_skip_list_cells_failed (oldest_level, err))
             else return_unit
           in
           let number_of_slots =
             Dal_plugin.(proto_parameters.number_of_slots)
           in
           let* () =
             let* res =
               Store.Statuses.remove_level_status
                 ~level:oldest_level
                 store.slot_header_statuses
             in
             match res with
             | Ok () -> Event.(emit removed_status oldest_level)
             | Error err ->
                 Event.(emit removing_status_failed (oldest_level, err))
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
    let number_of_slots = proto_parameters.Dal_plugin.number_of_slots in
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
          (Int32.of_int proto_parameters.Dal_plugin.attestation_lag)
      in
      Node_context.fetch_committee ctxt ~level
    in
    Profile_manager.on_new_head
      (Node_context.get_profile_ctxt ctxt)
      proto_parameters
      (Node_context.get_gs_worker ctxt)
      committee

  let get_constants ctxt cctxt ~level =
    let open Lwt_result_syntax in
    let*? (module Plugin) = Node_context.get_plugin_for_level ctxt ~level in
    Plugin.get_constants `Main (`Level level) cctxt

  let store_skip_list_cells (type block_info) ctxt cctxt dal_constants
      (block_info : block_info) block_level
      (module Plugin : Dal_plugin.T with type block_info = block_info) =
    let open Lwt_result_syntax in
    let* cells_of_level =
      let pred_published_level =
        Int32.sub
          block_level
          (Int32.of_int (1 + dal_constants.Dal_plugin.attestation_lag))
      in
      Plugin.Skip_list.cells_of_level
        block_info
        cctxt
        ~dal_constants
        ~pred_publication_level_dal_constants:
          (lazy (get_constants ctxt cctxt ~level:pred_published_level))
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
      store.skip_list_cells
      ~attested_level:block_level
      cells_of_level

  let process_block ctxt cctxt proto_parameters finalized_shell_header =
    let open Lwt_result_syntax in
    let store = Node_context.get_store ctxt in
    let block_level = finalized_shell_header.Block_header.level in
    let block = `Level block_level in
    let pred_level = Int32.pred block_level in
    let*? (module Plugin) =
      Node_context.get_plugin_for_level ctxt ~level:pred_level
    in
    let* block_info = Plugin.block_info cctxt ~block ~metadata:`Always in
    let* dal_constants = get_constants ctxt cctxt ~level:block_level in
    let* () =
      if dal_constants.Dal_plugin.feature_enable then
        let* slot_headers = Plugin.get_published_slot_headers block_info in
        let* () =
          if supports_refutations ctxt then
            store_skip_list_cells
              ctxt
              cctxt
              dal_constants
              block_info
              block_level
              (module Plugin : Dal_plugin.T
                with type block_info = Plugin.block_info)
          else return_unit
        in
        let* () =
          if not (is_bootstrap_node ctxt) then
            Slot_manager.store_slot_headers
              ~number_of_slots:proto_parameters.Dal_plugin.number_of_slots
              ~block_level
              slot_headers
              store
          else return_unit
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
                    ~level_committee:(Node_context.fetch_committee ctxt)
                    ~slot_size:proto_parameters.cryptobox_parameters.slot_size
                    store
                    (Node_context.get_gs_worker ctxt)
                    proto_parameters
                    commitment
                    slot_id
              | Dal_plugin.Failed -> return_unit)
            slot_headers
        in
        let*? attested_slots = Plugin.attested_slot_headers block_info in
        let* () =
          Slot_manager.update_selected_slot_headers_statuses
            ~block_level
            ~attestation_lag:proto_parameters.attestation_lag
            ~number_of_slots:proto_parameters.number_of_slots
            (Plugin.is_attested attested_slots)
            store
        in
        let*! () =
          remove_unattested_slots_and_shards
            proto_parameters
            ctxt
            ~published_level:
              Int32.(sub block_level (of_int proto_parameters.attestation_lag))
            (Plugin.is_attested attested_slots)
        in
        let* () = may_update_topics ctxt proto_parameters ~block_level in
        return_unit
      else return_unit
    in
    let*? block_round = Plugin.get_round finalized_shell_header.fitness in
    Dal_metrics.layer1_block_finalized ~block_level ;
    Dal_metrics.layer1_block_finalized_round ~block_round ;
    let*! () =
      Event.(emit layer1_node_final_block (block_level, block_round))
    in
    (* This should be done at the end of the function. *)
    Store.Last_processed_level.save store.last_processed_level block_level

  let rec try_process_block ~retries ctxt cctxt proto_parameters
      finalized_shell_header =
    let open Lwt_syntax in
    let* res =
      process_block ctxt cctxt proto_parameters finalized_shell_header
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
      let proto_parameters = Node_context.get_proto_parameters ctxt in
      let cryptobox = Node_context.get_cryptobox ctxt in
      let*! next_final_head = Lwt_stream.get stream in
      match next_final_head with
      | None -> Lwt.fail_with "L1 crawler lib shut down"
      | Some (_finalized_hash, finalized_shell_header) ->
          let level = finalized_shell_header.level in
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
                  @@ add
                       level
                       (of_int proto_parameters.Dal_plugin.attestation_lag))
              in
              let* _committee =
                Node_context.fetch_committee ctxt ~level:attestation_level
              in
              return_unit
          in
          let* () =
            (* FIXME: https://gitlab.com/tezos/tezos/-/issues/7291
               We should use the head level instead. *)
            Node_context.may_add_plugin
              ctxt
              cctxt
              ~proto_level:finalized_shell_header.proto_level
              ~block_level:level
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
          in
          loop ()
    in
    let*! () = Event.(emit layer1_node_tracking_started ()) in
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

let connect_gossipsub_with_p2p gs_worker transport_layer node_store node_ctxt
    amplificator =
  let open Gossipsub in
  let shards_handler ({shards; _} : Store.t) =
    let save_and_notify = Store.Shards.write_all shards in
    fun Types.Message.{share; _}
        Types.Message_id.{commitment; shard_index; level; slot_index; _} ->
      let open Lwt_result_syntax in
      let slot_id : Types.slot_id = {slot_level = level; slot_index} in
      let* () =
        Seq.return {Cryptobox.share; index = shard_index}
        |> save_and_notify slot_id |> Errors.to_tzresult
      in
      match
        Profile_manager.get_profiles @@ Node_context.get_profile_ctxt node_ctxt
      with
      | Operator profile
        when Operator_profile.is_observed_slot slot_index profile -> (
          match amplificator with
          | None ->
              let*! () = Event.(emit amplificator_uninitialized ()) in
              return_unit
          | Some amplificator ->
              Amplificator.try_amplification commitment slot_id amplificator)
      | _ -> return_unit
  in
  Lwt.dont_wait
    (fun () ->
      Transport_layer_hooks.activate
        gs_worker
        transport_layer
        ~app_messages_callback:(shards_handler node_store))
    (fun exn ->
      "[dal_node] error in Daemon.connect_gossipsub_with_p2p: "
      ^ Printexc.to_string exn
      |> Stdlib.failwith)

let resolve points =
  List.concat_map_es
    (Tezos_base_unix.P2p_resolve.resolve_addr
       ~default_addr:"::"
       ~default_port:(Configuration_file.default.listen_addr |> snd))
    points

let wait_for_l1_bootstrapped (cctxt : Rpc_context.t) =
  let open Lwt_result_syntax in
  let*! () = Event.(emit waiting_l1_node_bootstrapped) () in
  let* stream, _stop = Monitor_services.bootstrapped cctxt in
  let*! () =
    Lwt_stream.iter_s (fun (_hash, _timestamp) -> Lwt.return_unit) stream
  in
  let*! () = Event.(emit l1_node_bootstrapped) () in
  return_unit

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
  proto_parameters.Dal_plugin.attestation_lag + 1

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
      Int32.to_int proto_parameters.Dal_plugin.blocks_per_cycle
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
        ~higher_prio:loaded_profile
      |> return
  | Error err ->
      let*! () = Event.(emit loading_profiles_failed err) in
      return config.Configuration_file.profile

(* Registers the attester profile context once we have the protocol plugin. This is supposed
   to be called only once. *)
let update_and_register_profiles ctxt =
  let open Lwt_result_syntax in
  let profile_ctxt = Node_context.get_profile_ctxt ctxt in
  let gs_worker = Node_context.get_gs_worker ctxt in
  let proto_parameters = Node_context.get_proto_parameters ctxt in
  let profile_ctxt =
    Profile_manager.register_profile profile_ctxt proto_parameters gs_worker
  in
  let*! () = Node_context.set_profile_ctxt ctxt profile_ctxt in
  return_unit

(* This function fetches the protocol plugins for levels for which it is needed
   to add skip list cells. It starts by computing the oldest level at which it
   will be needed to add skip list cells. *)
let get_proto_plugins cctxt profile_ctxt ~last_processed_level ~first_seen_level
    head_level proto_parameters =
  (* We resolve the plugins for all levels starting with [(max
     last_processed_level (head_level - storage_period)], or (max
     last_processed_level (head_level - storage_period) - (attestation_lag -
     1))] in case the node supports refutations. This is necessary as skip list
     cells are stored for attested levels is this storage period and
     [store_skip_list_cells] needs the L1 context for these levels. (It would
     actually not be necessary to go as far in the past, because the protocol
     parameters and the relevant encodings do not change for now, so the head
     plugin could be used). *)
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
    else first_level
  in
  let first_level = Int32.(max 1l first_level) in
  Proto_plugins.initial_plugins cctxt ~first_level ~last_level:head_level

(* This function removes old data starting from [last_processed_level -
   storage_period] to [target_level - storage_period], where [storage_period] is
   the period for which the DAL node stores data related to attested slots and
   [target_level] is the level at which we connect the P2P and switch to
   processing blocks in sync with the L1. [target_level] is set to [head_level -
   2]. It also inserts skip list cells if needed in the period [head_level -
   storage_level].

   FIXME: https://gitlab.com/tezos/tezos/-/issues/7429
   We don't call [may_add_plugin], so there is a chance the plugin changes
   and we don't detect it if this code starts running just before the migration
   level, and the head changes meanwhile to be above the migration level.
*)
let clean_up_store ctxt cctxt ~last_processed_level ~first_seen_level head_level
    proto_parameters =
  let open Lwt_result_syntax in
  let store_skip_list_cells ~level =
    let*? (module Plugin) =
      Node_context.get_plugin_for_level ctxt ~level:(Int32.pred level)
    in
    let* block_info =
      Plugin.block_info cctxt ~block:(`Level level) ~metadata:`Always
    in
    let* dal_constants = Handler.get_constants ctxt cctxt ~level in
    Handler.store_skip_list_cells
      ctxt
      cctxt
      dal_constants
      block_info
      level
      (module Plugin : Dal_plugin.T with type block_info = Plugin.block_info)
  in
  let store = Node_context.get_store ctxt in
  let supports_refutations = Handler.supports_refutations ctxt in
  (* [target_level] identifies the level wrt to head level at which we want to
     start the P2P and process blocks as usual. *)
  let target_level head_level = Int32.(sub head_level 2l) in
  let first_level_for_skip_list_storage period level =
    (* Note that behind this first level we do not have
       the plugin. *)
    Int32.(sub level (of_int period))
  in
  let should_store_skip_list_cells ~head_level ~level =
    let profile_ctxt = Node_context.get_profile_ctxt ctxt in
    let period =
      get_storage_period
        profile_ctxt
        proto_parameters
        head_level
        first_seen_level
      + skip_list_offset proto_parameters
    in
    supports_refutations
    && level >= first_level_for_skip_list_storage period head_level
  in
  let rec do_clean_up last_processed_level head_level =
    let last_level = target_level head_level in
    let rec clean_up_at_level level =
      if level > last_level then return_unit
      else
        let*! () =
          Handler.remove_old_level_stored_data proto_parameters ctxt level
        in
        let* () =
          if should_store_skip_list_cells ~head_level ~level then
            store_skip_list_cells ~level
          else return_unit
        in
        let* () =
          Store.Last_processed_level.save store.last_processed_level level
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
    else return_unit
  in
  do_clean_up last_processed_level head_level

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
  let*! () = Event.(emit starting_node) () in
  let* ({
          network_name;
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
        let*! () = Event.(emit data_dir_not_found data_dir) in
        (* Store the default configuration if no configuration were found. *)
        let configuration = configuration_override Configuration_file.default in
        let* () = Configuration_file.save configuration in
        return configuration
  in
  let*! () = Event.(emit configuration_loaded) () in

  let cctxt = Rpc_context.make endpoint in
  let* dal_config = fetch_dal_config cctxt in
  (* Resolve:
     - [points] from DAL node config file and CLI.
     - [dal_config.bootstrap_peers] from the L1 network config. *)
  let* points = resolve (points @ dal_config.bootstrap_peers) in
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
      if Profile_manager.is_bootstrap_profile profile then
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
    let gs_worker =
      Gossipsub.Worker.(
        make
          ~bootstrap_points:points
          ~events_logging:Logging.event
          rng
          limits
          peer_filter_parameters)
    in
    Gossipsub.Worker.start [] gs_worker ;
    gs_worker
  in
  (* Create a transport (P2P) layer instance. *)
  let* transport_layer =
    let open Transport_layer_parameters in
    let* p2p_config = p2p_config config in
    Gossipsub.Transport_layer.create
      ~public_addr
      ~is_bootstrap_peer:(profile = Profile_manager.bootstrap)
      p2p_config
      p2p_limits
      ~network_name
  in
  (* Initialize store *)
  let* store = Store.init config in
  let* last_processed_level =
    Store.Last_processed_level.load store.last_processed_level
  in
  let* first_seen_level = Store.First_seen_level.load store.first_seen_level in
  (* Get the current L1 head and its DAL plugin and parameters. *)
  let* header = Shell_services.Blocks.Header.shell_header cctxt () in
  let head_level = header.Block_header.level in
  let* (module Plugin : Dal_plugin.T) =
    Proto_plugins.resolve_plugin_for_level cctxt ~level:head_level
  in
  let proto_plugins =
    Proto_plugins.singleton
      ~first_level:head_level
      ~proto_level:header.proto_level
      (module Plugin)
  in
  let* proto_parameters =
    Plugin.get_constants `Main (`Level head_level) cctxt
  in
  let* profile_ctxt = build_profile_context config in
  let*? () =
    Profile_manager.validate_slot_indexes
      profile_ctxt
      ~number_of_slots:proto_parameters.number_of_slots
  in
  (* Check the DAL node's and L1 node's history mode. *)
  let* () = check_history_mode config profile_ctxt proto_parameters in
  let* () =
    match first_seen_level with
    | None -> Store.First_seen_level.save store.first_seen_level head_level
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
    init_cryptobox config proto_parameters
  in
  let ctxt =
    Node_context.init
      config
      profile_ctxt
      cryptobox
      shards_proofs_precomputation
      proto_parameters
      proto_plugins
      store
      gs_worker
      transport_layer
      cctxt
  in
  let is_prover_profile = Profile_manager.is_prover_profile profile_ctxt in
  (* Initialize amplificator if in prover profile.
     This forks a process and should be kept early to avoid copying opened file
     descriptors. *)
  let* amplificator =
    if not is_prover_profile then return_none
    else
      let* amplificator = Amplificator.make ctxt in
      return_some amplificator
  in
  (* Starts the metrics *after* the amplificator fork, to avoid forked opened
     sockets *)
  let*! _metrics_server = Metrics.launch config.metrics_addr in
  (* Set value size hooks. *)
  Value_size_hooks.set_share_size
    (Cryptobox.Internal_for_tests.encoded_share_size cryptobox) ;
  Value_size_hooks.set_number_of_slots proto_parameters.number_of_slots ;
  (* Start RPC server. We do that before the waiting for the L1 node to be
     bootstrapped so that queries can already be issued. Note that that the node
     will thus already respond to the baker about shards status if queried. *)
  let* rpc_server = RPC_server.(start config ctxt) in
  let _ = RPC_server.install_finalizer rpc_server in
  let*! () = Event.(emit rpc_server_is_ready rpc_addr) in
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
        clean_up_store
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
      Store.Last_processed_level.load store.last_processed_level
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
  connect_gossipsub_with_p2p gs_worker transport_layer store ctxt amplificator ;
  let*! () =
    Gossipsub.Transport_layer.activate ~additional_points:points transport_layer
  in
  let*! () = Event.(emit p2p_server_is_ready listen_addr) in
  (* Start collecting stats related to the Gossipsub worker. *)
  Dal_metrics.collect_gossipsub_metrics gs_worker ;
  (* Register topics with gossipsub worker. *)
  let* () = update_and_register_profiles ctxt in
  (* Start never-ending monitoring daemons *)
  let*! () = Event.(emit node_is_ready ()) in
  let* () = daemonize [Handler.new_finalized_head ctxt cctxt crawler] in
  return_unit

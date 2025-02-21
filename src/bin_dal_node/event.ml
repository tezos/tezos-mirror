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

open Internal_event.Simple

(* DAL node event definitions *)

open struct
  let section = ["dal"; "node"]

  let starting_node =
    declare_0
      ~section
      ~name:"starting_dal_node"
      ~msg:"starting the DAL node"
      ~level:Notice
      ()

  let waiting_l1_node_bootstrapped =
    declare_0
      ~section
      ~name:"waiting_l1_node_to_be_bootstrapped"
      ~msg:"waiting for the L1 node to be bootstrapped"
      ~level:Notice
      ()

  let l1_node_bootstrapped =
    declare_0
      ~section
      ~name:"l1_node_is_bootstrapped"
      ~msg:"the L1 node is bootstrapped"
      ~level:Notice
      ()

  let waiting_known_plugin =
    declare_0
      ~section
      ~name:"waiting_l1_node_known_plugin"
      ~msg:"waiting for a block with a known protocol plugin"
      ~level:Notice
      ()

  let shutdown_node =
    declare_1
      ~section
      ~name:"stopping_dal_node"
      ~msg:"stopping DAL node"
      ~level:Notice
      ("exit_status", Data_encoding.int8)

  let dal_node_sqlite3_store_init =
    declare_0
      ~section
      ~name:"dal_node_sqlite3_store_init"
      ~msg:"initializing the SQLite3 store"
      ~level:Info
      ()

  let store_is_ready =
    declare_0
      ~section
      ~name:"dal_node_store_is_ready"
      ~msg:"the DAL node store is ready"
      ~level:Notice
      ()

  let node_is_ready =
    declare_0
      ~section
      ~name:"dal_node_is_ready"
      ~msg:"the DAL node is ready"
      ~level:Notice
      ()

  let data_dir_not_found =
    declare_1
      ~section
      ~name:"dal_node_no_data_dir"
      ~msg:
        "the DAL node configuration file does not exist in {path}, creating one"
      ~level:Warning
      ("path", Data_encoding.(string))

  let retry_fetching_node_config level prefix =
    declare_2
      ~section
      ~name:(prefix ^ "retry_fetching_config")
      ~msg:
        "cannot fetch config from l1 node at {endpoint}, retrying in {delay}s"
      ~level
      ("endpoint", Data_encoding.string)
      ("delay", Data_encoding.float)

  let retry_fetching_node_config_notice =
    retry_fetching_node_config Internal_event.Notice "notice"

  let retry_fetching_node_config_warning =
    retry_fetching_node_config Internal_event.Warning "warning"

  let config_error_no_bootstrap =
    declare_0
      ~section
      ~name:"config_error_no_bootstrap"
      ~msg:
        "no bootstrap peers found in the configuration file or network settings"
      ~level:Error
      ()

  let resolved_bootstrap_points =
    declare_2
      ~section
      ~name:"resolved_bootstrap_points"
      ~msg:
        "DNS resolution of {domainname} returned {number} bootstrap IP \
         addresses"
      ~level:Notice
      ("domainname", Data_encoding.string)
      ("number", Data_encoding.int31)

  let resolved_bootstrap_no_points =
    declare_0
      ~section
      ~name:"resolved_bootstrap_no_points"
      ~msg:"DNS resolution returned no bootstrap IP address"
      ~level:Error
      ()

  let resolved_bootstrap_points_total =
    declare_1
      ~section
      ~name:"resolved_bootstrap_points_total"
      ~msg:"DNS resolution returned a total of {number} bootstrap IP addresses"
      ~level:Notice
      ("number", Data_encoding.int31)

  let fetched_config_success =
    declare_1
      ~section
      ~name:"fetched_config_success"
      ~msg:"success fetching config from l1 node at {endpoint}"
      ~level:Notice
      ("endpoint", Data_encoding.string)

  let failed_to_persist_profiles =
    declare_2
      ~section
      ~name:"failed_to_persist_profiles"
      ~msg:"failed to persist the profiles to the config file"
      ~level:Error
      ("profiles", Types.profile_encoding)
      ("error", Error_monad.trace_encoding)

  let fetched_slot =
    declare_2
      ~section
      ~name:"reconstructed_slot"
      ~msg:"reconstructed slot: size {size}, shards {shards}"
      ~level:Info
      ("size", Data_encoding.int31)
      ("shards", Data_encoding.int31)

  let layer1_node_new_head =
    declare_3
      ~section
      ~name:"dal_node_layer_1_new_head"
      ~msg:
        "head of Layer 1 node updated to {hash} at level {level}, fitness \
         {fitness}"
      ~level:Info
      ("hash", Block_hash.encoding)
      ("level", Data_encoding.int32)
      ("fitness", Fitness.encoding)

  let layer1_node_final_block =
    declare_2
      ~section
      ~name:"dal_node_layer_1_new_final_block"
      ~msg:"layer 1 node's block at level {level}, round {round} is final"
      ~level:Notice
      ("level", Data_encoding.int32)
      ("round", Data_encoding.int32)

  let layer1_node_tracking_started =
    declare_0
      ~section
      ~name:"dal_node_layer_1_start_tracking"
      ~msg:"started tracking layer 1's node"
      ~level:Notice
      ()

  let protocol_plugin_resolved =
    declare_1
      ~section
      ~name:"dal_node_plugin_resolved"
      ~msg:"resolved plugin on protocol {proto_hash}"
      ~level:Notice
      ~pp1:Protocol_hash.pp_short
      ("proto_hash", Protocol_hash.encoding)

  let no_protocol_plugin =
    declare_1
      ~section
      ~name:"dal_node_no_plugin"
      ~msg:"could not resolve plugin for protocol {proto_hash}"
      ~level:Error
      ~pp1:Protocol_hash.pp_short
      ("proto_hash", Protocol_hash.encoding)

  let unexpected_protocol_plugin =
    declare_0
      ~section
      ~name:"dal_node_unexpected_plugin"
      ~msg:
        "found plugin for the current protocol, expected one for the next \
         protocol."
      ~level:Error
      ()

  let daemon_error =
    declare_1
      ~section
      ~name:"dal_node_daemon_error"
      ~msg:"daemon thrown an error: {error}"
      ~level:Notice
      ~pp1:Error_monad.pp_print_trace
      ("error", Error_monad.trace_encoding)

  let failed_to_fetch_block =
    declare_4
      ~section
      ~name:"dal_node_crawler_failed_to_fetch_header"
      ~msg:
        "the crawler failed to fetch the block {type} at level {level} (for \
         last_notified_level {last_notified}): {error}\n\
         If you're a rollup producer or observer, you may be not be able to \
         defend your rollup commitments involving DAL inputs in a refutation \
         game."
      ~level:Warning
      ~pp4:Error_monad.pp_print_trace
      ("type", Data_encoding.string)
      ("level", Data_encoding.int32)
      ("last_notified", Data_encoding.int32)
      ("error", Error_monad.trace_encoding)

  let history_mode_warning =
    declare_2
      ~section
      ~name:"dal_node_history_mode_warning"
      ~msg:
        "The node will only store data related to the last {stored_levels} \
         levels, but it should store data for {storage_period} levels in order \
         to be able to participate in refutation games"
      ~level:Warning
      ("stored_levels", Data_encoding.int31)
      ("storage_period", Data_encoding.int31)

  let configuration_loaded =
    declare_0
      ~section
      ~name:"configuration_loaded"
      ~msg:"configuration loaded successfully"
      ~level:Notice
      ()

  let stored_slot_content =
    declare_2
      ~section
      ~name:"stored_slot_content"
      ~msg:"stored slot for level {published_level} and index {slot_index}"
      ~level:Info
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)

  let stored_slot_shard =
    declare_3
      ~section
      ~name:"stored_slot_shard"
      ~msg:
        "stored shard {shard_index} for level {published_level} and index \
         {slot_index}"
      ~level:Debug
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("shard_index", Data_encoding.int31)

  let stored_slot_status =
    declare_3
      ~section
      ~name:"stored_slot_status"
      ~msg:
        "stored slot status for level {published_level} and index \
         {slot_index}: {status}"
      ~level:Debug
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("status", Types.header_status_encoding)

  let removed_slot_shards =
    declare_2
      ~section
      ~name:"removed_slot_shards"
      ~msg:"removed shards for level {published_level} and index {slot_index}"
      ~level:Debug
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)

  let removed_slot =
    declare_2
      ~section
      ~name:"removed_slot"
      ~msg:"removed slot for level {published_level} and index {slot_index}"
      ~level:Debug
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)

  let removed_status =
    declare_1
      ~section
      ~name:"removed_status"
      ~msg:"removed statuses for level {level}"
      ~level:Debug
      ("level", Data_encoding.int32)

  let slot_header_status_storage_error =
    declare_3
      ~section
      ~name:"slot_header_status_storage_error"
      ~msg:
        "slot header status storage error for level {published_level}, slot \
         index {slot_index}: {error}"
      ~level:Error
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("error", Error_monad.trace_encoding)

  let unexpected_slot_header_status =
    declare_4
      ~section
      ~name:"unexpected_slot_header_status"
      ~msg:
        "Internal error: unexpected slot header status {got_status}, expected \
         {expected_status}, for level {published_level}, slot index \
         {slot_index}"
      ~level:Error
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("expected_status", Types.header_status_encoding)
      ("got_status", Types.header_status_encoding)
      ~pp3:Types.pp_header_status
      ~pp4:Types.pp_header_status

  let removed_skip_list_cells =
    declare_1
      ~section
      ~name:"removed_skip_list_cells"
      ~msg:"removed skip list cells for level {level}"
      ~level:Debug
      ("level", Data_encoding.int32)

  let removing_shards_failed =
    declare_3
      ~section
      ~name:"removing_shards_failed"
      ~level:Warning
      ~msg:
        "removing shards for level {published_level} and index {slot_index} \
         failed: {error}"
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("error", Error_monad.trace_encoding)

  let removing_slot_failed =
    declare_3
      ~section
      ~name:"removing_slot_failed"
      ~level:Warning
      ~msg:
        "removing slot for level {published_level} and index {slot_index} \
         failed: {error}"
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("error", Error_monad.trace_encoding)

  let removing_status_failed =
    declare_2
      ~section
      ~name:"removing_status_failed"
      ~level:Warning
      ~msg:"removing status file for level {level} failed: {error}"
      ("level", Data_encoding.int32)
      ("error", Error_monad.trace_encoding)

  let removing_skip_list_cells_failed =
    declare_2
      ~section
      ~name:"removing_skip_list_cells_failed"
      ~level:Warning
      ~msg:"removing skip list cells for level {level} failed: {error}"
      ("level", Data_encoding.int32)
      ("error", Error_monad.trace_encoding)

  let decoding_data_failed =
    declare_1
      ~section
      ~name:"decoding_failed"
      ~msg:"error while decoding a {data_kind} value"
      ~level:Warning
      ("data_kind", Types.Store.encoding)

  let message_validation_error =
    declare_2
      ~section
      ~name:"message_validation_failed"
      ~msg:
        "validating message with id {message_id} failed with error \
         {validation_error}"
      ~level:Warning
      ~pp1:Gossipsub.Worker.GS.Message_id.pp
      ("message_id", Types.Message_id.encoding)
      ("validation_error", Data_encoding.string)

  let p2p_server_is_ready =
    declare_1
      ~section
      ~name:"dal_node_p2p_server_is_ready"
      ~msg:"P2P server is listening on {point}"
      ~level:Notice
      ("point", P2p_point.Id.encoding)

  let rpc_server_is_ready =
    declare_1
      ~section
      ~name:"dal_node_rpc_server_is_ready"
      ~msg:"RPC server is listening on {point}"
      ~level:Notice
      ("point", P2p_point.Id.encoding)

  let metrics_server_starting =
    declare_1
      ~section:(section @ ["metrics"])
      ~name:"metrics_service_start"
      ~msg:"Starting metrics service at {endpoint}"
      ~level:Notice
      ("endpoint", P2p_point.Id.encoding)

  let metrics_server_not_starting =
    declare_0
      ~section:(section @ ["metrics"])
      ~name:"metrics_service_no_start"
      ~msg:"metrics service not enabled"
      ~level:Notice
      ()

  let metrics_server_is_ready =
    let open Internal_event.Simple in
    declare_2
      ~section
      ~name:"starting_metrics_server"
      ~msg:"metrics server is listening on {host}:{port}"
      ~level:Notice
      ("host", Data_encoding.string)
      ("port", Data_encoding.uint16)

  let loading_profiles_failed =
    declare_1
      ~section
      ~name:"loading_profiles_failed"
      ~msg:"loading profiles failed: {error}"
      ~level:Info
      ("error", Error_monad.trace_encoding)

  let saving_profiles_failed =
    declare_1
      ~section
      ~name:"saving_profiles_failed"
      ~msg:"saving profiles failed: {error}"
      ~level:Error
      ("error", Error_monad.trace_encoding)

  let reconstruct_starting_in =
    declare_3
      ~section
      ~name:"reconstruct_starting_in"
      ~msg:
        "For the level {level} and slot index {slot_index}, enough shards have \
         been received to reconstruct the slot. If the remaining shards are \
         not received in {delay} seconds, they will be reconstructed."
      ~level:Info
      ~pp1:(fun fmt -> Format.fprintf fmt "%ld")
      ("level", Data_encoding.int32)
      ~pp2:Format.pp_print_int
      ("slot_index", Data_encoding.int31)
      ~pp3:Format.pp_print_float
      ("delay", Data_encoding.float)

  let reconstruct_started =
    declare_4
      ~section
      ~name:"reconstruct_started"
      ~msg:
        "For the level {level} and slot index {slot_index}, \
         {number_of_received_shards} out of {number_of_shards} shards were \
         received, starting a reconstruction of the missing shards."
      ~level:Notice
      ~pp1:(fun fmt -> Format.fprintf fmt "%ld")
      ("level", Data_encoding.int32)
      ~pp2:Format.pp_print_int
      ("slot_index", Data_encoding.int31)
      ~pp3:Format.pp_print_int
      ("number_of_received_shards", Data_encoding.int31)
      ~pp4:Format.pp_print_int
      ("number_of_shards", Data_encoding.int31)

  let reconstruct_finished =
    declare_2
      ~section
      ~name:"reconstruct_finished"
      ~msg:
        "For the level {level} and slot index {slot_index}, missing shards \
         have been successfully reconstructed."
      ~level:Notice
      ~pp1:(fun fmt -> Format.fprintf fmt "%ld")
      ("level", Data_encoding.int32)
      ~pp2:Format.pp_print_int
      ("slot_index", Data_encoding.int31)

  let reconstruct_no_missing_shard =
    declare_2
      ~section
      ~name:"reconstruct_no_missing_shard"
      ~msg:
        "For the level {level}, all shards for slot index {slot_index} were \
         received. The planned reconstruction has been cancelled."
      ~level:Info
      ~pp1:(fun fmt -> Format.fprintf fmt "%ld")
      ("level", Data_encoding.int32)
      ~pp2:Format.pp_print_int
      ("slot_index", Data_encoding.int31)

  let reconstruct_error =
    declare_3
      ~section
      ~name:"reconstruct_error"
      ~msg:
        "For the level {level} and slot index {slot_index}, unexpected error \
         during reconstruction: {error}."
      ~level:Error
      ~pp1:(fun fmt -> Format.fprintf fmt "%ld")
      ("level", Data_encoding.int32)
      ~pp2:Format.pp_print_int
      ("slot_index", Data_encoding.int31)
      ("error", Error_monad.trace_encoding)

  let store_upgrade_error_moving_directory =
    declare_3
      ~section
      ~name:"store_upgrade_error_moving_directory"
      ~msg:"There was an error trying to move {src} to {dst}: {exn}"
      ~level:Warning
      ("src", Data_encoding.string)
      ("dst", Data_encoding.string)
      ("exn", Data_encoding.string)

  let store_upgrade_error_creating_directory =
    declare_2
      ~section
      ~name:"store_upgrade_error_creating_directory"
      ~msg:"There was an error trying to create directory {path}: {exn}"
      ~level:Warning
      ("path", Data_encoding.string)
      ("exn", Data_encoding.string)

  let store_upgrade_start =
    declare_2
      ~section
      ~name:"store_upgrading"
      ~msg:
        "starting to upgrade the store from version {old_version} to \
         {new_version}"
      ~level:Notice
      ("old_version", Data_encoding.int31)
      ("new_version", Data_encoding.int31)

  let store_upgraded =
    declare_2
      ~section
      ~name:"store_upgraded"
      ~msg:
        "the store has been upgraded from version {old_version} to \
         {new_version}"
      ~level:Notice
      ("old_version", Data_encoding.int31)
      ("new_version", Data_encoding.int31)

  let store_upgrade_error =
    declare_0
      ~section
      ~name:"store_upgrade_error"
      ~msg:"Failed to upgrade the store."
      ~level:Error
      ()

  let crypto_process_started =
    declare_1
      ~section:(section @ ["crypto"])
      ~name:"crypto_process_started"
      ~msg:"cryptographic child process started (pid: {pid})"
      ~level:Notice
      ("pid", Data_encoding.int31)

  let amplificator_uninitialized =
    declare_0
      ~section:(section @ ["crypto"])
      ~name:"amplificator_uninitialized"
      ~msg:"the amplificator process worker is not initialized"
      ~level:Warning
      ()

  let crypto_process_received_query =
    declare_1
      ~section:(section @ ["crypto"])
      ~name:"crypto_process_received_query"
      ~msg:"cryptographic child process: received query #{query_id}."
      ~level:Notice
      ("query_id", Data_encoding.int31)

  let crypto_process_sending_reply =
    declare_1
      ~section:(section @ ["crypto"])
      ~name:"crypto_process_sending_reply"
      ~msg:"cryptographic child process: sending reply #{query_id}."
      ~level:Info
      ("query_id", Data_encoding.int31)

  let main_process_sending_query =
    declare_1
      ~section:(section @ ["crypto"])
      ~name:"main_process_sending_query"
      ~msg:
        "main process: sending query #{query_id} to cryptographic child \
         process."
      ~level:Info
      ("query_id", Data_encoding.int31)

  let main_process_received_reply =
    declare_1
      ~section:(section @ ["crypto"])
      ~name:"main_process_received_reply"
      ~msg:"main process: received reply #{query_id}."
      ~level:Info
      ("query_id", Data_encoding.int31)

  let main_process_enqueue_query =
    declare_1
      ~section:(section @ ["crypto"])
      ~name:"main_process_enqueue_query"
      ~msg:"main process: enqueue query #{query_id}."
      ~level:Info
      ("query_id", Data_encoding.int31)

  let pp_int_list fmt l =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
      Format.pp_print_int
      fmt
      l

  let get_attestable_slots_ok_notice =
    declare_3
      ~section
      ~name:"get_attestable_slots_ok_notice"
      ~msg:
        "For slots {slots_indices} published at level {published_level}, \
         {attester} got all its shards."
      ~level:Notice
      ("attester", Signature.Public_key_hash.encoding)
      ("published_level", Data_encoding.int32)
      ("slots_indices", Data_encoding.(list int31))
      ~pp1:Signature.Public_key_hash.pp_short
      ~pp3:pp_int_list

  let get_attestable_slots_not_ok_warning =
    declare_4
      ~section
      ~name:"get_attestable_slots_missing_shards_warning"
      ~msg:
        "For slots {slots_indices} published at level {published_level}, \
         {attester} missed shards:\n\
         {slot_indexes_with_details}"
      ~level:Warning
      ("attester", Signature.Public_key_hash.encoding)
      ("published_level", Data_encoding.int32)
      ("slots_indices", Data_encoding.(list int31))
      ( "slot_indexes_with_details",
        Data_encoding.(list (tup3 int31 int31 int31)) )
      ~pp1:Signature.Public_key_hash.pp_short
      ~pp3:pp_int_list
      ~pp4:
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n")
           (fun fmt (slot_index, stored_shards, expected_shards) ->
             Format.fprintf
               fmt
               " For slot index %d, %d shards out of %d were received"
               slot_index
               stored_shards
               expected_shards))

  let get_attestable_slots_future_level_warning =
    declare_2
      ~section
      ~name:"get_attestable_slots_future_level_warning"
      ~msg:
        "It looks like the DAL node is lagging (its current level is \
         {current_level}, while the Layer1 node's level is \
         {current_baker_level})."
      ~level:Warning
      ("current_level", Data_encoding.int32)
      ("current_baker_level", Data_encoding.int32)

  let warn_attester_not_dal_attesting =
    declare_2
      ~section
      ~name:"attester_not_dal_attesting"
      ~msg:
        "No DAL content was included by {attester} for attested level \
         {attested_level}"
      ~level:Warning
      ("attester", Signature.Public_key_hash.encoding)
      ("attested_level", Data_encoding.int32)
      ~pp1:Signature.Public_key_hash.pp_short

  let warn_attester_did_not_attest_slot =
    declare_3
      ~section
      ~name:"attester_did_not_attest_slot"
      ~msg:
        "At level {attested_level}, slot index {slot_index} was sufficiently \
         attested, but shards from {attester} are missing"
      ~level:Warning
      ("attester", Signature.Public_key_hash.encoding)
      ("slot_index", Data_encoding.int31)
      ("attested_level", Data_encoding.int32)
      ~pp1:Signature.Public_key_hash.pp_short

  let attester_did_not_attest_slot_because_of_traps =
    declare_3
      ~section
      ~name:"attester_did_not_attest_slot_with_traps"
      ~msg:
        "At level {attested_level}, slot index {slot_index} was sufficiently \
         attested, but {attester} did not attest because of traps"
      ~level:Notice
      ("attester", Signature.Public_key_hash.encoding)
      ("slot_index", Data_encoding.int31)
      ("attested_level", Data_encoding.int32)
      ~pp1:Signature.Public_key_hash.pp_short

  let trap_injection =
    declare_5
      ~section
      ~name:"trap_injection"
      ~msg:
        "Injecting entrapment evidence for delegate {delegate}, published \
         level {published_level}, attested level {attested_level}, slot index \
         {slot_index}, shard index {shard_index}"
      ~level:Notice
      ("delegate", Signature.Public_key_hash.encoding)
      ("published_level", Data_encoding.int32)
      ("attested_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("shard_index", Data_encoding.int31)

  let trap_injection_failure =
    declare_6
      ~section
      ~name:"trap_injection_failure"
      ~msg:
        "Failed to inject an entrapment evidence for delegate {delegate}, \
         published level {published_level}, attested level {attested_level}, \
         slot index {slot_index}, shard index {shard_index}: {error}"
      ~level:Warning
      ~pp6:pp_print_trace
      ("delegate", Signature.Public_key_hash.encoding)
      ("published_level", Data_encoding.int32)
      ("attested_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("shard_index", Data_encoding.int31)
      ("error", trace_encoding)

  let trap_check_failure =
    declare_3
      ~section
      ~name:"trap_check_failure"
      ~msg:
        "An error occurred when checking the trap for published level \
         {published_level}, slot index {slot_index}, shard index {shard_index}"
      ~level:Warning
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("shard_index", Data_encoding.int31)

  let trap_registration_fail =
    declare_3
      ~section
      ~name:"trap_registration_fail"
      ~msg:
        "An error occurred when checking if the shard for delegate {delegate}, \
         slot index {slot_index} and shard index {shard_index} is a trap"
      ~level:Warning
      ("delegate", Signature.Public_key_hash.encoding)
      ("slot_index", Data_encoding.int31)
      ("shard_index", Data_encoding.int31)

  let trap_delegate_attestation_not_found =
    declare_5
      ~section
      ~name:"trap_delegate_attestation_not_found"
      ~msg:
        "Unable to associate an attestation with delegate {delegate} for \
         attested level {attested_level}. Failed while injecting trap evidence \
         from published level {published_level} at slot index {slot_index} and \
         shard index {shard_index}"
      ~level:Warning
      ("delegate", Signature.Public_key_hash.encoding)
      ("slot_index", Data_encoding.int31)
      ("shard_index", Data_encoding.int31)
      ("published_level", Data_encoding.int32)
      ("attested_level", Data_encoding.int32)

  let registered_pkh_not_a_delegate =
    declare_1
      ~section
      ~name:"register_pkh_not_a_delegate"
      ~msg:
        "The public key hash {pkh} registered by PATCH /profiles is not a \
         delegate."
      ~level:Warning
      ("pkh", Signature.Public_key_hash.encoding)

  let cannot_attest_slot_because_of_trap =
    declare_4
      ~section
      ~name:"slot_contains_trap"
      ~msg:
        "{delegate} cannot attest slot {slot_index} at published level \
         {published_level}: shard {shard_index} is a trap."
      ~level:Notice
      ("delegate", Signature.Public_key_hash.encoding)
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("shard_index", Data_encoding.int31)

  let register_trap =
    declare_4
      ~section
      ~name:"register_trap"
      ~msg:
        "The shard {shard_index} for slot {slot_index} and published level \
         {published_level} is a trap for {delegate}."
      ~level:Info
      ("delegate", Signature.Public_key_hash.encoding)
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("shard_index", Data_encoding.int31)
end

(* DAL node event emission functions *)

let emit_starting_node () = emit starting_node ()

let emit_waiting_l1_node_bootstrapped () = emit waiting_l1_node_bootstrapped ()

let emit_l1_node_bootstrapped () = emit l1_node_bootstrapped ()

let emit_waiting_known_plugin () = emit waiting_known_plugin ()

let emit_shutdown_node ~exit_status = emit shutdown_node exit_status

let emit_dal_node_sqlite3_store_init () = emit dal_node_sqlite3_store_init ()

let emit_store_is_ready () = emit store_is_ready ()

let emit_node_is_ready () = emit node_is_ready ()

let emit_data_dir_not_found ~path = emit data_dir_not_found path

let emit_retry_fetching_node_config_notice ~endpoint ~delay =
  emit retry_fetching_node_config_notice (endpoint, delay)

let emit_retry_fetching_node_config_warning ~endpoint ~delay =
  emit retry_fetching_node_config_warning (endpoint, delay)

let emit_config_error_no_bootstrap () = emit config_error_no_bootstrap ()

let emit_resolved_bootstrap_points ~domainname ~number =
  emit resolved_bootstrap_points (domainname, number)

let emit_resolved_bootstrap_no_points () = emit resolved_bootstrap_no_points ()

let emit_resolved_bootstrap_points_total ~number =
  emit resolved_bootstrap_points_total number

let emit_fetched_config_success ~endpoint = emit fetched_config_success endpoint

let emit_failed_to_persist_profiles ~profiles ~error =
  emit failed_to_persist_profiles (profiles, error)

let emit_fetched_slot ~size ~shards = emit fetched_slot (size, shards)

let emit_layer1_node_new_head ~hash ~level ~fitness =
  emit layer1_node_new_head (hash, level, fitness)

let emit_layer1_node_final_block ~level ~round =
  emit layer1_node_final_block (level, round)

let emit_layer1_node_tracking_started () = emit layer1_node_tracking_started ()

let emit_protocol_plugin_resolved ~proto_hash =
  emit protocol_plugin_resolved proto_hash

let emit_no_protocol_plugin ~proto_hash = emit no_protocol_plugin proto_hash

let emit_unexpected_protocol_plugin () = emit unexpected_protocol_plugin ()

let emit_daemon_error ~error = emit daemon_error error

let emit_failed_to_fetch_block ~type_ ~level ~last_notified ~error =
  emit failed_to_fetch_block (type_, level, last_notified, error)

let emit_history_mode_warning ~stored_levels ~storage_period =
  emit history_mode_warning (stored_levels, storage_period)

let emit_configuration_loaded () = emit configuration_loaded ()

let emit_stored_slot_content ~published_level ~slot_index =
  emit stored_slot_content (published_level, slot_index)

let emit_stored_slot_shard ~published_level ~slot_index ~shard_index =
  emit stored_slot_shard (published_level, slot_index, shard_index)

let emit_stored_slot_status ~published_level ~slot_index ~status =
  emit stored_slot_status (published_level, slot_index, status)

let emit_removed_slot_shards ~published_level ~slot_index =
  emit removed_slot_shards (published_level, slot_index)

let emit_removed_slot ~published_level ~slot_index =
  emit removed_slot (published_level, slot_index)

let emit_removed_status ~level = emit removed_status level

let emit_slot_header_status_storage_error ~published_level ~slot_index ~error =
  emit slot_header_status_storage_error (published_level, slot_index, error)

let emit_unexpected_slot_header_status ~published_level ~slot_index
    ~expected_status ~got_status =
  emit
    unexpected_slot_header_status
    (published_level, slot_index, expected_status, got_status)

let emit_removed_skip_list_cells ~level = emit removed_skip_list_cells level

let emit_removing_shards_failed ~published_level ~slot_index ~error =
  emit removing_shards_failed (published_level, slot_index, error)

let emit_removing_slot_failed ~published_level ~slot_index ~error =
  emit removing_slot_failed (published_level, slot_index, error)

let emit_removing_status_failed ~level ~error =
  emit removing_status_failed (level, error)

let emit_removing_skip_list_cells_failed ~level ~error =
  emit removing_skip_list_cells_failed (level, error)

let emit_decoding_data_failed ~data_kind = emit decoding_data_failed data_kind

let emit_dont_wait__message_validation_error ~message_id ~validation_error =
  emit__dont_wait__use_with_care
    message_validation_error
    (message_id, validation_error)

let emit_p2p_server_is_ready ~point = emit p2p_server_is_ready point

let emit_rpc_server_is_ready ~point = emit rpc_server_is_ready point

let emit_metrics_server_starting ~endpoint =
  emit metrics_server_starting endpoint

let emit_metrics_server_not_starting () = emit metrics_server_not_starting ()

let emit_metrics_server_is_ready ~host ~port =
  emit metrics_server_is_ready (host, port)

let emit_loading_profiles_failed ~error = emit loading_profiles_failed error

let emit_saving_profiles_failed ~error = emit saving_profiles_failed error

let emit_reconstruct_starting_in ~level ~slot_index ~delay =
  emit reconstruct_starting_in (level, slot_index, delay)

let emit_reconstruct_started ~level ~slot_index ~number_of_received_shards
    ~number_of_shards =
  emit
    reconstruct_started
    (level, slot_index, number_of_received_shards, number_of_shards)

let emit_reconstruct_finished ~level ~slot_index =
  emit reconstruct_finished (level, slot_index)

let emit_reconstruct_no_missing_shard ~level ~slot_index =
  emit reconstruct_no_missing_shard (level, slot_index)

let emit_reconstruct_error ~level ~slot_index ~error =
  emit reconstruct_error (level, slot_index, error)

let emit_store_upgrade_error_moving_directory ~src ~dst ~exn =
  emit store_upgrade_error_moving_directory (src, dst, exn)

let emit_store_upgrade_error_creating_directory ~path ~exn =
  emit store_upgrade_error_creating_directory (path, exn)

let emit_store_upgrade_start ~old_version ~new_version =
  emit store_upgrade_start (old_version, new_version)

let emit_store_upgraded ~old_version ~new_version =
  emit store_upgraded (old_version, new_version)

let emit_store_upgrade_error () = emit store_upgrade_error ()

let emit_crypto_process_started ~pid = emit crypto_process_started pid

let emit_amplificator_uninitialized () = emit amplificator_uninitialized ()

let emit_crypto_process_received_query ~query_id =
  emit crypto_process_received_query query_id

let emit_crypto_process_sending_reply ~query_id =
  emit crypto_process_sending_reply query_id

let emit_main_process_sending_query ~query_id =
  emit main_process_sending_query query_id

let emit_main_process_received_reply ~query_id =
  emit main_process_received_reply query_id

let emit_main_process_enqueue_query ~query_id =
  emit main_process_enqueue_query query_id

let emit_get_attestable_slots_ok_notice ~attester ~published_level
    ~slots_indices =
  emit get_attestable_slots_ok_notice (attester, published_level, slots_indices)

let emit_get_attestable_slots_not_ok_warning ~attester ~published_level
    ~slots_indices ~slot_indexes_with_details =
  emit
    get_attestable_slots_not_ok_warning
    (attester, published_level, slots_indices, slot_indexes_with_details)

let emit_get_attestable_slots_future_level_warning ~current_level
    ~current_baker_level =
  emit
    get_attestable_slots_future_level_warning
    (current_level, current_baker_level)

let emit_warn_attester_not_dal_attesting ~attester ~attested_level =
  emit warn_attester_not_dal_attesting (attester, attested_level)

let emit_warn_attester_did_not_attest_slot ~attester ~slot_index ~attested_level
    =
  emit warn_attester_did_not_attest_slot (attester, slot_index, attested_level)

let emit_attester_did_not_attest_slot_because_of_traps ~attester ~slot_index
    ~attested_level =
  emit
    attester_did_not_attest_slot_because_of_traps
    (attester, slot_index, attested_level)

let emit_trap_injection ~delegate ~published_level ~attested_level ~slot_index
    ~shard_index =
  emit
    trap_injection
    (delegate, published_level, attested_level, slot_index, shard_index)

let emit_trap_injection_failure ~delegate ~published_level ~attested_level
    ~slot_index ~shard_index ~error =
  emit
    trap_injection_failure
    (delegate, published_level, attested_level, slot_index, shard_index, error)

let emit_trap_check_failure ~published_level ~slot_index ~shard_index =
  emit trap_check_failure (published_level, slot_index, shard_index)

let emit_dont_wait__trap_registration_fail ~delegate ~slot_index ~shard_index =
  emit__dont_wait__use_with_care
    trap_registration_fail
    (delegate, slot_index, shard_index)

let emit_trap_delegate_attestation_not_found ~delegate ~slot_index ~shard_index
    ~published_level ~attested_level =
  emit
    trap_delegate_attestation_not_found
    (delegate, slot_index, shard_index, published_level, attested_level)

let emit_registered_pkh_not_a_delegate ~pkh =
  emit registered_pkh_not_a_delegate pkh

let emit_cannot_attest_slot_because_of_trap ~pkh ~published_level ~slot_index
    ~shard_index =
  emit
    cannot_attest_slot_because_of_trap
    (pkh, published_level, slot_index, shard_index)

let emit_dont_wait__register_trap ~delegate ~published_level ~slot_index
    ~shard_index =
  emit__dont_wait__use_with_care
    register_trap
    (delegate, published_level, slot_index, shard_index)

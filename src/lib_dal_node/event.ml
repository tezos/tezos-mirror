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

let pp_int_list fmt l =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
    Format.pp_print_int
    fmt
    l

let pp_pkh_list fmt l =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
    Signature.Public_key_hash.pp
    fmt
    l

(* DAL node event definitions *)

open struct
  let section = ["dal"]

  let starting_node =
    declare_2
      ~section
      ~prefix_name_with_section:true
      ~name:"starting"
      ~msg:
        "starting the DAL node (network: {network}, octez version: {version})"
      ~level:Notice
      ("network", Distributed_db_version.Name.encoding)
      ("version", Data_encoding.string)

  let waiting_l1_node_bootstrapped =
    declare_0
      ~section
      ~prefix_name_with_section:true
      ~name:"waiting_l1_node_to_bootstrap"
      ~msg:"waiting for the L1 node to be bootstrapped"
      ~level:Notice
      ()

  let l1_node_bootstrapped =
    declare_0
      ~section
      ~prefix_name_with_section:true
      ~name:"l1_node_bootstrapped"
      ~msg:"the L1 node is bootstrapped"
      ~level:Notice
      ()

  let waiting_known_plugin =
    declare_0
      ~section
      ~prefix_name_with_section:true
      ~name:"waiting_l1_node_known_plugin"
      ~msg:"waiting for a block with a known protocol plugin"
      ~level:Notice
      ()

  let shutdown_node =
    declare_1
      ~section
      ~prefix_name_with_section:true
      ~name:"stopping"
      ~msg:"stopping DAL node"
      ~level:Notice
      ("exit_status", Data_encoding.int8)

  let dal_node_sqlite3_store_init =
    declare_0
      ~section
      ~prefix_name_with_section:true
      ~name:"sqlite3_store_init"
      ~msg:"initializing the SQLite3 store"
      ~level:Info
      ()

  let store_is_ready =
    declare_0
      ~section
      ~prefix_name_with_section:true
      ~name:"store_is_ready"
      ~msg:"the DAL node store is ready"
      ~level:Notice
      ()

  let node_is_ready =
    declare_0
      ~section
      ~prefix_name_with_section:true
      ~name:"is_ready"
      ~msg:"the DAL node is ready"
      ~level:Notice
      ()

  let config_file_not_found =
    declare_1
      ~section
      ~prefix_name_with_section:true
      ~name:"no_data_dir"
      ~msg:
        "the DAL node configuration file does not exist in {path}, creating one"
      ~level:Warning
      ("path", Data_encoding.(string))

  let retry_fetching_info_from_l1 event_level =
    declare_3
      ~section
      ~prefix_name_with_section:true
      ~name:
        (Internal_event.Level.to_string event_level ^ "_fetching_info_from_l1")
      ~msg:
        "failed to fetch {info} from L1 node at {endpoint}, retry in {delay}s"
      ~level:event_level
      ("info", Data_encoding.string)
      ("endpoint", Data_encoding.string)
      ("delay", Data_encoding.float)

  let retry_fetching_info_from_l1_notice =
    retry_fetching_info_from_l1 Internal_event.Notice

  let retry_fetching_info_from_l1_warning =
    retry_fetching_info_from_l1 Internal_event.Warning

  let config_error_no_bootstrap =
    declare_0
      ~section
      ~prefix_name_with_section:true
      ~name:"config_error_no_bootstrap"
      ~msg:
        "no bootstrap peers found in the configuration file or network settings"
      ~level:Error
      ()

  let resolved_bootstrap_points =
    declare_2
      ~section
      ~prefix_name_with_section:true
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
      ~prefix_name_with_section:true
      ~name:"resolved_bootstrap_no_points"
      ~msg:"DNS resolution returned no bootstrap IP address"
      ~level:Error
      ()

  let resolved_bootstrap_points_total =
    declare_1
      ~section
      ~prefix_name_with_section:true
      ~name:"resolved_bootstrap_points_total"
      ~msg:"DNS resolution returned a total of {number} bootstrap IP addresses"
      ~level:Notice
      ("number", Data_encoding.int31)

  let fetched_l1_info_success =
    declare_2
      ~section
      ~prefix_name_with_section:true
      ~name:"fetched_l1_info_success"
      ~msg:"successfully fetched {info} from L1 node at {endpoint}"
      ~level:Notice
      ("info", Data_encoding.string)
      ("endpoint", Data_encoding.string)

  let reconstructed_slot =
    declare_2
      ~section
      ~prefix_name_with_section:true
      ~name:"reconstructed_slot"
      ~msg:"reconstructed slot: size {size}, shards {shards}"
      ~level:Info
      ("size", Data_encoding.int31)
      ("shards", Data_encoding.int31)

  let layer1_node_new_head =
    declare_3
      ~section
      ~prefix_name_with_section:true
      ~name:"new_L1_head_block"
      ~msg:"L1 head updated to {hash} at level {level}, fitness {fitness}"
      ~level:Info
      ~pp1:Block_hash.pp_short
      ~pp3:Fitness.pp
      ("hash", Block_hash.encoding)
      ("level", Data_encoding.int32)
      ("fitness", Fitness.encoding)

  let layer1_node_final_block =
    declare_3
      ~section
      ~prefix_name_with_section:true
      ~name:"new_L1_final_block"
      ~msg:"Finalized block {hash} at level {level}, round {round}"
      ~level:Notice
      ~pp1:Block_hash.pp
      ("hash", Block_hash.encoding)
      ("level", Data_encoding.int32)
      ("round", Data_encoding.int32)

  let layer1_node_tracking_started =
    declare_0
      ~section
      ~prefix_name_with_section:true
      ~name:"start_tracking_L1"
      ~msg:"started tracking layer 1's node"
      ~level:Notice
      ()

  let protocol_plugin_resolved =
    declare_2
      ~section
      ~prefix_name_with_section:true
      ~name:"plugin_resolved"
      ~msg:
        "resolved plugin on protocol {proto_hash} that starts at level \
         {start_level}"
      ~level:Notice
      ~pp1:Protocol_hash.pp_short
      ("proto_hash", Protocol_hash.encoding)
      ("start_level", Data_encoding.int32)

  let no_protocol_plugin =
    declare_1
      ~section
      ~prefix_name_with_section:true
      ~name:"no_plugin"
      ~msg:"could not resolve plugin for protocol {proto_hash}"
      ~level:Warning
      ~pp1:Protocol_hash.pp_short
      ("proto_hash", Protocol_hash.encoding)

  let no_protocol_constants =
    declare_2
      ~section
      ~prefix_name_with_section:true
      ~name:"no_protocol_constants"
      ~msg:
        "could not get the constants for protocol {proto_hash} at level {level}"
      ~level:Warning
      ~pp1:Protocol_hash.pp_short
      ("proto_hash", Protocol_hash.encoding)
      ("level", Data_encoding.int32)

  let unexpected_protocol_plugin =
    declare_0
      ~section
      ~prefix_name_with_section:true
      ~name:"unexpected_plugin"
      ~msg:
        "found plugin for the current protocol, expected one for the next \
         protocol."
      ~level:Error
      ()

  let daemon_error =
    declare_1
      ~section
      ~prefix_name_with_section:true
      ~name:"daemon_error"
      ~msg:"daemon thrown an error: {error}"
      ~level:Notice
      ~pp1:Error_monad.pp_print_trace
      ("error", Error_monad.trace_encoding)

  let failed_to_fetch_block =
    declare_4
      ~section
      ~prefix_name_with_section:true
      ~name:"crawler_failed_to_fetch_header"
      ~msg:
        "the crawler failed to fetch the block {type} at level {level} (for \
         last_notified_level {last_notified}): {error}\n\
         If you're a rollup operator or observer, you may be not be able to \
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
      ~prefix_name_with_section:true
      ~name:"history_mode_warning"
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
      ~prefix_name_with_section:true
      ~name:"configuration_loaded"
      ~msg:"configuration loaded successfully"
      ~level:Notice
      ()

  let upgrading_configuration =
    declare_2
      ~section
      ~prefix_name_with_section:true
      ~name:"upgrading_configuration"
      ~msg:"upgrading configuration from version {from_version} to {to_version}"
      ~level:Notice
      ("from_version", Data_encoding.int31)
      ("to_version", Data_encoding.int31)

  let stored_slot_content =
    declare_2
      ~section
      ~prefix_name_with_section:true
      ~name:"stored_slot_content"
      ~msg:"stored slot for level {published_level} and index {slot_index}"
      ~level:Info
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)

  let cached_or_stored_slot_shard ~kind =
    declare_3
      ~section
      ~prefix_name_with_section:true
      ~name:(Format.sprintf "%s_slot_shard" kind)
      ~msg:
        (Format.sprintf
           "%s shard {shard_index} for level {published_level} and index \
            {slot_index}"
           kind)
      ~level:Debug
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("shard_index", Data_encoding.int31)

  let stored_slot_shard = cached_or_stored_slot_shard ~kind:"stored"

  let cached_slot_shard = cached_or_stored_slot_shard ~kind:"cached"

  let removed_slot_shards =
    declare_2
      ~section
      ~prefix_name_with_section:true
      ~name:"removed_slot_shards"
      ~msg:"removed shards for level {published_level} and index {slot_index}"
      ~level:Debug
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)

  let removed_slot =
    declare_2
      ~section
      ~prefix_name_with_section:true
      ~name:"removed_slot"
      ~msg:"removed slot for level {published_level} and index {slot_index}"
      ~level:Debug
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)

  let slot_header_status_not_found =
    declare_2
      ~section
      ~prefix_name_with_section:true
      ~name:"slot_header_status_not_found"
      ~msg:
        "Slot header status not found for level {published_level}, slot index \
         {slot_index}."
      ~level:Warning
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)

  let slot_header_status_storage_error =
    declare_3
      ~section
      ~prefix_name_with_section:true
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
      ~prefix_name_with_section:true
      ~name:"unexpected_slot_header_status"
      ~msg:
        "Unexpected slot header status {got_status}, expected \
         {expected_status}, for level {published_level}, slot index \
         {slot_index}. The DAL node may be lagging."
      ~level:Warning
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("expected_status", Types.header_status_encoding)
      ("got_status", Types.header_status_encoding)
      ~pp3:Types.pp_header_status
      ~pp4:Types.pp_header_status

  let removed_skip_list_cells =
    declare_1
      ~section
      ~prefix_name_with_section:true
      ~name:"removed_skip_list_cells"
      ~msg:"removed skip list cells for level {level}"
      ~level:Debug
      ("level", Data_encoding.int32)

  let removing_shards_failed =
    declare_3
      ~section
      ~prefix_name_with_section:true
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
      ~prefix_name_with_section:true
      ~name:"removing_slot_failed"
      ~level:Warning
      ~msg:
        "removing slot for level {published_level} and index {slot_index} \
         failed: {error}"
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("error", Error_monad.trace_encoding)

  let removing_skip_list_cells_failed =
    declare_2
      ~section
      ~prefix_name_with_section:true
      ~name:"removing_skip_list_cells_failed"
      ~level:Warning
      ~msg:"removing skip list cells for level {level} failed: {error}"
      ("level", Data_encoding.int32)
      ("error", Error_monad.trace_encoding)

  let decoding_data_failed =
    declare_1
      ~section
      ~prefix_name_with_section:true
      ~name:"decoding_failed"
      ~msg:"error while decoding a {data_kind} value"
      ~level:Warning
      ("data_kind", Types.Store.encoding)

  let message_validation_error =
    declare_2
      ~section
      ~prefix_name_with_section:true
      ~name:"message_validation_failed"
      ~msg:
        "validating message with id {message_id} failed with error \
         {validation_error}"
      ~level:Warning
      ~pp1:Gossipsub.Worker.GS.Message_id.pp
      ("message_id", Types.Message_id.encoding)
      ("validation_error", Data_encoding.string)

  let batch_validation_error =
    declare_3
      ~section
      ~prefix_name_with_section:true
      ~name:"batch_validation_failed"
      ~msg:
        "validating batch of messages for level {level} and slot {slot_index} \
         failed with error {validation_error}"
      ~level:Warning
      ~pp1:(fun fmt -> Format.fprintf fmt "%ld")
      ~pp2:(fun fmt -> Format.fprintf fmt "%d")
      ("level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("validation_error", Data_encoding.string)

  let batch_validation_stats =
    declare_5
      ~section
      ~prefix_name_with_section:true
      ~name:"batch_validation_stats"
      ~msg:
        "Batch {batch_id} for level {level} of {elements} shards \
         ({percentage}) verified in {validation_duration}s"
      ~level:Info
      ("batch_id", Data_encoding.int31)
      ("level", Data_encoding.int32)
      ("elements", Data_encoding.int31)
      ("percentage", Data_encoding.float)
      ~pp4:(fun fmt -> Format.fprintf fmt "%.2f%%")
      ("validation_duration", Data_encoding.float)
      ~pp5:(fun fmt -> Format.fprintf fmt "%.4f")

  let pp_print_array_i ?(pp_sep = Format.pp_print_cut) pp_v ppf v =
    let pp_print_iter ?(pp_sep = Format.pp_print_cut) iter pp_v ppf v =
      let is_first = ref true in
      let pp_v v =
        if !is_first then is_first := false else pp_sep ppf () ;
        pp_v ppf v
      in
      iter pp_v v
    in
    pp_print_iter ~pp_sep Array.iteri pp_v ppf v

  let batch_validation_distribution_stats =
    declare_3
      ~section
      ~prefix_name_with_section:true
      ~name:"batch_validation_distribution_stats"
      ~msg:
        "Shard distribution of batch {batch_id} for level {level}: \
         {shard_distribution}"
      ~level:Debug
      ("batch_id", Data_encoding.int31)
      ("level", Data_encoding.int32)
      ("shard_distribution", Data_encoding.(array int31))
      ~pp3:(fun fmt ->
        Format.fprintf
          fmt
          "%a@."
          (pp_print_array_i
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
             (fun fmt i shards -> Format.fprintf fmt "(%d:%d)" i shards)))

  let batch_validation_completion_stats =
    declare_4
      ~section
      ~prefix_name_with_section:true
      ~name:"batch_validation_completion_stats"
      ~msg:
        "Shards for level {level} of {total_shard_processed} elements treated \
         with {total_batches_processed} batches in {validation_duration}s."
      ~level:Info
      ("level", Data_encoding.int32)
      ("total_shard_processed", Data_encoding.int31)
      ("total_batches_processed", Data_encoding.int31)
      ("validation_duration", Data_encoding.float)
      ~pp4:(fun ppf -> Format.fprintf ppf "%.3f")

  let p2p_server_is_ready =
    declare_1
      ~section
      ~prefix_name_with_section:true
      ~name:"p2p_server_is_ready"
      ~msg:"P2P server is listening on {point}"
      ~level:Notice
      ("point", P2p_point.Id.encoding)

  let rpc_server_is_ready =
    declare_1
      ~section
      ~prefix_name_with_section:true
      ~name:"rpc_server_is_ready"
      ~msg:"RPC server is listening on {point}"
      ~level:Notice
      ("point", P2p_point.Id.encoding)

  let metrics_server_starting =
    declare_1
      ~section:(section @ ["metrics"])
      ~prefix_name_with_section:true
      ~name:"metrics_service_start"
      ~msg:"Starting metrics service at {endpoint}"
      ~level:Notice
      ("endpoint", P2p_point.Id.encoding)

  let metrics_server_not_starting =
    declare_0
      ~section:(section @ ["metrics"])
      ~prefix_name_with_section:true
      ~name:"metrics_service_no_start"
      ~msg:"metrics service not enabled"
      ~level:Notice
      ()

  let metrics_server_is_ready =
    let open Internal_event.Simple in
    declare_2
      ~section
      ~prefix_name_with_section:true
      ~name:"starting_metrics_server"
      ~msg:"metrics server is listening on {host}:{port}"
      ~level:Notice
      ("host", Data_encoding.string)
      ("port", Data_encoding.uint16)

  let loading_profiles_failed =
    declare_1
      ~section
      ~prefix_name_with_section:true
      ~name:"loading_profiles_failed"
      ~msg:"loading profiles failed: {error}"
      ~level:Info
      ("error", Error_monad.trace_encoding)

  let saving_profiles_failed =
    declare_1
      ~section
      ~prefix_name_with_section:true
      ~name:"saving_profiles_failed"
      ~msg:"saving profiles failed: {error}"
      ~level:Error
      ("error", Error_monad.trace_encoding)

  let reconstruct_starting_in =
    declare_3
      ~section
      ~prefix_name_with_section:true
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
      ~prefix_name_with_section:true
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
      ~prefix_name_with_section:true
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
      ~prefix_name_with_section:true
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
      ~prefix_name_with_section:true
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
      ~prefix_name_with_section:true
      ~name:"store_upgrade_error_moving_directory"
      ~msg:"There was an error trying to move {src} to {dst}: {exn}"
      ~level:Warning
      ("src", Data_encoding.string)
      ("dst", Data_encoding.string)
      ("exn", Data_encoding.string)

  let store_upgrade_error_creating_directory =
    declare_2
      ~section
      ~prefix_name_with_section:true
      ~name:"store_upgrade_error_creating_directory"
      ~msg:"There was an error trying to create directory {path}: {exn}"
      ~level:Warning
      ("path", Data_encoding.string)
      ("exn", Data_encoding.string)

  let store_upgrade_start =
    declare_2
      ~section
      ~prefix_name_with_section:true
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
      ~prefix_name_with_section:true
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
      ~prefix_name_with_section:true
      ~name:"store_upgrade_error"
      ~msg:"Failed to upgrade the store."
      ~level:Error
      ()

  let crypto_process_started =
    declare_0
      ~section:(section @ ["crypto"])
      ~prefix_name_with_section:true
      ~name:"crypto_process_started"
      ~msg:"cryptographic child process started"
      ~level:Notice
      ()

  let crypto_process_stopped =
    declare_0
      ~section:(section @ ["crypto"])
      ~prefix_name_with_section:true
      ~name:"crypto_process_stopped"
      ~msg:"cryptographic child process stopped gracefully"
      ~level:Notice
      ()

  let crypto_process_fatal =
    declare_1
      ~section:(section @ ["crypto"])
      ~prefix_name_with_section:true
      ~name:"crypto_process_fatal"
      ~msg:"cryptographic child process terminated unexpectedly: #{error}."
      ~level:Error
      ("error", Data_encoding.string)
      ~pp1:Format.pp_print_string

  let crypto_process_error =
    declare_1
      ~section:(section @ ["crypto"])
      ~prefix_name_with_section:true
      ~name:"crypto_process_error"
      ~msg:"cryptographic child process error: #{error}."
      ~level:Warning
      ("error", Data_encoding.string)

  let amplificator_uninitialized =
    declare_0
      ~section:(section @ ["crypto"])
      ~prefix_name_with_section:true
      ~name:"amplificator_uninitialized"
      ~msg:"the amplificator process worker is not initialized"
      ~level:Warning
      ()

  let crypto_process_received_query =
    declare_1
      ~section:(section @ ["crypto"])
      ~prefix_name_with_section:true
      ~name:"crypto_process_received_query"
      ~msg:"cryptographic child process: received query #{query_id}."
      ~level:Notice
      ("query_id", Data_encoding.int31)

  let crypto_process_sending_reply =
    declare_1
      ~section:(section @ ["crypto"])
      ~prefix_name_with_section:true
      ~name:"crypto_process_sending_reply"
      ~msg:"cryptographic child process: sending reply #{query_id}."
      ~level:Info
      ("query_id", Data_encoding.int31)

  let crypto_process_sending_reply_error =
    declare_1
      ~section:(section @ ["crypto"])
      ~prefix_name_with_section:true
      ~name:"crypto_process_sending_reply_error"
      ~msg:"cryptographic child process: sending reply error #{query_id}."
      ~level:Warning
      ("query_id", Data_encoding.int31)

  let main_process_sending_query =
    declare_1
      ~section:(section @ ["crypto"])
      ~prefix_name_with_section:true
      ~name:"main_process_sending_query"
      ~msg:
        "main process: sending query #{query_id} to cryptographic child \
         process."
      ~level:Info
      ("query_id", Data_encoding.int31)

  let main_process_received_reply =
    declare_1
      ~section:(section @ ["crypto"])
      ~prefix_name_with_section:true
      ~name:"main_process_received_reply"
      ~msg:"main process: received reply #{query_id}."
      ~level:Info
      ("query_id", Data_encoding.int31)

  let main_process_received_reply_error =
    declare_2
      ~section:(section @ ["crypto"])
      ~prefix_name_with_section:true
      ~name:"main_process_received_reply_error"
      ~msg:"main process: received reply error on query #{query_id} : {error}."
      ~level:Warning
      ("query_id", Data_encoding.int31)
      ("error", Data_encoding.string)

  let main_process_enqueue_query =
    declare_1
      ~section:(section @ ["crypto"])
      ~prefix_name_with_section:true
      ~name:"main_process_enqueue_query"
      ~msg:"main process: enqueue query #{query_id}."
      ~level:Info
      ("query_id", Data_encoding.int31)

  let get_attestable_slots_ok_notice =
    declare_3
      ~section
      ~prefix_name_with_section:true
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
      ~prefix_name_with_section:true
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
      ~prefix_name_with_section:true
      ~name:"get_attestable_slots_future_level_warning"
      ~msg:
        "It looks like the DAL node is lagging (its current level is \
         {current_level}, while the Layer1 node's level is \
         {current_baker_level})."
      ~level:Warning
      ("current_level", Data_encoding.int32)
      ("current_baker_level", Data_encoding.int32)

  let warn_no_attestation =
    declare_2
      ~section
      ~prefix_name_with_section:true
      ~name:"no_attestation"
      ~msg:
        "An attestation operation was not included for {attester} at attested \
         level {attested_level}."
      ~level:Warning
      ("attester", Signature.Public_key_hash.encoding)
      ("attested_level", Data_encoding.int32)
      ~pp1:Signature.Public_key_hash.pp_short

  let attester_attested =
    declare_3
      ~section
      ~prefix_name_with_section:true
      ~name:"attester_attested"
      ~msg:
        "{attester} attested slot(s) {slot_indexes} at attested level \
         {attested_level}."
      ~level:Notice
      ("attester", Signature.Public_key_hash.encoding)
      ("attested_level", Data_encoding.int32)
      ("slot_indexes", Data_encoding.(list int31))
      ~pp1:Signature.Public_key_hash.pp_short
      ~pp3:pp_int_list

  let warn_attester_not_dal_attesting =
    declare_2
      ~section
      ~prefix_name_with_section:true
      ~name:"attester_not_dal_attesting"
      ~msg:
        "No DAL content was included by {attester} for attested level \
         {attested_level}"
      ~level:Warning
      ("attester", Signature.Public_key_hash.encoding)
      ("attested_level", Data_encoding.int32)
      ~pp1:Signature.Public_key_hash.pp_short

  let warn_attester_did_not_attest =
    declare_3
      ~section
      ~prefix_name_with_section:true
      ~name:"attester_did_not_attest"
      ~msg:
        "At level {attested_level}, slot index(es) {slot_indexes} were \
         sufficiently attested, but {attester} neither attested them nor \
         identified them as traps."
      ~level:Warning
      ("attester", Signature.Public_key_hash.encoding)
      ("attested_level", Data_encoding.int32)
      ("slot_indexes", Data_encoding.(list int31))
      ~pp1:Signature.Public_key_hash.pp_short
      ~pp3:pp_int_list

  let attester_did_not_attest_because_of_traps =
    declare_3
      ~section
      ~prefix_name_with_section:true
      ~name:"attester_did_not_attest_traps"
      ~msg:
        "At level {attested_level}, slot index(es) {slot_indexes} were \
         sufficiently attested, but {attester} did not attest them because of \
         traps"
      ~level:Notice
      ("attester", Signature.Public_key_hash.encoding)
      ("attested_level", Data_encoding.int32)
      ("slot_indexes", Data_encoding.(list int31))
      ~pp1:Signature.Public_key_hash.pp_short
      ~pp3:pp_int_list

  let trap_injection =
    declare_5
      ~section
      ~prefix_name_with_section:true
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
      ~prefix_name_with_section:true
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
    declare_4
      ~section
      ~prefix_name_with_section:true
      ~name:"trap_check_failure"
      ~msg:
        "An error occurred when checking the trap for published level \
         {published_level}, slot index {slot_index}, shard index {shard_index} \
         and delegate {delegate}"
      ~level:Warning
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("shard_index", Data_encoding.int31)
      ("delegate", Signature.Public_key_hash.encoding)

  let registered_pkh_not_a_delegate =
    declare_1
      ~section
      ~prefix_name_with_section:true
      ~name:"register_pkh_not_a_delegate"
      ~msg:
        "The public key hash {pkh} registered by PATCH /profiles is not a \
         delegate."
      ~level:Warning
      ("pkh", Signature.Public_key_hash.encoding)

  let cannot_attest_slot_because_of_trap =
    declare_4
      ~section
      ~prefix_name_with_section:true
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
      ~prefix_name_with_section:true
      ~name:"register_trap"
      ~msg:
        "The shard {shard_index} for slot {slot_index} and published level \
         {published_level} is a trap for {delegate}."
      ~level:Info
      ("delegate", Signature.Public_key_hash.encoding)
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("shard_index", Data_encoding.int31)

  let start_catchup =
    declare_3
      ~section
      ~prefix_name_with_section:true
      ~name:"start_catchup"
      ~msg:
        "catching up to level {end_level}, from level {start_level} (that is, \
         {levels_to_clean_up} levels to process)"
      ~level:Notice
      ("start_level", Data_encoding.int32)
      ("end_level", Data_encoding.int32)
      ("levels_to_clean_up", Data_encoding.int32)

  let catching_up =
    declare_1
      ~section
      ~prefix_name_with_section:true
      ~name:"catching_up"
      ~msg:"caught up the store up to level {current_level}"
      ~level:Notice
      ("current_level", Data_encoding.int32)

  let end_catchup =
    declare_0
      ~section
      ~prefix_name_with_section:true
      ~name:"end_catchup"
      ~msg:"catching up done"
      ~level:Notice
      ()

  let failed_to_retrieve_commitment_of_slot_id =
    declare_3
      ~section
      ~prefix_name_with_section:true
      ~name:"failed_to_retrieve_commitment_of_slot_id"
      ~msg:
        "The node is unable to retrieve the commitment of the slot published \
         at level {published_level} and index {slot_index} from neither \
         memory, on-disk store, nor the L1 context. Error {error}"
      ~level:Warning
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("error", Data_encoding.list Error_monad.trace_encoding)

  let slot_from_backup_has_unexpected_size =
    declare_5
      ~section
      ~prefix_name_with_section:true
      ~name:"slot_from_backup_has_unexpected_size"
      ~msg:
        "The content of the slot published at level {published_level} and \
         index {slot_index} fetched from {backup_uri} has a bad size. \
         Expected:{expected_size}, got: {obtained_size}."
      ~level:Warning
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("backup_uri", Data_encoding.string)
      ("expected_size", Data_encoding.int31)
      ("obtained_size", Data_encoding.int31)

  let slot_from_backup_has_unexpected_commitment =
    declare_5
      ~section
      ~prefix_name_with_section:true
      ~name:"slot_from_backup_has_unexpected_commitment"
      ~msg:
        "The content of the slot published at level {published_level} and \
         index {slot_index} fetched from {backup_uri} has a bad commitment. \
         Expected:{expected_commitment}, got: {obtained_commitment}."
      ~level:Warning
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("backup_uri", Data_encoding.string)
      ("expected_commitment", Cryptobox.Commitment.encoding)
      ("obtained_commitment", Cryptobox.Commitment.encoding)

  let fetching_slot_from_backup_failed =
    declare_4
      ~section
      ~prefix_name_with_section:true
      ~name:"fetching_slot_from_backup_failed"
      ~msg:
        "Fetching the content of the slot published at level {published_level} \
         and index {slot_index} from {backup_uri} failed with status {status}."
      ~level:Warning
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("backup_uri", Data_encoding.string)
      ("status", Data_encoding.string)

  let shard_validation_is_disabled =
    declare_0
      ~section
      ~prefix_name_with_section:true
      ~name:"shard_validation_is_disabled"
      ~msg:"shard validation is disabled"
      ~level:Warning
      ()

  let ignoring_pkhs =
    declare_1
      ~section
      ~prefix_name_with_section:true
      ~name:"ignoring_pkhs"
      ~msg:"The node will not propagate shards assigned to {delegates}."
      ~level:Warning
      ("delegates", Data_encoding.list Signature.Public_key_hash.encoding)
      ~pp1:pp_pkh_list

  let reception_of_shard_update =
    declare_4
      ~section:["dal"; "reception"]
      ~prefix_name_with_section:true
      ~name:"reception_of_shard_update"
      ~msg:
        "For level {level} and slot {slot_index} {position} shard has been \
         received.\n\
         {slot_metrics}"
      ~level:Info
      ("level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("position", Data_encoding.string)
      ("slot_metrics", Dal_metrics.slot_metrics_encoding)
      ~pp4:Dal_metrics.pp_slot_metrics_received

  let validation_of_shard_update =
    declare_4
      ~section:["dal"; "attestation"]
      ~prefix_name_with_section:true
      ~name:"validation_of_shard_update"
      ~msg:
        "For level {level} and slot {slot_index} {position} shard(s) have been \
         validated.\n\
         {slot_metrics}"
      ~level:Debug
      ("level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("position", Data_encoding.string)
      ("slot_metrics", Dal_metrics.slot_metrics_encoding)
      ~pp4:Dal_metrics.pp_slot_metrics

  let reception_of_shard_detailed =
    declare_4
      ~section:["dal_shards"; "reception"]
      ~name:"reception_of_a_shard"
      ~msg:
        "For level {level} and slot {slot_index}, shard {shard_index} has been \
         received from {sender}."
      ~level:Debug
      ("level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("shard_index", Data_encoding.int31)
      ("sender", Types.Peer.encoding)
      ~pp4:Types.Peer.pp

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/8064 *)
  let skip_attesting_shards =
    declare_1
      ~section:["dal_shards"; "skip"]
      ~name:"skip_attesting_shards"
      ~msg:
        "Skip attested level {level} due to change in attestation lag at \
         migration level."
      ~level:Warning
      ("level", Data_encoding.int32)

  let backfill_error =
    declare_1
      ~section
      ~name:"backfill_error"
      ~msg:"Backfill failed with error: {error}"
      ~level:Error
      ~pp1:Error_monad.pp_print_trace
      ("error", Error_monad.trace_encoding)

  let publication =
    declare_2
      ~section
      ~name:"publication"
      ~msg:"Publication operation {op_hash} at level {block_level} injected"
      ~level:Info
      ("op_hash", Operation_hash.encoding)
      ("block_level", Data_encoding.int32)

  let publication_failed =
    declare_2
      ~section
      ~name:"publication_failed"
      ~msg:"Publication at level {block_level} failed with error: {error}"
      ~level:Warning
      ("block_level", Data_encoding.int32)
      ~pp2:Error_monad.pp_print_trace
      ("error", Error_monad.trace_encoding)
end

(* DAL node event emission functions *)

let emit_failed_to_retrieve_commitment_of_slot_id ~published_level ~slot_index
    ~error =
  emit
    failed_to_retrieve_commitment_of_slot_id
    (published_level, slot_index, error)

let emit_slot_from_backup_has_unexpected_size ~published_level ~slot_index
    ~backup_uri ~expected_size ~obtained_size =
  emit
    slot_from_backup_has_unexpected_size
    ( published_level,
      slot_index,
      Uri.to_string backup_uri,
      expected_size,
      obtained_size )

let emit_slot_from_backup_has_unexpected_commitment ~published_level ~slot_index
    ~backup_uri ~expected_commitment ~obtained_commitment =
  emit
    slot_from_backup_has_unexpected_commitment
    ( published_level,
      slot_index,
      Uri.to_string backup_uri,
      expected_commitment,
      obtained_commitment )

let emit_fetching_slot_from_backup_failed ~published_level ~slot_index
    ~backup_uri ~status =
  emit
    fetching_slot_from_backup_failed
    ( published_level,
      slot_index,
      Uri.to_string backup_uri,
      Cohttp.Code.string_of_status status )

let emit_starting_node ~network_name ~version =
  emit starting_node (network_name, version)

let emit_waiting_l1_node_bootstrapped () = emit waiting_l1_node_bootstrapped ()

let emit_l1_node_bootstrapped () = emit l1_node_bootstrapped ()

let emit_waiting_known_plugin () = emit waiting_known_plugin ()

let emit_shutdown_node ~exit_status = emit shutdown_node exit_status

let emit_dal_node_sqlite3_store_init () = emit dal_node_sqlite3_store_init ()

let emit_store_is_ready () = emit store_is_ready ()

let emit_node_is_ready () = emit node_is_ready ()

let emit_config_file_not_found ~path = emit config_file_not_found path

let emit_retry_fetching_info_from_l1 ~endpoint ~delay ~requested_info
    ~event_level =
  match event_level with
  | `Notice ->
      emit retry_fetching_info_from_l1_notice (requested_info, endpoint, delay)
  | `Warning ->
      emit retry_fetching_info_from_l1_warning (requested_info, endpoint, delay)

let emit_config_error_no_bootstrap () = emit config_error_no_bootstrap ()

let emit_resolved_bootstrap_points ~domainname ~number =
  emit resolved_bootstrap_points (domainname, number)

let emit_resolved_bootstrap_no_points () = emit resolved_bootstrap_no_points ()

let emit_resolved_bootstrap_points_total ~number =
  emit resolved_bootstrap_points_total number

let emit_fetched_l1_info_success ~requested_info ~endpoint =
  emit fetched_l1_info_success (requested_info, endpoint)

let emit_reconstructed_slot ~size ~shards =
  emit reconstructed_slot (size, shards)

let emit_layer1_node_new_head ~hash ~level ~fitness =
  emit layer1_node_new_head (hash, level, fitness)

let emit_layer1_node_final_block ~hash ~level ~round =
  emit layer1_node_final_block (hash, level, round)

let emit_layer1_node_tracking_started () = emit layer1_node_tracking_started ()

let emit_protocol_plugin_resolved ~proto_hash ~start_level =
  emit protocol_plugin_resolved (proto_hash, start_level)

let emit_no_protocol_plugin ~proto_hash = emit no_protocol_plugin proto_hash

let emit_no_protocol_constnts ~proto_hash ~level =
  emit no_protocol_constants (proto_hash, level)

let emit_unexpected_protocol_plugin () = emit unexpected_protocol_plugin ()

let emit_daemon_error ~error = emit daemon_error error

let emit_failed_to_fetch_block ~type_ ~level ~last_notified ~error =
  emit failed_to_fetch_block (type_, level, last_notified, error)

let emit_history_mode_warning ~stored_levels ~storage_period =
  emit history_mode_warning (stored_levels, storage_period)

let emit_configuration_loaded () = emit configuration_loaded ()

let emit_upgrading_configuration ~from ~into =
  emit upgrading_configuration (from, into)

let emit_stored_slot_content ~published_level ~slot_index =
  emit stored_slot_content (published_level, slot_index)

let emit_stored_slot_shard ~published_level ~slot_index ~shard_index =
  emit stored_slot_shard (published_level, slot_index, shard_index)

let emit_cached_slot_shard ~published_level ~slot_index ~shard_index =
  emit cached_slot_shard (published_level, slot_index, shard_index)

let emit_removed_slot_shards ~published_level ~slot_index =
  emit removed_slot_shards (published_level, slot_index)

let emit_removed_slot ~published_level ~slot_index =
  emit removed_slot (published_level, slot_index)

let emit_slot_header_status_not_found ~published_level ~slot_index =
  emit slot_header_status_not_found (published_level, slot_index)

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

let emit_removing_skip_list_cells_failed ~level ~error =
  emit removing_skip_list_cells_failed (level, error)

let emit_decoding_data_failed ~data_kind = emit decoding_data_failed data_kind

let emit_dont_wait__message_validation_error ~message_id ~validation_error =
  emit__dont_wait__use_with_care
    message_validation_error
    (message_id, validation_error)

let emit_batch_validation_error ~level ~slot_index ~validation_error =
  emit batch_validation_error (level, slot_index, validation_error)

let emit_dont_wait__batch_validation_stats ~batch_id ~head_level
    ~number_of_shards ~shard_percentage ~duration =
  emit__dont_wait__use_with_care
    batch_validation_stats
    (batch_id, head_level, number_of_shards, shard_percentage, duration)

let emit_dont_wait__batch_validation_distribution_stats ~batch_id ~head_level
    ~shard_distribution =
  emit__dont_wait__use_with_care
    batch_validation_distribution_stats
    (batch_id, head_level, shard_distribution)

let emit_dont_wait__batch_validation_completion_stats ~level
    ~total_shard_processed ~total_batches_processed ~duration =
  emit__dont_wait__use_with_care
    batch_validation_completion_stats
    (level, total_shard_processed, total_batches_processed, duration)

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

let emit_crypto_process_started () = emit crypto_process_started ()

let emit_crypto_process_stopped () = emit crypto_process_stopped ()

let emit_crypto_process_error ~msg = emit crypto_process_error msg

let emit_crypto_process_fatal ~msg = emit crypto_process_fatal msg

let emit_amplificator_uninitialized () = emit amplificator_uninitialized ()

let emit_crypto_process_received_query ~query_id =
  emit crypto_process_received_query query_id

let emit_crypto_process_sending_reply ~query_id =
  emit crypto_process_sending_reply query_id

let emit_crypto_process_sending_reply_error ~query_id =
  emit crypto_process_sending_reply_error query_id

let emit_main_process_sending_query ~query_id =
  emit main_process_sending_query query_id

let emit_main_process_received_reply ~query_id =
  emit main_process_received_reply query_id

let emit_main_process_received_reply_error ~query_id ~msg =
  emit main_process_received_reply_error (query_id, msg)

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

let emit_warn_no_attestation ~attester ~attested_level =
  emit warn_no_attestation (attester, attested_level)

let emit_attester_attested ~attester ~attested_level ~slot_indexes =
  emit attester_attested (attester, attested_level, slot_indexes)

let emit_warn_attester_not_dal_attesting ~attester ~attested_level =
  emit warn_attester_not_dal_attesting (attester, attested_level)

let emit_warn_attester_did_not_attest ~attester ~attested_level ~slot_indexes =
  emit warn_attester_did_not_attest (attester, attested_level, slot_indexes)

let emit_attester_did_not_attest_because_of_traps ~attester ~attested_level
    ~slot_indexes =
  emit
    attester_did_not_attest_because_of_traps
    (attester, attested_level, slot_indexes)

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

let emit_trap_check_failure ~published_level ~slot_index ~shard_index ~delegate
    =
  emit trap_check_failure (published_level, slot_index, shard_index, delegate)

let emit_registered_pkh_not_a_delegate ~pkh =
  emit registered_pkh_not_a_delegate pkh

let emit_cannot_attest_slot_because_of_trap ~pkh ~published_level ~slot_index
    ~shard_index =
  emit
    cannot_attest_slot_because_of_trap
    (pkh, published_level, slot_index, shard_index)

let emit_register_trap ~delegate ~published_level ~slot_index ~shard_index =
  emit register_trap (delegate, published_level, slot_index, shard_index)

let emit_start_catchup ~start_level ~end_level ~levels_to_clean_up =
  emit start_catchup (start_level, end_level, levels_to_clean_up)

let emit_catching_up ~current_level = emit catching_up current_level

let emit_end_catchup () = emit end_catchup ()

let emit_shard_validation_is_disabled () = emit shard_validation_is_disabled ()

let emit_ignoring_pkhs ~pkhs = emit ignoring_pkhs pkhs

let emit_reception_of_shard_update ~level ~slot_index ~slot_metrics =
  let position =
    Option.fold
      ~none:"first"
      ~some:(fun _ -> "last")
      slot_metrics.Dal_metrics.duration_all_shards_received
  in
  emit reception_of_shard_update (level, slot_index, position, slot_metrics)

let emit_validation_of_shard_update ~level ~slot_index ~slot_metrics =
  let position =
    Option.fold
      ~none:"first"
      ~some:(fun _ ->
        Option.fold
          ~none:"enough"
          ~some:(fun _ -> "all")
          slot_metrics.Dal_metrics.duration_all_shards_validated)
      slot_metrics.Dal_metrics.duration_enough_shards_validated
  in
  emit validation_of_shard_update (level, slot_index, position, slot_metrics)

let emit_reception_of_shard_detailed ~level ~slot_index ~shard_index ~sender =
  emit reception_of_shard_detailed (level, slot_index, shard_index, sender)

let emit_skip_attesting_shards ~level = emit skip_attesting_shards level

let emit_backfill_error ~error = emit backfill_error error

let emit_publication ~block_level ~op_hash =
  emit publication (op_hash, block_level)

let emit_publication_failed ~block_level ~error =
  emit publication_failed (block_level, error)

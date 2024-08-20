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

include Internal_event.Simple

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

let shutdown_node =
  declare_1
    ~section
    ~name:"stopping_dal_node"
    ~msg:"stopping DAL node"
    ~level:Notice
    ("exit_status", Data_encoding.int8)

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

let retry_fetching_node_config =
  declare_2
    ~section
    ~name:"retry_fetching_config"
    ~msg:"Cannot fetch config from l1 node at {endpoint}, retrying in {delay}s"
    ~level:Error
    ("endpoint", Data_encoding.string)
    ("delay", Data_encoding.float)

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
      "stored slot status for level {published_level} and index {slot_index}: \
       {status}"
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

let reconstruct_missing_prover_srs =
  declare_2
    ~section
    ~name:"reconstruct_missing_prover_srs"
    ~msg:
      "Missing prover SRS, reconstruction for the level {level} and slot index \
       {slot_index} was skipped."
    ~level:Warning
    ~pp1:(fun fmt -> Format.fprintf fmt "%ld")
    ("level", Data_encoding.int32)
    ~pp2:Format.pp_print_int
    ("slot_index", Data_encoding.int31)

let reconstruct_starting_in =
  declare_3
    ~section
    ~name:"reconstruct_starting_in"
    ~msg:
      "For the level {level} and slot index {slot_index}, enough shards have \
       been received to reconstruct the slot. If the remaining shards are not \
       received in {delay} seconds, they will be reconstructed."
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
      "For the level {level} and slot index {slot_index}, missing shards have \
       been successfully reconstructed."
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

let store_upgraded =
  declare_2
    ~section
    ~name:"store_upgraded"
    ~msg:
      "The store has been upgraded from version {old_version} to {new_version}."
    ~level:Notice
    ("old_version", Data_encoding.int31)
    ("new_version", Data_encoding.int31)

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
      "main process: sending query #{query_id} to cryptographic child process."
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

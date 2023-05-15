(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
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

let section = ["dac"; "node"]

let starting_node =
  declare_0
    ~section
    ~name:"starting_dac_node"
    ~msg:"Starting the DAC node"
    ~level:Notice
    ()

let shutdown_node =
  declare_1
    ~section
    ~name:"stopping_dac_node"
    ~msg:"Stopping DAC node"
    ~level:Notice
    ("exit_status", Data_encoding.int8)

let store_is_ready =
  declare_0
    ~section
    ~name:"dac_node_store_is_ready"
    ~msg:"The DAC node store is ready"
    ~level:Notice
    ()

let operating_mode =
  declare_1
    ~section
    ~name:"dac_node_running_mode"
    ~msg:"The DAC node is running in mode {mode}"
    ~level:Notice
    ("mode", Data_encoding.string)

let rpc_server_is_ready =
  declare_2
    ~section
    ~name:"dac_node_rpc_server_is_ready"
    ~msg:"The DAC node is listening to {addr}:{port}"
    ~level:Notice
    ("addr", Data_encoding.string)
    ("port", Data_encoding.uint16)

let node_is_ready =
  declare_0
    ~section
    ~name:"dac_node_is_ready"
    ~msg:"The DAC node is ready"
    ~level:Notice
    ()

let committee_keys_imported =
  declare_0
    ~section
    ~name:"committee_keys_imported"
    ~msg:"The Data Availability Committee keys had been successfully imported"
    ~level:Notice
    ()

let data_dir_not_found =
  declare_1
    ~section
    ~name:"dac_node_no_data_dir"
    ~msg:
      "The DAC node data directory {path} doesn't exists. Create using: \
       init-config --data-dir={path} "
    ~level:Error
    ("path", Data_encoding.(string))

let layer1_node_new_head =
  declare_2
    ~section
    ~name:"dac_node_layer_1_new_head"
    ~msg:"Head of layer 1's node updated to {hash} at level {level}"
    ~level:Notice
    ("hash", Block_hash.encoding)
    ("level", Data_encoding.int32)

let layer1_node_tracking_started =
  declare_0
    ~section
    ~name:"dac_node_layer_1_start_tracking"
    ~msg:"Started tracking layer 1's node"
    ~level:Notice
    ()

let protocol_plugin_resolved =
  declare_1
    ~section
    ~name:"dac_node_plugin_resolved"
    ~msg:"Resolved plugin for protocol {proto_hash}"
    ~level:Notice
    ("proto_hash", Data_encoding.string)

let protocol_plugin_not_resolved =
  declare_2
    ~section
    ~name:"dac_node_plugin_not_resolved"
    ~msg:
      "Could not resolve plugin for protocols {current_protocol} or \
       {next_protocol}"
    ~level:Notice
    ("current_protocol", Data_encoding.string)
    ("next_protocol", Data_encoding.string)

let daemon_error =
  declare_1
    ~section
    ~name:"dac_node_daemon_error"
    ~msg:"Daemon thrown an error: {error}"
    ~level:Notice
    ~pp1:Error_monad.pp_print_trace
    ("error", Error_monad.trace_encoding)

let dac_threshold_not_reached =
  declare_2
    ~section
    ~name:"dac_threshold_not_reached"
    ~msg:
      "Only {provided} out of {required} dac accounts are available for \
       signing messages"
    ~level:Warning
    ("provided", Data_encoding.int31)
    ("required", Data_encoding.int31)

let committee_member_not_in_wallet =
  declare_1
    ~section
    ~name:"committee_member_not_in_wallet"
    ~msg:
      "There is no account with public key {tz4_account} in the Tezos client \
       wallet. This account won't be used for signing DAC root hash pages."
    ~level:Warning
    ("tz4_account", Tezos_crypto.Aggregate_signature.Public_key_hash.encoding)

let committee_member_cannot_sign =
  declare_1
    ~section
    ~name:"committee_member_cannot_sign"
    ~msg:
      "There is an account with public key {tz4_account} in the Tezos client \
       wallet, but its secret key URI is not available. This account won't be \
       used for signing DAC root hash pages."
    ~level:Warning
    ("tz4_account", Tezos_crypto.Aggregate_signature.Public_key_hash.encoding)

let commit_member_no_public_key =
  declare_1
    ~section
    ~name:"committee_member_no_public_key"
    ~msg:
      "There is an account with public key hash {tz4_account} in the Tezos \
       client wallet, but its public key is not available. Signatures from \
       this account cannot be verified and will be ignored."
    ~level:Warning
    ("tz4_account", Tezos_crypto.Aggregate_signature.Public_key_hash.encoding)

let handle_new_subscription_to_hash_streamer =
  declare_0
    ~section
    ~name:"handle_new_subscription_to_hash_streamer"
    ~msg:
      "Subscription of another dac node to the hash streamer handled \
       successfully."
    ~level:Notice
    ()

let subscribed_to_root_hashes_stream =
  declare_0
    ~section
    ~name:"subscribed_to_root_hashes_stream"
    ~msg:"Subscribed to root hashes stream"
    ~level:Notice
    ()

let new_root_hash_received =
  declare_1
    ~section
    ~name:"dac_node_new_root_hash_received"
    ~msg:"Received new root hash via monitoring rpc {root_hash}"
    ~level:Notice
    ("root_hash", Data_encoding.string)

let new_subscription_to_certificate_updates =
  declare_1
    ~section
    ~name:"dac_node_new_certificate_upgrades_subscription"
    ~msg:
      "Received a client request to track certificate updates for {root_hash}"
    ~level:Notice
    ("root_hash", Data_encoding.string)

let received_root_hash_processed =
  declare_1
    ~section
    ~name:"dac_node_received_root_hash_processed"
    ~msg:"Finished processing previously received root hash {root_hash}"
    ~level:Notice
    ("root_hash", Data_encoding.string)

let new_hash_pushed_to_data_streamer =
  declare_1
    ~section
    ~name:"root_hash_pushed_to_the_data_streamer"
    ~msg:"New root hash pushed to the data streamer: {root_hash}"
    ~level:Notice
    ("root_hash", Data_encoding.string)

let processing_root_hash_failed =
  declare_2
    ~section
    ~name:"processing_root_hash_failed"
    ~msg:
      "Processing root hash {root_hash} resulted in the wollowing errors: \
       {errors}"
    ~level:Warning
    ("root_hash", Data_encoding.string)
    ("errors", Error_monad.trace_encoding)

let new_signature_pushed_to_coordinator =
  declare_1
    ~section
    ~name:"new_signature_pushed_to_coordinator"
    ~msg:"New signature from member pushed to the coordinator: {signature}"
    ~level:Notice
    ("signature", Tezos_crypto.Aggregate_signature.encoding)

let no_committee_member_address =
  declare_0
    ~section
    ~name:"no_committee_member_address"
    ~msg:
      "No committee member address provided, node will only deserialize \
       payload, not signing it"
    ~level:Notice
    ()

let cannot_retrieve_keys_from_address =
  declare_1
    ~section
    ~name:"cannot_retrieve_keys_from_address"
    ~msg:
      "Cannot retrieve keys from address: {address}, node is not a Committee \
       Member"
    ~level:Notice
    ("address", Tezos_crypto.Aggregate_signature.Public_key_hash.encoding)

let proto_short_hash_string hash =
  Format.asprintf "%a" Protocol_hash.pp_short hash

let emit_protocol_plugin_resolved hash =
  emit protocol_plugin_resolved @@ proto_short_hash_string hash

let emit_protocol_plugin_not_resolved current_protocol next_protocol =
  emit
    protocol_plugin_not_resolved
    ( proto_short_hash_string current_protocol,
      proto_short_hash_string next_protocol )

let emit_new_root_hash_received ((module Plugin) : Dac_plugin.t) hash =
  emit new_root_hash_received (Plugin.to_hex hash)

let emit_new_subscription_to_certificate_updates hash =
  let hex = Hex.show @@ Dac_plugin.hash_to_hex hash in
  emit new_subscription_to_certificate_updates hex

let emit_root_hash_pushed_to_data_streamer ((module Plugin) : Dac_plugin.t) hash
    =
  emit new_hash_pushed_to_data_streamer (Plugin.to_hex hash)

let emit_received_root_hash_processed ((module Plugin) : Dac_plugin.t) hash =
  emit received_root_hash_processed (Plugin.to_hex hash)

let emit_processing_root_hash_failed ((module Plugin) : Dac_plugin.t) hash
    errors =
  emit processing_root_hash_failed (Plugin.to_hex hash, errors)

let emit_signature_pushed_to_coordinator signature =
  emit new_signature_pushed_to_coordinator signature

let emit_no_committee_member_address = emit no_committee_member_address

let emit_cannot_retrieve_keys_from_address address =
  emit cannot_retrieve_keys_from_address address

let fetched_missing_page =
  declare_1
    ~section
    ~name:"missing_page_fetched"
    ~msg:"Successfully fetched missing page for hash: {hash}"
    ~level:Notice
    ("hash", Dac_plugin.raw_hash_encoding)

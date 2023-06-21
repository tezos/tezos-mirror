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
    ~msg:"Starting the DAL node"
    ~level:Notice
    ()

let shutdown_node =
  declare_1
    ~section
    ~name:"stopping_dal_node"
    ~msg:"Stopping DAL node"
    ~level:Notice
    ("exit_status", Data_encoding.int8)

let store_is_ready =
  declare_0
    ~section
    ~name:"dal_node_store_is_ready"
    ~msg:"The DAL node store is ready"
    ~level:Notice
    ()

let rpc_server_is_ready =
  declare_1
    ~section
    ~name:"dal_node_rpc_server_is_ready"
    ~msg:"The DAL node is listening to {point}"
    ~level:Notice
    ("point", P2p_point.Id.encoding)

let node_is_ready =
  declare_0
    ~section
    ~name:"dal_node_is_ready"
    ~msg:"The DAL node is ready"
    ~level:Notice
    ()

let data_dir_not_found =
  declare_1
    ~section
    ~name:"dal_node_no_data_dir"
    ~msg:
      "The DAL node data directory {path} doesn't exists. Create using: \
       init-config --data-dir={path} "
    ~level:Error
    ("path", Data_encoding.(string))

let fetched_slot =
  declare_2
    ~section
    ~name:"fetched_slot"
    ~msg:"Slot fetched: size {size}, shards {shards}"
    ~level:Notice
    ("size", Data_encoding.int31)
    ("shards", Data_encoding.int31)

let layer1_node_new_head =
  declare_2
    ~section
    ~name:"dal_node_layer_1_new_head"
    ~msg:"Head of layer 1's node updated to {hash} at level {level}"
    ~level:Notice
    ("hash", Block_hash.encoding)
    ("level", Data_encoding.int32)

let layer1_node_tracking_started =
  declare_0
    ~section
    ~name:"dal_node_layer_1_start_tracking"
    ~msg:"Started tracking layer 1's node"
    ~level:Notice
    ()

let protocol_plugin_resolved =
  declare_1
    ~section
    ~name:"dal_node_plugin_resolved"
    ~msg:"Resolved plugin on protocol {proto_hash}"
    ~level:Notice
    ~pp1:Protocol_hash.pp_short
    ("proto_hash", Protocol_hash.encoding)

let daemon_error =
  declare_1
    ~section
    ~name:"dal_node_daemon_error"
    ~msg:"Daemon thrown an error: {error}"
    ~level:Notice
    ~pp1:Error_monad.pp_print_trace
    ("error", Error_monad.trace_encoding)

let configuration_loaded =
  declare_0
    ~section
    ~name:"configuration_loaded"
    ~msg:"Configuration loaded successfully"
    ~level:Notice
    ()

let stored_slot_content =
  declare_1
    ~section
    ~name:"stored_slot_content"
    ~msg:"Slot stored: commitment {commitment}"
    ~level:Notice
    ("commitment", Cryptobox.Commitment.encoding)

let stored_slot_shards =
  declare_2
    ~section
    ~name:"stored_slot_shards"
    ~msg:"Slot stored: commitment {commitment}, shards {shards}"
    ~level:Notice
    ("commitment", Cryptobox.Commitment.encoding)
    ("shards", Data_encoding.int31)

let decoding_data_failed =
  declare_1
    ~section
    ~name:"decoding_failed"
    ~msg:"Error while decoding a {data_kind} value"
    ~level:Warning
    ("data_kind", Types.kind_encoding)

let loading_shard_data_failed =
  declare_1
    ~section
    ~name:"loading_shard_data_failed"
    ~msg:"Error while reading shard data {message}"
    ~level:Warning
    ("message", Data_encoding.string)

let message_validation_error =
  declare_2
    ~section
    ~name:"message_validation_failed"
    ~msg:
      "Validating message with id {message_id} failed with error \
       {validation_error}"
    ~level:Warning
    ~pp1:Gossipsub.Worker.GS.Message_id.pp
    ("message_id", Gossipsub.message_id_encoding)
    ("validation_error", Data_encoding.string)

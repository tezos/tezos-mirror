(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

module Simple = struct
  include Internal_event.Simple

  let section = ["tx_rollup_node"]

  let preamble_warning =
    declare_0
      ~section
      ~name:"tx_rollup_node_preamble_warning"
      ~msg:
        "this node is primarily being developed for testing purposes at the \
         moment"
      ~level:Warning
      ()

  let configuration_was_written =
    declare_2
      ~section
      ~name:"tx_rollup_node_configuration_written"
      ~msg:"configuration written in {file}"
      ~level:Notice
      ("file", Data_encoding.string)
      ("config", Configuration.encoding)

  let starting_node =
    declare_0
      ~section
      ~name:"tx_rollup_node_starting"
      ~msg:"starting the transaction rollup node"
      ~level:Notice
      ()

  let node_is_ready =
    declare_2
      ~section
      ~name:"tx_rollup_node_is_ready"
      ~msg:"the transaction rollup node is listening to {addr}:{port}"
      ~level:Notice
      ("addr", Data_encoding.string)
      ("port", Data_encoding.uint16)

  let node_is_shutting_down =
    declare_1
      ~section
      ~name:"tx_rollup_node_shutting_down"
      ~msg:"the transaction rollup node is shutting down with code {exit_code}"
      ~level:Notice
      ("exit_code", Data_encoding.int31)

  let cannot_connect =
    declare_1
      ~section
      ~name:"tx_rollup_node_cannot_connect"
      ~msg:"cannot connect to a node, retrying in {delay}s"
      ~level:Warning
      ("delay", Data_encoding.float)

  let connection_lost =
    declare_0
      ~section
      ~name:"tx_rollup_node_connection_lost"
      ~msg:"connection to the node has been lost"
      ~level:Warning
      ()

  let new_block =
    declare_1
      ~section
      ~name:"tx_rollup_node_new_block"
      ~msg:"new block with hash: {block_hash}"
      ~level:Notice
      ("block_hash", Block_hash.encoding)

  let processing_block =
    declare_2
      ~section
      ~name:"tx_rollup_node_processing_block"
      ~msg:"processing block: {block_hash} (pred: {predecessor_hash})"
      ~level:Notice
      ("block_hash", Block_hash.encoding)
      ("predecessor_hash", Block_hash.encoding)

  let block_processed =
    declare_1
      ~section
      ~name:"oru_processed"
      ~msg:"block processed with hash: {block_hash}"
      ~level:Notice
      ("block_hash", Block_hash.encoding)

  let block_already_seen =
    declare_1
      ~section
      ~name:"tx_rollup_node_block_already_seen"
      ~msg:
        "the block {block_hash} has already been seen, nothing more to be done"
      ~level:Notice
      ("block_hash", Block_hash.encoding)

  let processing_block_predecessor =
    declare_1
      ~section
      ~name:"tx_rollup_node_processing_block_predecessor"
      ~msg:"processing block predecessor: {predecessor_hash}"
      ~level:Notice
      ("predecessor_hash", Block_hash.encoding)

  let messages_application =
    declare_1
      ~section
      ~name:"tx_rollup_node_messages_application"
      ~msg:"has {number} messages to apply"
      ~level:Notice
      ("number", Data_encoding.int31)

  let inbox_stored =
    declare_3
      ~section
      ~name:"tx_rollup_node_inbox_stored"
      ~msg:
        "an inbox with size {cumulated_size} has been stored for {block_hash}: \
         {messages}"
      ~level:Notice
      ("block_hash", Block_hash.encoding)
      ( "messages",
        Data_encoding.list Protocol.Alpha_context.Tx_rollup_message.encoding )
      ("cumulated_size", Data_encoding.int31)

  let irmin_store_loaded =
    declare_1
      ~section
      ~name:"tx_rollup_node_irmin_store_loaded"
      ~msg:"an Irmin store has been loaded from {data_dir}"
      ~level:Notice
      ("data_dir", Data_encoding.string)

  let new_tezos_head =
    declare_1
      ~section
      ~name:"tx_rollup_node_new_tezos_head"
      ~msg:"a new tezos head ({tezos_head}) is stored"
      ~level:Notice
      ("tezos_head", Block_hash.encoding)
end

let preamble_warning = Simple.(emit preamble_warning)

let configuration_was_written ~into ~config =
  Simple.(emit configuration_was_written (into, config)) >|= ok

let starting_node () = Simple.(emit starting_node) () >|= ok

let node_is_ready ~rpc_addr ~rpc_port =
  Simple.(emit node_is_ready (rpc_addr, rpc_port)) >|= ok

let node_is_shutting_down ~exit_status =
  Simple.(emit node_is_shutting_down) exit_status

let cannot_connect ~delay = Simple.(emit cannot_connect) delay

let connection_lost = Simple.(emit connection_lost)

let new_block block_hash = Simple.(emit new_block) block_hash >|= ok

let processing_block ~block_hash ~predecessor_hash =
  Simple.(emit processing_block) (block_hash, predecessor_hash) >|= ok

let block_already_seen block_hash =
  Simple.(emit block_already_seen) block_hash >|= ok

let processing_block_predecessor predecessor_hash =
  Simple.(emit processing_block_predecessor) predecessor_hash >|= ok

let messages_application number =
  Simple.(emit messages_application) number >|= ok

let block_processed block_hash = Simple.(emit block_processed) block_hash >|= ok

let inbox_stored ~block_hash ~messages ~cumulated_size =
  Simple.(emit inbox_stored) (block_hash, messages, cumulated_size) >|= ok

let irmin_store_loaded data_dir =
  Simple.(emit irmin_store_loaded) data_dir >|= ok

let new_tezos_head tezos_head = Simple.(emit new_tezos_head) tezos_head >|= ok

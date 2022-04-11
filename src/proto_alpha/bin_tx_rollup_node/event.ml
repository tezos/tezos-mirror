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

let rpc_server_is_ready =
  declare_1
    ~section
    ~name:"tx_rollup_node_rpc_server_is_ready"
    ~msg:"the transaction rollup node RPC server is listening on {addr}"
    ~level:Notice
    ("addr", P2p_point.Id.encoding)

let node_is_ready =
  declare_0
    ~section
    ~name:"tx_rollup_node_is_ready"
    ~msg:"the transaction rollup node is ready"
    ~level:Notice
    ()

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
  declare_2
    ~section
    ~name:"tx_rollup_node_tezos_block_processed"
    ~msg:"tezos block {block_hash} at level {level} was sucessfully processed"
    ~level:Notice
    ("block_hash", Block_hash.encoding)
    ("level", Data_encoding.int32)

let block_already_processed =
  declare_1
    ~section
    ~name:"tx_rollup_node_block_already_processed"
    ~msg:
      "the block {block_hash} has already been processed, nothing more to be \
       done"
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

let rollup_block =
  declare_3
    ~section
    ~name:"tx_rollup_level"
    ~msg:"Level {level}: L2 block {hash} at Tezos {tezos_hash}"
    ~level:Notice
    ("level", L2block.level_encoding)
    ("hash", L2block.Hash.encoding)
    ("tezos_hash", Block_hash.encoding)

let inbox_stored =
  declare_4
    ~section
    ~name:"tx_rollup_node_inbox_stored"
    ~msg:
      "an inbox with size {cumulated_size} and resulting context hash \
       {context_hash} has been stored for {block_hash}: {messages}"
    ~level:Notice
    ("block_hash", Block_hash.encoding)
    ("messages", Data_encoding.list Inbox.message_encoding)
    ("cumulated_size", Data_encoding.int31)
    ("context_hash", Protocol.Tx_rollup_l2_context_hash.encoding)

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

module Batcher = struct
  let section = section @ ["batcher"]

  let queue =
    declare_1
      ~section
      ~name:"queue"
      ~msg:"adding {tr_hash} to queue"
      ~level:Notice
      ("tr_hash", L2_transaction.Hash.encoding)

  let batch =
    declare_2
      ~section
      ~name:"batch"
      ~msg:"batching {nb_transactions} transactions into {nb_batches} batches"
      ~level:Notice
      ("nb_batches", Data_encoding.int31)
      ("nb_transactions", Data_encoding.int31)

  let no_full_batch =
    declare_0
      ~section
      ~name:"no_full_batch"
      ~msg:"No full batch to inject and we requested so"
      ~level:Info
      ()

  let batch_success =
    declare_0
      ~section
      ~name:"batch_success"
      ~msg:"transactions were successfully batched"
      ~level:Notice
      ()

  let invalid_transaction =
    declare_1
      ~section
      ~name:"invalid_transaction"
      ~msg:"a batch with this only transaction is invalid: {tr}"
      ("tr", L2_transaction.encoding)

  module Worker = struct
    open Batcher_worker_types

    let section = section @ ["worker"]

    let request_failed =
      declare_3
        ~section
        ~name:"request_failed"
        ~msg:"request {view} failed ({worker_status}): {errors}"
        ~level:Warning
        ("view", Request.encoding)
        ~pp1:Request.pp
        ("worker_status", Worker_types.request_status_encoding)
        ~pp2:Worker_types.pp_status
        ("errors", Error_monad.trace_encoding)
        ~pp3:Error_monad.pp_print_trace

    let request_completed_notice =
      declare_2
        ~section
        ~name:"request_completed_notice"
        ~msg:"{view} {worker_status}"
        ~level:Notice
        ("view", Request.encoding)
        ("worker_status", Worker_types.request_status_encoding)
        ~pp1:Request.pp
        ~pp2:Worker_types.pp_status

    let request_completed_debug =
      declare_2
        ~section
        ~name:"request_completed_debug"
        ~msg:"{view} {worker_status}"
        ~level:Debug
        ("view", Request.encoding)
        ("worker_status", Worker_types.request_status_encoding)
        ~pp1:Request.pp
        ~pp2:Worker_types.pp_status
  end
end

module Injector = struct
  open Injector_worker_types

  let section = section @ ["injector"]

  let request_failed =
    declare_3
      ~section
      ~name:"request_failed"
      ~msg:"request {view} failed ({worker_status}): {errors}"
      ~level:Warning
      ("view", Request.encoding)
      ~pp1:Request.pp
      ("worker_status", Worker_types.request_status_encoding)
      ~pp2:Worker_types.pp_status
      ("errors", Error_monad.trace_encoding)
      ~pp3:Error_monad.pp_print_trace

  let request_completed_notice =
    declare_2
      ~section
      ~name:"request_completed_notice"
      ~msg:"{view} {worker_status}"
      ~level:Notice
      ("view", Request.encoding)
      ("worker_status", Worker_types.request_status_encoding)
      ~pp1:Request.pp
      ~pp2:Worker_types.pp_status

  let request_completed_debug =
    declare_2
      ~section
      ~name:"request_completed_debug"
      ~msg:"{view} {worker_status}"
      ~level:Debug
      ("view", Request.encoding)
      ("worker_status", Worker_types.request_status_encoding)
      ~pp1:Request.pp
      ~pp2:Worker_types.pp_status

  let new_tezos_head =
    declare_1
      ~section
      ~name:"new_tezos_head"
      ~msg:"processing new Tezos head {head}"
      ~level:Debug
      ("head", Block_hash.encoding)

  let injecting_pending =
    declare_1
      ~section
      ~name:"injecting_pending"
      ~msg:"Injecting {count} pending operations"
      ~level:Notice
      ("count", Data_encoding.int31)

  let pp_operations_list ppf operations =
    Format.fprintf
      ppf
      "@[%a@]"
      (Format.pp_print_list L1_operation.pp)
      operations

  let pp_operations_hash_list ppf operations =
    Format.fprintf
      ppf
      "@[%a@]"
      (Format.pp_print_list L1_operation.Hash.pp)
      operations

  let injecting_operations =
    declare_1
      ~section
      ~name:"injecting_operations"
      ~msg:"Injecting operations: {operations}"
      ~level:Notice
      ("operations", Data_encoding.list L1_operation.encoding)
      ~pp1:pp_operations_list

  let simulating_operations =
    declare_1
      ~section
      ~name:"simulating_operations"
      ~msg:"Simulating operations: {operations}"
      ~level:Debug
      ("operations", Data_encoding.list L1_operation.encoding)
      ~pp1:pp_operations_list

  let dropping_operation =
    declare_2
      ~section
      ~name:"dropping_operation"
      ~msg:"Dropping failing operation {operation} failing with {error}"
      ~level:Notice
      ("operation", L1_operation.encoding)
      ~pp1:L1_operation.pp
      ("error", Environment.Error_monad.trace_encoding)
      ~pp2:Environment.Error_monad.pp_trace

  let injected =
    declare_1
      ~section
      ~name:"injected"
      ~msg:"Injected in {oph}"
      ~level:Notice
      ("oph", Operation_hash.encoding)

  let add_pending =
    declare_1
      ~section
      ~name:"add_pending"
      ~msg:"Add {operation} to pending"
      ~level:Notice
      ("operation", L1_operation.encoding)
      ~pp1:L1_operation.pp

  let included =
    declare_3
      ~section
      ~name:"included"
      ~msg:"Included operations of {block} at level {level}: {operations}"
      ~level:Notice
      ("block", Block_hash.encoding)
      ("level", Data_encoding.int32)
      ("operations", Data_encoding.list L1_operation.Hash.encoding)
      ~pp3:pp_operations_hash_list

  let revert_operations =
    declare_1
      ~section
      ~name:"revert_operations"
      ~msg:"Reverting operations: {operations}"
      ~level:Notice
      ("operations", Data_encoding.list L1_operation.Hash.encoding)
      ~pp1:pp_operations_hash_list

  let confirmed_level =
    declare_1
      ~section
      ~name:"confirmed_level"
      ~msg:"Confirmed Tezos level {level}"
      ~level:Notice
      ("level", Data_encoding.int32)

  let confirmed_operations =
    declare_2
      ~section
      ~name:"confirmed_operations"
      ~msg:"Confirmed operations of level {level}: {operations}"
      ~level:Notice
      ("level", Data_encoding.int32)
      ("operations", Data_encoding.list L1_operation.Hash.encoding)
      ~pp2:pp_operations_hash_list

  let wait =
    declare_1
      ~section
      ~name:"wait"
      ~msg:"Waiting {delay} seconds to trigger injection"
      ~level:Notice
      ("delay", Data_encoding.float)
end

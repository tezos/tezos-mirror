(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)
open Internal_event.Simple

let section = Events.section @ ["tx_queue"]

let is_ready =
  declare_0
    ~section
    ~name:"tx_queue_is_ready"
    ~level:Notice
    ~msg:"tx queue is ready"
    ()

let shutdown =
  declare_0
    ~section
    ~name:"shutting_down_tx_queue"
    ~msg:"stopping the tx queue"
    ~level:Notice
    ()

let cleared =
  declare_0
    ~section
    ~name:"tx_queue_cleared"
    ~msg:"cleared the tx queue"
    ~level:Notice
    ()

let injecting_transactions =
  declare_1
    ~section
    ~name:"tx_queue_injecting_transaction"
    ~msg:"injecting {n} transactions"
    ~level:Info
    ("n", Data_encoding.int31)

let injecting_transactions_failed =
  declare_1
    ~section
    ~name:"tx_queue_injecting_transactions_failed"
    ~msg:"injecting transactions failed: {error}"
    ~level:Error
    ("error", Events.trace_encoding)

let rpc_error =
  declare_2
    ~section
    ~name:"tx_queue_rpc_error"
    ~msg:"an RPC produced the error :\n\tcode:{code},\n\tmessage:{message}"
    ~level:Error
    ("code", Data_encoding.int32)
    ("message", Data_encoding.string)

let callback_error =
  declare_1
    ~section
    ~name:"tx_queue_callback_error"
    ~msg:"A callback produced an error: [<v 2>@{error}@]"
    ~level:Error
    ~pp1:Error_monad.pp_print_trace
    ("error", Events.trace_encoding)

let add_transaction =
  declare_1
    ~section
    ~name:"tx_queue_add_transaction"
    ~msg:"transaction {tx_hash} received"
    ~level:Debug
    ~pp1:(fun fmt Ethereum_types.(Hash (Hex h)) -> Format.fprintf fmt "%10s" h)
    ("tx_hash", Ethereum_types.hash_encoding)

let transaction_dropped =
  declare_1
    ~section
    ~name:"tx_queue_transaction_dropped"
    ~msg:"transaction {tx_hash} dropped"
    ~level:Debug
    ~pp1:(fun fmt Ethereum_types.(Hash (Hex h)) -> Format.fprintf fmt "%10s" h)
    ("tx_hash", Ethereum_types.hash_encoding)

let transaction_confirmed =
  declare_1
    ~section
    ~name:"tx_queue_transaction_confirmed"
    ~msg:"transaction {tx_hash} confirmed"
    ~level:Debug
    ~pp1:(fun fmt Ethereum_types.(Hash (Hex h)) -> Format.fprintf fmt "%10s" h)
    ("tx_hash", Ethereum_types.hash_encoding)

let missing_tx_object =
  declare_1
    ~section
    ~name:"tx_queue_missing_tx_object"
    ~msg:"transaction {tx_hash} has no associated object"
    ~level:Error
    ~pp1:(fun fmt Ethereum_types.(Hash (Hex h)) -> Format.fprintf fmt "%10s" h)
    ("tx_hash", Ethereum_types.hash_encoding)

let is_ready () = emit is_ready ()

let shutdown () = emit shutdown ()

let cleared () = emit cleared ()

let injecting_transactions n = emit injecting_transactions n

let injecting_transactions_failed err = emit injecting_transactions_failed err

let add_transaction tx = emit add_transaction tx

let transaction_dropped tx = emit transaction_dropped tx

let transaction_confirmed tx = emit transaction_confirmed tx

let rpc_error (error : Rpc_encodings.JSONRPC.error) =
  emit rpc_error (Int32.of_int error.code, error.message)

let callback_error__dont_wait__use_with_care (error : tztrace) =
  emit__dont_wait__use_with_care callback_error error

let callback_error (error : tztrace) = emit callback_error error

let missing_tx_object tx = emit missing_tx_object tx

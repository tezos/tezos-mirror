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

let injecting_transactions =
  declare_1
    ~name:"tx_queue_injecting_transaction"
    ~msg:"injecting {n} transactions"
    ~level:Info
    ("n", Data_encoding.int31)

let rpc_error =
  declare_2
    ~section
    ~name:"tx_queue_rpc_error"
    ~msg:"an RPC produced the error :\n\tcode:{code},\n\tmessage:{message}"
    ~level:Error
    ("code", Data_encoding.int32)
    ("message", Data_encoding.string)

let add_transaction =
  declare_1
    ~name:"tx_queue_add_transaction"
    ~msg:"transaction {tx_hash} received"
    ~level:Debug
    ~pp1:(fun fmt Ethereum_types.(Hash (Hex h)) -> Format.fprintf fmt "%10s" h)
    ("tx_hash", Ethereum_types.hash_encoding)

let is_ready () = emit is_ready ()

let shutdown () = emit shutdown ()

let injecting_transactions n = emit injecting_transactions n

let add_transaction tx = emit add_transaction tx

let rpc_error (error : Rpc_encodings.JSONRPC.error) =
  emit rpc_error (Int32.of_int error.code, error.message)

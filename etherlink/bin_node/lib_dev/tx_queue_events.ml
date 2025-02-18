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
  declare_0 ~section ~name:"is_ready" ~level:Notice ~msg:"Tx queue is ready" ()

let injecting_transactions =
  declare_1
    ~name:"injecting_transaction"
    ~msg:"Injecting {n} transactions"
    ~level:Info
    ("n", Data_encoding.int31)

let rpc_error =
  declare_2
    ~section
    ~name:"rpc_error"
    ~msg:"An RPC produced the error :\n code:{code},\n message:{message}"
    ~level:Error
    ("code", Data_encoding.int32)
    ("message", Data_encoding.string)

let is_ready () = emit is_ready ()

let injecting_transactions n = emit injecting_transactions n

let rpc_error (error : Rpc_encodings.JSONRPC.error) =
  emit rpc_error (Int32.of_int error.code, error.message)

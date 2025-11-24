(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Simple

let section = Baking_events_section.section @ ["rpc"]

let error_while_monitoring_heads =
  declare_1
    ~section
    ~name:"error_while_monitoring_heads"
    ~level:Error
    ~msg:"error while monitoring heads {trace}"
    ~pp1:Error_monad.pp_print_trace
    ("trace", Error_monad.trace_encoding)

let error_while_monitoring_valid_proposals =
  declare_1
    ~section
    ~name:"error_while_monitoring_valid_proposals"
    ~level:Error
    ~msg:"error while monitoring valid proposals {trace}"
    ~pp1:Error_monad.pp_print_trace
    ("trace", Error_monad.trace_encoding)

let chain_id =
  declare_1
    ~section
    ~name:"node_chain_id"
    ~level:Info
    ~msg:"Running baker with chain id: {chain_id}"
    ("chain_id", Chain_id.encoding)

let stalling_rpc =
  declare_2
    ~section
    ~name:"stalling_rpc"
    ~level:Warning
    ~msg:"RPC {rpc_name} has not answered in the last {seconds} seconds"
    ("rpc_name", Data_encoding.string)
    ("seconds", Data_encoding.float)

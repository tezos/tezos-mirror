(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Simple

let section = ["proxy_server_run"]

let starting_rpc_server =
  declare_2
    ~section
    ~name:"starting_proxy_rpc_server"
    ~msg:"starting proxy RPC server on {host}:{port}"
    ~level:Notice
    ("host", Data_encoding.string)
    ("port", Data_encoding.uint16)

let shutting_down_proxy_server =
  declare_0
    ~section
    ~name:"shutting_down_proxy_server"
    ~msg:"shutting down the proxy server"
    ~level:Notice
    ()

let shutting_down_rpc_server =
  declare_0
    ~section
    ~name:"shutting_down_rpc_server"
    ~msg:"shutting down the proxy RPC server"
    ~level:Notice
    ()

let accepted_conn_proxy_server =
  declare_2
    ~section
    ~name:"accepted_conn"
    ~msg:"[pid:{pid}] ({con}) accepted connection at proxy server"
    ~level:Debug
    ("pid", Data_encoding.int32)
    ("con", Data_encoding.string)

let conn_closed_proxy_server =
  declare_2
    ~section
    ~name:"conn_closed"
    ~msg:"[pid:{pid}] ({con}) got connection closed at proxy server"
    ~level:Debug
    ("pid", Data_encoding.int32)
    ("con", Data_encoding.string)

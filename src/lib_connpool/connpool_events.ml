(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Simple

let section = ["connection_pool"]

let new_connection =
  declare_1
    ~section
    ~name:"new_connection"
    ~msg:"new connection to {endpoint} established"
    ~level:Info
    ~pp1:Format.pp_print_string
    ("endpoint", Data_encoding.string)

let disposed_connection =
  declare_1
    ~section
    ~name:"disposed_connection"
    ~msg:"one connection to {endpoint} was disposed of"
    ~level:Info
    ~pp1:Format.pp_print_string
    ("endpoint", Data_encoding.string)

let new_connection endpoint = emit new_connection (Uri.to_string endpoint)

let disposed_connection endpoint =
  emit disposed_connection (Uri.to_string endpoint)

(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

let section = ["rpc-process"]

let starting_rpc_server =
  declare_4
    ~section
    ~name:"starting_rpc_server"
    ~msg:"starting RPC server on {host}:{port} (acl = {acl_policy})"
    ~level:Notice
    ("host", Data_encoding.string)
    ("port", Data_encoding.uint16)
    ("tls", Data_encoding.bool)
    ("acl_policy", Data_encoding.string)

let forwarding_rpc =
  declare_1
    ~section
    ~name:"forwarding_rpc"
    ~msg:"forwarding to the node: {uri}"
    ~level:Debug
    ("uri", Data_encoding.string)

let locally_handled_rpc =
  declare_1
    ~section
    ~name:"locally_handled_rpc"
    ~msg:"locally handled: {uri}"
    ~level:Debug
    ("uri", Data_encoding.string)

(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023-2024 Nomadic Labs. <contact@nomadic-labs.com>          *)
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

let section = ["rpc"; "process"]

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

let daemon_error =
  declare_1
    ~section
    ~name:"octez_rpc_server_daemon_error"
    ~msg:"Daemon thrown an error: {error}"
    ~level:Notice
    ~pp1:Error_monad.pp_print_trace
    ("error", Error_monad.trace_encoding)

let new_head =
  declare_1
    ~section
    ~name:"new_head"
    ~msg:"New head received at level ({level})"
    ~level:Info
    ("level", Data_encoding.int32)

let new_applied_block =
  declare_1
    ~section
    ~name:"new_applied_block"
    ~msg:"New applied block received ({level})"
    ~level:Info
    ("level", Data_encoding.int32)

let start_synchronization =
  declare_2
    ~section
    ~name:"start_synchronization"
    ~msg:"Starting store synchronization for block {level} ({hash})"
    ~level:Info
    ("level", Data_encoding.int32)
    ~pp2:Block_hash.pp_short
    ("hash", Block_hash.encoding)

let shutting_head_daemon =
  declare_0
    ~section
    ~name:"shutting_head_daemon"
    ~msg:"shutting down head daemon"
    ~level:Info
    ()

let store_synchronized_on_head =
  declare_2
    ~section
    ~name:"store_synchronized_on_head"
    ~msg:"Store synchronized on head {hash} ({level})"
    ~level:Notice
    ~pp1:Block_hash.pp_short
    ("hash", Block_hash.encoding)
    ("level", Data_encoding.int32)

let enable_http_cache_headers_for_external =
  declare_0
    ~section
    ~name:"enable_http_cache_headers_for_external"
    ~msg:"HTTP cache headers enabled for external rpc server"
    ~level:Notice
    ()

(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) Nomadic Labs, <contact@nomadic-labs.com>                    *)
(* Copyright (c) Functori, <contact@functori.com>                            *)
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

  let section = ["lib_crawler"; "layer_1"]

  let declare_0 ~name ~msg ?level () =
    declare_1
      ~section
      ~name
      ~msg:("[{name}] " ^ msg)
      ?level
      ("name", Data_encoding.string)
      ~pp1:Format.pp_print_string

  let declare_1 ~name ~msg ?level ?pp1 enc1 =
    declare_2
      ~section
      ~name
      ~msg:("[{name}] " ^ msg)
      ?level
      ("name", Data_encoding.string)
      enc1
      ~pp1:Format.pp_print_string
      ?pp2:pp1

  let declare_2 ~name ~msg ?level ?pp1 ?pp2 enc1 enc2 =
    declare_3
      ~section
      ~name
      ~msg:("[{name}] " ^ msg)
      ?level
      ("name", Data_encoding.string)
      enc1
      enc2
      ~pp1:Format.pp_print_string
      ?pp2:pp1
      ?pp3:pp2

  let starting =
    declare_0
      ~name:"lib_crawler_layer_1_starting"
      ~msg:"Starting layer 1 tracker of the smart rollup node"
      ~level:Notice
      ()

  let stopping =
    declare_0
      ~name:"lib_crawler_layer_1_stopping"
      ~msg:"Stopping layer 1 tracker of the smart rollup node"
      ~level:Notice
      ()

  let connection_lost =
    declare_0
      ~name:"lib_crawler_connection_lost"
      ~msg:"connection to the node has been lost"
      ~level:Warning
      ()

  let connection_timeout =
    declare_1
      ~name:"lib_crawler_connection_timeout"
      ~msg:
        "Connection to the node has timeout after {timeout}s waiting for a new \
         head"
      ~level:Warning
      ("timeout", Data_encoding.float)

  let connection_error =
    declare_1
      ~name:"lib_crawler_connection_error"
      ~msg:"Connection error: {error}"
      ~level:Warning
      ("error", trace_encoding)
      ~pp1:pp_print_trace

  let cannot_connect =
    declare_2
      ~name:"lib_crawler_cannot_connect"
      ~msg:"cannot connect to Tezos node ({count}) {error}"
      ~level:Warning
      ("count", Data_encoding.int31)
      ("error", trace_encoding)
      ~pp2:pp_print_trace

  let wait_reconnect =
    declare_1
      ~name:"lib_crawler_wait_reconnect"
      ~msg:"Retrying to connect in {delay}s"
      ~level:Warning
      ("delay", Data_encoding.float)

  let switched_new_head =
    declare_2
      ~name:"lib_crawler_layer_1_new_head"
      ~msg:"Layer 1 node has switched to head {hash} at level {level}"
      ~level:Info
      ("hash", Block_hash.encoding)
      ("level", Data_encoding.int32)
end

let starting ~name = Simple.(emit starting) name

let stopping ~name = Simple.(emit stopping) name

let connection_lost ~name = Simple.(emit connection_lost) name

let connection_timeout ~name ~timeout =
  Simple.(emit connection_timeout) (name, timeout)

let connection_error ~name error = Simple.(emit connection_error) (name, error)

let cannot_connect ~name ~count error =
  Simple.(emit cannot_connect) (name, count, error)

let wait_reconnect ~name delay = Simple.(emit wait_reconnect) (name, delay)

let switched_new_head ~name hash level =
  Simple.(emit switched_new_head) (name, hash, level)

(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

  let section = [Protocol.name; "smart_rollup_node"]

  let starting_node =
    declare_0
      ~section
      ~name:"starting_smart_rollup_node"
      ~msg:"Starting the smart rollup node"
      ~level:Notice
      ()

  let shutdown_node =
    declare_1
      ~section
      ~name:"stopping_smart_rollup_node"
      ~msg:"Stopping the smart rollup node"
      ~level:Notice
      ("exit_status", Data_encoding.int8)

  let node_is_ready =
    declare_2
      ~section
      ~name:"smart_rollup_node_is_ready"
      ~msg:"The smart rollup node is listening to {addr}:{port}"
      ~level:Notice
      ("addr", Data_encoding.string)
      ("port", Data_encoding.uint16)

  let rollup_exists =
    declare_2
      ~section
      ~name:"smart_rollup_node_knows_its_rollup"
      ~msg:
        "The smart rollup node is interacting with rollup {addr} of kind {kind}"
      ~level:Notice
      ("addr", Protocol.Alpha_context.Sc_rollup.Address.encoding)
      ("kind", Data_encoding.string)

  let connection_lost =
    declare_0
      ~section
      ~name:"smart_rollup_daemon_connection_lost"
      ~msg:"connection to the node has been lost"
      ~level:Warning
      ()

  let cannot_connect =
    declare_2
      ~section
      ~name:"smart_rollup_daemon_cannot_connect"
      ~msg:"cannot connect to Tezos node ({count}) {error}"
      ~level:Warning
      ("count", Data_encoding.int31)
      ("error", trace_encoding)
      ~pp2:pp_print_trace

  let wait_reconnect =
    declare_1
      ~section
      ~name:"smart_rollup_daemon_wait_reconnect"
      ~msg:"Retrying to connect in {delay}s"
      ~level:Warning
      ("delay", Data_encoding.float)

  let starting_metrics_server =
    declare_2
      ~section
      ~name:"starting_metrics_server"
      ~msg:"starting metrics server on {host}:{port}"
      ~level:Notice
      ("host", Data_encoding.string)
      ("port", Data_encoding.uint16)

  let metrics_ended =
    declare_1
      ~section
      ~name:"metrics_ended"
      ~level:Error
      ~msg:"metrics server ended with error {stacktrace}"
      ("stacktrace", Data_encoding.string)

  let kernel_debug =
    declare_1
      ~section
      ~name:"kernel_debug"
      ~level:Info
      ~msg:"{log}"
      ("log", Data_encoding.string)
      ~pp1:Format.pp_print_string

  let warn_dal_enabled_no_node =
    declare_0
      ~section
      ~name:"dal_enabled_no_node"
      ~level:Warning
      ~msg:
        "Warning: DAL is enabled in the protocol but no DAL node was provided \
         for the rollup node."
      ()

  let waiting_first_block =
    declare_0
      ~section
      ~name:"waiting_first_block"
      ~level:Notice
      ~msg:
        (Format.asprintf
           "Waiting for first block of protocol %a to appear."
           Protocol_hash.pp
           Protocol.hash)
      ()

  let received_first_block =
    declare_1
      ~section
      ~name:"received_first_block"
      ~level:Notice
      ~msg:
        (Format.asprintf
           "First block of protocol %a received: {block}."
           Protocol_hash.pp
           Protocol.hash)
      ("block", Block_hash.encoding)

  let acquiring_lock =
    declare_0
      ~section
      ~name:"acquiring_lock"
      ~level:Notice
      ~msg:"Acquiring lock on data directory."
      ()
end

let starting_node = Simple.(emit starting_node)

let shutdown_node exit_status = Simple.(emit shutdown_node exit_status)

let node_is_ready ~rpc_addr ~rpc_port =
  Simple.(emit node_is_ready (rpc_addr, rpc_port))

let rollup_exists ~addr ~kind =
  let kind = Protocol.Alpha_context.Sc_rollup.Kind.to_string kind in
  Simple.(emit rollup_exists (addr, kind))

let connection_lost () = Simple.(emit connection_lost) ()

let cannot_connect ~count error = Simple.(emit cannot_connect) (count, error)

let wait_reconnect delay = Simple.(emit wait_reconnect) delay

let starting_metrics_server ~host ~port =
  Simple.(emit starting_metrics_server) (host, port)

let metrics_ended error = Simple.(emit metrics_ended) error

let metrics_ended_dont_wait error =
  Simple.(emit__dont_wait__use_with_care metrics_ended) error

let kernel_debug msg = Simple.(emit kernel_debug) msg

let kernel_debug_dont_wait msg =
  Simple.(emit__dont_wait__use_with_care kernel_debug) msg

let warn_dal_enabled_no_node () = Simple.(emit warn_dal_enabled_no_node) ()

let waiting_first_block () = Simple.(emit waiting_first_block) ()

let received_first_block b = Simple.(emit received_first_block) b

let acquiring_lock () = Simple.(emit acquiring_lock) ()

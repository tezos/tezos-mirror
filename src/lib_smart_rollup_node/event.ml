(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

  let section = ["smart_rollup_node"]

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
      ~level:Info
      ("addr", Octez_smart_rollup.Address.encoding)
      ("kind", Data_encoding.string)

  let starting_metrics_server =
    declare_2
      ~section
      ~name:"starting_metrics_server"
      ~msg:"Starting metrics server on {host}:{port}"
      ~level:Notice
      ("host", Data_encoding.string)
      ("port", Data_encoding.uint16)

  let metrics_ended =
    declare_1
      ~section
      ~name:"metrics_ended"
      ~level:Error
      ~msg:"[Error]: Metrics server ended with error {stacktrace}"
      ("stacktrace", Data_encoding.string)

  let kernel_debug =
    declare_1
      ~section
      ~name:"kernel_debug"
      ~level:Info
      ~msg:"{log}"
      ("log", Data_encoding.string)
      ~pp1:Format.pp_print_string

  let simulation_kernel_debug =
    declare_1
      ~section
      ~name:"simulation_kernel_debug"
      ~level:Info
      ~msg:"[simulation] {log}"
      ("log", Data_encoding.string)
      ~pp1:Format.pp_print_string

  let warn_dal_enabled_no_node =
    declare_0
      ~section
      ~name:"dal_enabled_no_node"
      ~level:Warning
      ~msg:
        "[Warning]: DAL is enabled in the protocol but no DAL node was \
         provided for the rollup node"
      ()

  let waiting_first_block =
    declare_1
      ~section
      ~name:"smart_rollup_node_waiting_first_block"
      ~level:Info
      ~msg:"Waiting for first block of protocol {protocol} to appear"
      ("protocol", Protocol_hash.encoding)

  let received_first_block =
    declare_2
      ~section
      ~name:"smart_rollup_node_received_first_block"
      ~level:Info
      ~msg:"First block of protocol {protocol} received: {block}"
      ("block", Block_hash.encoding)
      ("protocol", Protocol_hash.encoding)

  let detected_protocol_migration =
    declare_0
      ~section
      ~name:"detected_protocol_migration"
      ~level:Notice
      ~msg:"Detected protocol migration, the rollup node will now stop"
      ()

  let acquiring_lock =
    declare_0
      ~section
      ~name:"acquiring_lock"
      ~level:Info
      ~msg:"Acquiring lock on data directory."
      ()

  let calling_gc =
    declare_2
      ~section
      ~name:"calling_gc"
      ~level:Info
      ~msg:
        "Garbage collection started for level {gc_level} at head level \
         {head_level}"
      ("gc_level", Data_encoding.int32)
      ("head_level", Data_encoding.int32)

  let starting_context_gc =
    declare_1
      ~section
      ~name:"starting_context_gc"
      ~level:Info
      ~msg:"Starting context garbage collection for commit {context_hash}"
      ("context_hash", Smart_rollup_context_hash.encoding)
      ~pp1:Smart_rollup_context_hash.pp

  let context_gc_already_launched =
    declare_0
      ~section
      ~name:"gc_already_launched"
      ~level:Info
      ~msg:
        "An attempt to launch context GC was made, but a previous GC run has \
         not yet finished. No action was taken"
      ()

  let ending_context_gc =
    declare_2
      ~section
      ~name:"ending_context_gc"
      ~level:Info
      ~msg:
        "Context garbage collection finished in {duration} (finalised in \
         {finalisation})"
      ~pp1:Time.System.Span.pp_hum
      ("duration", Time.System.Span.encoding)
      ~pp2:Time.System.Span.pp_hum
      ("finalisation", Time.System.Span.encoding)

  let context_gc_failure =
    declare_1
      ~section
      ~name:"gc_failure"
      ~level:Warning
      ~msg:"[Warning] Context garbage collection failed: {error}"
      ("error", Data_encoding.string)

  let context_gc_launch_failure =
    declare_1
      ~section
      ~name:"context_gc_launch_failure"
      ~level:Warning
      ~msg:"[Warning] Context garbage collection launch failed: {error}"
      ("error", Data_encoding.string)

  let gc_levels_storage_failure =
    declare_0
      ~section
      ~name:"gc_levels_storage_failure"
      ~level:Warning
      ~msg:"[Warning] An attempt to write GC level information to disk failed"
      ()

  let convert_history_mode =
    declare_2
      ~section
      ~name:"convert_history_mode"
      ~level:Notice
      ~msg:
        "Converting the {old_history_mode} rollup node into an \
         {new_history_mode} rollup node"
      ("old_history_mode", Configuration.history_mode_encoding)
      ("new_history_mode", Configuration.history_mode_encoding)

  let gc_finished =
    declare_2
      ~section
      ~name:"gc_finished"
      ~level:Info
      ~msg:
        "Garbage collection finished for level {gc_level} at head level \
         {head_level}"
      ("gc_level", Data_encoding.int32)
      ("head_level", Data_encoding.int32)
end

let starting_node = Simple.(emit starting_node)

let shutdown_node exit_status = Simple.(emit shutdown_node exit_status)

let node_is_ready ~rpc_addr ~rpc_port =
  Simple.(emit node_is_ready (rpc_addr, rpc_port))

let rollup_exists ~addr ~kind =
  let kind = Octez_smart_rollup.Kind.to_string kind in
  Simple.(emit rollup_exists (addr, kind))

let starting_metrics_server ~host ~port =
  Simple.(emit starting_metrics_server) (host, port)

let metrics_ended error = Simple.(emit metrics_ended) error

let metrics_ended_dont_wait error =
  Simple.(emit__dont_wait__use_with_care metrics_ended) error

let kernel_debug msg = Simple.(emit kernel_debug) msg

let simulation_kernel_debug msg = Simple.(emit simulation_kernel_debug) msg

let kernel_debug_dont_wait msg =
  Simple.(emit__dont_wait__use_with_care kernel_debug) msg

let warn_dal_enabled_no_node () = Simple.(emit warn_dal_enabled_no_node) ()

let waiting_first_block p = Simple.(emit waiting_first_block) p

let received_first_block b p = Simple.(emit received_first_block) (b, p)

let detected_protocol_migration () =
  Simple.(emit detected_protocol_migration) ()

let acquiring_lock () = Simple.(emit acquiring_lock) ()

let calling_gc ~gc_level ~head_level =
  Simple.(emit calling_gc) (gc_level, head_level)

let starting_context_gc hash = Simple.(emit starting_context_gc) hash

let context_gc_already_launched () =
  Simple.(emit context_gc_already_launched) ()

let ending_context_gc t = Simple.(emit ending_context_gc) t

let context_gc_failure msg = Simple.(emit context_gc_failure) msg

let context_gc_launch_failure msg = Simple.(emit context_gc_launch_failure) msg

let gc_levels_storage_failure () = Simple.(emit gc_levels_storage_failure) ()

let convert_history_mode old_history_mode new_history_mode =
  Simple.(emit convert_history_mode) (old_history_mode, new_history_mode)

let gc_finished ~gc_level ~head_level =
  Simple.(emit gc_finished) (gc_level, head_level)

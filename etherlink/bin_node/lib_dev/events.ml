(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Simple

let section = ["evm_node"; "dev"]

let received_upgrade =
  declare_1
    ~section
    ~name:"received_upgrade"
    ~msg:"Received an upgrade payload: {payload}"
    ~level:Notice
    ("payload", Data_encoding.string)

let pending_upgrade =
  declare_2
    ~section
    ~name:"pending_upgrade"
    ~msg:
      "Pending upgrade to root hash {root_hash} expected to activate at \
       {timestamp}"
    ~level:Notice
    ("root_hash", Ethereum_types.hash_encoding)
    ("timestamp", Time.Protocol.encoding)

let applied_upgrade =
  declare_2
    ~section
    ~name:"applied_upgrade"
    ~msg:"Kernel successfully upgraded to {root_hash} with blueprint {level}"
    ~level:Notice
    ("root_hash", Ethereum_types.hash_encoding)
    ("level", Data_encoding.n)

let failed_upgrade =
  declare_2
    ~section
    ~name:"failed_upgrade"
    ~msg:"Kernel failed to upgrade to {root_hash} with blueprint {level}"
    ~level:Warning
    ("root_hash", Ethereum_types.hash_encoding)
    ("level", Data_encoding.n)

let ignored_kernel_arg =
  declare_0
    ~section
    ~name:"ignored_kernel_arg"
    ~msg:
      "Ignored the kernel command-line argument since the EVM state was \
       already initialized"
    ~level:Warning
    ()

let catching_up_evm_event =
  Internal_event.Simple.declare_2
    ~section
    ~name:"catching_up"
    ~msg:"the EVM node is catching up on evm event from {from} to {to}"
    ~level:Notice
    ("from", Data_encoding.int32)
    ("to", Data_encoding.int32)

let event_is_ready =
  Internal_event.Simple.declare_2
    ~section
    ~name:"is_ready"
    ~msg:"the EVM node is listening to {addr}:{port}"
    ~level:Notice
    ("addr", Data_encoding.string)
    ("port", Data_encoding.uint16)

let event_private_server_is_ready =
  declare_2
    ~section
    ~name:"private_server_is_ready"
    ~msg:"the EVM node private RPC server is listening to {addr}:{port}"
    ~level:Notice
    ("addr", Data_encoding.string)
    ("port", Data_encoding.uint16)

let event_shutdown_node =
  Internal_event.Simple.declare_1
    ~section
    ~name:"shutting_down"
    ~msg:"Stopping the EVM node with {exit_status}"
    ~level:Notice
    ("exit_status", Data_encoding.int8)

let event_shutdown_rpc_server ~private_ =
  let server = if private_ then "private" else "public" in
  Internal_event.Simple.declare_0
    ~section
    ~name:("shutting_down_" ^ server ^ "_rpc_server")
    ~msg:("Stopping the" ^ server ^ " RPC server")
    ~level:Notice
    ()

let event_callback_log =
  Internal_event.Simple.declare_3
    ~section
    ~name:"callback_log"
    ~msg:"Uri: {uri}\nMethod: {method}\nBody: {body}\n"
    ~level:Debug
    ("uri", Data_encoding.string)
    ("method", Data_encoding.string)
    ("body", Data_encoding.string)

let event_retrying_connect =
  Internal_event.Simple.declare_2
    ~section
    ~name:"retrying_connect"
    ~msg:"Cannot connect to {endpoint}, retrying in {delay} seconds."
    ~level:Notice
    ("endpoint", Data_encoding.string)
    ("delay", Data_encoding.float)

let event_deprecation_note =
  Internal_event.Simple.declare_1
    ~section
    ~name:"deprecation_note"
    ~msg:"{msg}"
    ~level:Warning
    ("msg", Data_encoding.string)

type kernel_log_kind = Application | Simulation

type kernel_log_level = Debug | Info | Error | Fatal

let kernel_log_kind_to_string = function
  | Application -> "application"
  | Simulation -> "simulation"

let event_kernel_log kind level =
  Internal_event.Simple.declare_1
    ~section:(section @ ["kernel"; kernel_log_kind_to_string kind])
    ~name:
      (Format.sprintf "kernel_log_%s" (Internal_event.Level.to_string level))
    ~msg:"{msg}"
    ~pp1:(fun fmt msg -> Format.pp_print_string fmt (String.trim msg))
    ~level
    ("msg", Data_encoding.string)

let event_kernel_log_application_debug = event_kernel_log Application Debug

let event_kernel_log_simulation_debug = event_kernel_log Simulation Debug

let event_kernel_log_application_info = event_kernel_log Application Notice

let event_kernel_log_simulation_info = event_kernel_log Simulation Info

let event_kernel_log_application_error = event_kernel_log Application Error

let event_kernel_log_simulation_error = event_kernel_log Simulation Error

let event_kernel_log_application_fatal = event_kernel_log Application Fatal

let event_kernel_log_simulation_fatal = event_kernel_log Simulation Fatal

let patched_state =
  Internal_event.Simple.declare_2
    ~level:Warning
    ~section
    ~name:"patched_state"
    ~msg:"Key {key} successfully patched, starting from level {level}"
    ("key", Data_encoding.string)
    ("level", Ethereum_types.quantity_encoding)

let preload_kernel =
  Internal_event.Simple.declare_1
    ~level:Notice
    ~section
    ~name:"preloaded_kernel"
    ~msg:"Kernel {version} successfully preloaded"
    ("version", Data_encoding.string)

let predownload_kernel =
  Internal_event.Simple.declare_1
    ~level:Notice
    ~section
    ~name:"predownload_kernel"
    ~msg:"Kernel {version} successfully predownloaded"
    ("version", Data_encoding.string)

let sandbox_started =
  Internal_event.Simple.declare_1
    ~level:Notice
    ~section
    ~name:"sandbox_started"
    ~msg:"Starting sandbox mode at level {level}"
    ("level", Data_encoding.z)

let cannot_fetch_time_between_blocks =
  Internal_event.Simple.declare_2
    ~level:Error
    ~section
    ~name:"cannot_fetch_time_between_blocks"
    ~msg:
      "Could not fetch the maximum time between blocks from remote EVM \
       endpoint, default to {tbb}: {trace}"
    ~pp1:Configuration.pp_time_between_blocks
    ~pp2:Error_monad.pp_print_trace
    ("tbb", Configuration.time_between_blocks_encoding)
    ("trace", Error_monad.trace_encoding)

let invalid_node_da_fees =
  Internal_event.Simple.declare_4
    ~level:Fatal
    ~section
    ~name:"node_da_fees"
    ~msg:
      "Internal: node gives {node_da_fees} DA fees, whereas kernel gives \
       {kernel_da_fees} on block {block_number} with {call}"
    ~pp1:Z.pp_print
    ~pp2:Z.pp_print
    ~pp3:(Format.pp_print_option Ethereum_types.pp_quantity)
    ~pp4:Data_encoding.Json.pp
    ("node_da_fees", Data_encoding.z)
    ("kernel_da_fees", Data_encoding.z)
    ("block_number", Data_encoding.option Ethereum_types.quantity_encoding)
    ("call", Data_encoding.Json.encoding)

let received_upgrade payload = emit received_upgrade payload

let pending_upgrade (upgrade : Evm_events.Upgrade.t) =
  emit pending_upgrade (upgrade.hash, upgrade.timestamp)

let applied_upgrade root_hash Ethereum_types.(Qty level) =
  emit applied_upgrade (root_hash, level)

let failed_upgrade root_hash Ethereum_types.(Qty level) =
  emit failed_upgrade (root_hash, level)

let ignored_kernel_arg () = emit ignored_kernel_arg ()

let catching_up_evm_event ~from ~to_ = emit catching_up_evm_event (from, to_)

let is_ready ~rpc_addr ~rpc_port = emit event_is_ready (rpc_addr, rpc_port)

let private_server_is_ready ~rpc_addr ~rpc_port =
  emit event_private_server_is_ready (rpc_addr, rpc_port)

let shutdown_rpc_server ~private_ =
  emit (event_shutdown_rpc_server ~private_) ()

let shutdown_node ~exit_status = emit event_shutdown_node exit_status

let callback_log ~uri ~meth ~body = emit event_callback_log (uri, meth, body)

let event_kernel_log ~level ~kind ~msg =
  match (level, kind) with
  | Debug, Application -> emit event_kernel_log_application_debug msg
  | Debug, Simulation -> emit event_kernel_log_simulation_debug msg
  | Info, Application -> emit event_kernel_log_application_info msg
  | Info, Simulation -> emit event_kernel_log_simulation_info msg
  | Error, Application -> emit event_kernel_log_application_error msg
  | Error, Simulation -> emit event_kernel_log_simulation_error msg
  | Fatal, Application -> emit event_kernel_log_application_fatal msg
  | Fatal, Simulation -> emit event_kernel_log_simulation_fatal msg

let retrying_connect ~endpoint ~delay =
  emit event_retrying_connect (Uri.to_string endpoint, delay)

let preload_kernel commit = emit preload_kernel commit

let patched_state key level = emit patched_state (key, level)

let predownload_kernel root_hash = emit predownload_kernel (Hex.show root_hash)

let sandbox_started level = emit sandbox_started level

let cannot_fetch_time_between_blocks fallback trace =
  emit cannot_fetch_time_between_blocks (fallback, trace)

let invalid_node_da_fees ~node_da_fees ~kernel_da_fees ~block_number ~call =
  emit invalid_node_da_fees (node_da_fees, kernel_da_fees, block_number, call)

let deprecation_note msg = emit event_deprecation_note msg

(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Simple

let section = ["evm_node"; "dev"]

let trace_encoding =
  let open Data_encoding in
  conv_with_guard
    (fun o -> Json.construct trace_encoding o)
    (fun json ->
      try Ok (Json.destruct trace_encoding json)
      with _ -> Error "JSON is not a valid trace")
    json

let received_upgrade =
  declare_1
    ~section
    ~name:"received_upgrade"
    ~msg:"received an upgrade payload: {payload}"
    ~level:Notice
    ("payload", Data_encoding.string)

let pending_upgrade =
  declare_2
    ~section
    ~name:"pending_upgrade"
    ~msg:
      "pending upgrade to root hash {root_hash} expected to activate at \
       {timestamp}"
    ~level:Notice
    ("root_hash", Ethereum_types.hash_encoding)
    ("timestamp", Time.Protocol.encoding)

let applied_upgrade =
  declare_2
    ~section
    ~name:"applied_upgrade"
    ~msg:"kernel successfully upgraded to {root_hash} with blueprint {level}"
    ~level:Notice
    ("root_hash", Ethereum_types.hash_encoding)
    ("level", Data_encoding.n)

let failed_upgrade =
  declare_2
    ~section
    ~name:"failed_upgrade"
    ~msg:"kernel failed to upgrade to {root_hash} with blueprint {level}"
    ~level:Warning
    ("root_hash", Ethereum_types.hash_encoding)
    ("level", Data_encoding.n)

let ignored_kernel_arg =
  declare_0
    ~section
    ~name:"ignored_kernel_arg"
    ~msg:
      "ignored the kernel command-line argument since the EVM state was \
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
  Internal_event.Simple.declare_4
    ~section
    ~name:"is_ready"
    ~msg:
      "the EVM node RPC server ({backend}) is listening to {addr}:{port} \
       {websockets}"
    ~level:Notice
    ("addr", Data_encoding.string)
    ("port", Data_encoding.uint16)
    ("backend", Configuration.rpc_server_encoding)
    ("websockets", Data_encoding.bool)
    ~pp4:(fun fmt b ->
      (if b then Format.fprintf else Format.ifprintf) fmt "(websockets enabled)")

let legacy_mode =
  Internal_event.Simple.declare_0
    ~section
    ~name:"legacy_mode"
    ~level:Warning
    ~msg:
      "node is using the (deprecated) legacy block storage, import a recent \
       snapshot to start using the new block storage"
    ()

let event_private_server_is_ready =
  declare_4
    ~section
    ~name:"private_server_is_ready"
    ~msg:
      "the EVM node private RPC server ({backend}) is listening to \
       {addr}:{port} {websockets}"
    ~level:Notice
    ("addr", Data_encoding.string)
    ("port", Data_encoding.uint16)
    ("backend", Configuration.rpc_server_encoding)
    ("websockets", Data_encoding.bool)
    ~pp4:(fun fmt b ->
      (if b then Format.fprintf else Format.ifprintf) fmt "(websockets enabled)")

let event_rpc_server_error =
  declare_1
    ~section
    ~name:"rpc_server_error"
    ~msg:"RPC server error: {exception}"
    ~level:Error
    ("exception", Data_encoding.string)

let event_shutdown_node =
  Internal_event.Simple.declare_1
    ~section
    ~name:"shutting_down"
    ~msg:"stopping the EVM node with {exit_status}"
    ~level:Notice
    ("exit_status", Data_encoding.int8)

let event_shutdown_rpc_server ~private_ =
  let server = if private_ then "private" else "public" in
  Internal_event.Simple.declare_0
    ~section
    ~name:("shutting_down_" ^ server ^ "_rpc_server")
    ~msg:("stopping the" ^ server ^ " RPC server")
    ~level:Notice
    ()

let event_callback_log =
  Internal_event.Simple.declare_3
    ~section
    ~name:"callback_log"
    ~msg:"uri: {uri}\nmethod: {method}\nbody: {body}\n"
    ~level:Debug
    ("uri", Data_encoding.string)
    ("method", Data_encoding.string)
    ("body", Data_encoding.string)

let event_retrying_connect =
  Internal_event.Simple.declare_2
    ~section
    ~name:"retrying_connect"
    ~msg:"cannot connect to {endpoint}, retrying in {delay} seconds"
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

let missing_chain_id =
  Internal_event.Simple.declare_0
    ~level:Warning
    ~section
    ~name:"missing_chain_id"
    ~msg:"missing chain id: skipping consistency check with selected network"
    ()

let pp_file_size fmt size =
  let pp unit power =
    Format.fprintf
      fmt
      "%d.%d%s"
      (size / power)
      (size / (power / 10) mod 10)
      unit
  in
  if size / 1_000_000_000 > 0 then pp "GB" 1_000_000_000
  else if size / 1_000_000 > 0 then pp "MB" 1_000_000
  else if size / 1_000 > 0 then pp "kB" 1_000
  else Format.fprintf fmt "%dB" size

let downloading_file =
  Internal_event.Simple.declare_2
    ~level:Notice
    ~section
    ~name:"downloading_snapshot"
    ~msg:"downloading {url}{size}"
    ~pp1:Format.pp_print_string (* No elapsing too long URLs. *)
    ~pp2:
      Format.(
        pp_print_option (fun fmt size -> fprintf fmt " (%a)" pp_file_size size))
    ("url", Data_encoding.string)
    ("size", Data_encoding.(option int31))

let download_in_progress =
  Internal_event.Simple.declare_3
    ~level:Notice
    ~section
    ~name:"snapshot_download_in_progress"
    ~msg:"still downloading {url} after {elapsed_time}{progress}"
    ~pp1:(fun fmt url -> Format.fprintf fmt "%s" (Filename.basename url))
    ~pp2:(fun fmt elapsed_time ->
      Format.fprintf fmt "%a" Ptime.Span.pp elapsed_time)
    ~pp3:
      Format.(
        pp_print_option (fun fmt (remaining, percentage) ->
            fprintf
              fmt
              " (%.1f%%, %a remaining)"
              percentage
              pp_file_size
              remaining))
    ("url", Data_encoding.string)
    ("elapsed_time", Time.System.Span.encoding)
    ( "progress",
      Data_encoding.(
        option (obj2 (req "remaining_bytes" int31) (req "percentage" float))) )

let importing_snapshot =
  Internal_event.Simple.declare_0
    ~level:Notice
    ~section
    ~name:"importing_snapshot"
    ~msg:"importing snapshot"
    ()

let extract_snapshot_archive_in_progress =
  Internal_event.Simple.declare_2
    ~level:Notice
    ~section
    ~name:"extract_snapshot_archive_in_progress"
    ~msg:"still extracting archive {name} after {elapsed_time}"
    ~pp1:(fun fmt url -> Format.fprintf fmt "%s" (Filename.basename url))
    ~pp2:(fun fmt elapsed_time ->
      Format.fprintf fmt "%a" Ptime.Span.pp elapsed_time)
    ("name", Data_encoding.string)
    ("elapsed_time", Time.System.Span.encoding)

type download_error = Http_error of Cohttp.Code.status_code | Exn of exn

let download_error_encoding =
  let open Data_encoding in
  let status_code_encoding =
    conv Cohttp.Code.code_of_status Cohttp.Code.status_of_code int31
  in
  union
    [
      case
        (Tag 0)
        ~title:"http_error"
        (obj2
           (req "kind" (constant "http_error"))
           (req "status_code" status_code_encoding))
        (function Http_error code -> Some ((), code) | _ -> None)
        (fun ((), code) -> Http_error code);
      case
        (Tag 1)
        ~title:"exn"
        (obj2 (req "kind" (constant "exception")) (req "description" string))
        (function Exn exn -> Some ((), Printexc.to_string exn) | _ -> None)
        (fun ((), _) -> Stdlib.failwith "encoding should not be used to decode");
    ]

let pp_download_error fmt =
  let open Format in
  function
  | Http_error code ->
      fprintf fmt "HTTP status code '%s'" (Cohttp.Code.string_of_status code)
  | Exn exn -> fprintf fmt "'%s'" (Printexc.to_string exn)

let download_failed =
  Internal_event.Simple.declare_2
    ~level:Error
    ~section
    ~name:"download_failed"
    ~msg:"downloading {url} failed with {error}"
    ~pp1:Format.pp_print_string (* No elapsing too long URLs. *)
    ~pp2:pp_download_error
    ("url", Data_encoding.string)
    ("error", download_error_encoding)

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
    ~msg:"key {key} successfully patched, starting from level {level}"
    ("key", Data_encoding.string)
    ("level", Ethereum_types.quantity_encoding)

let preload_kernel =
  Internal_event.Simple.declare_1
    ~level:Notice
    ~section
    ~name:"preloaded_kernel"
    ~msg:"kernel {version} successfully preloaded"
    ("version", Data_encoding.string)

let predownload_kernel =
  Internal_event.Simple.declare_1
    ~level:Notice
    ~section
    ~name:"predownload_kernel"
    ~msg:"kernel {version} successfully predownloaded"
    ("version", Data_encoding.string)

let sandbox_started =
  Internal_event.Simple.declare_1
    ~level:Notice
    ~section
    ~name:"sandbox_started"
    ~msg:"starting sandbox mode at level {level}"
    ("level", Data_encoding.z)

let cannot_fetch_time_between_blocks =
  Internal_event.Simple.declare_2
    ~level:Error
    ~section
    ~name:"cannot_fetch_time_between_blocks"
    ~msg:
      "could not fetch the maximum time between blocks from remote EVM \
       endpoint, default to {tbb}: {trace}"
    ~pp1:Configuration.pp_time_between_blocks
    ~pp2:Error_monad.pp_print_trace
    ("tbb", Configuration.time_between_blocks_encoding)
    ("trace", trace_encoding)

let invalid_node_da_fees =
  Internal_event.Simple.declare_4
    ~level:Fatal
    ~section
    ~name:"node_da_fees"
    ~msg:
      "internal: node gives {node_da_fees} DA fees, whereas kernel gives \
       {kernel_da_fees} on block {block_number} with {call}"
    ~pp1:Z.pp_print
    ~pp2:Z.pp_print
    ~pp3:(Format.pp_print_option Ethereum_types.pp_quantity)
    ~pp4:Data_encoding.Json.pp
    ("node_da_fees", Data_encoding.z)
    ("kernel_da_fees", Data_encoding.z)
    ("block_number", Data_encoding.option Ethereum_types.quantity_encoding)
    ("call", Data_encoding.Json.encoding)

let wasm_pvm_fallback =
  Internal_event.Simple.declare_0
    ~level:Warning
    ~section
    ~name:"wasm_pvm_fallback"
    ~msg:"the node needs to fallback to the WASM PVM to execute a block"
    ()

let received_upgrade payload = emit received_upgrade payload

let pending_upgrade (upgrade : Evm_events.Upgrade.t) =
  emit pending_upgrade (upgrade.hash, upgrade.timestamp)

let applied_upgrade root_hash Ethereum_types.(Qty level) =
  emit applied_upgrade (root_hash, level)

let failed_upgrade root_hash Ethereum_types.(Qty level) =
  emit failed_upgrade (root_hash, level)

let ignored_kernel_arg () = emit ignored_kernel_arg ()

let catching_up_evm_event ~from ~to_ = emit catching_up_evm_event (from, to_)

let is_ready ~rpc_addr ~rpc_port ~websockets ~backend =
  emit event_is_ready (rpc_addr, rpc_port, backend, websockets)

let legacy_mode () = emit legacy_mode ()

let private_server_is_ready ~rpc_addr ~rpc_port ~websockets ~backend =
  emit event_private_server_is_ready (rpc_addr, rpc_port, backend, websockets)

let rpc_server_error exn =
  emit__dont_wait__use_with_care event_rpc_server_error (Printexc.to_string exn)

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

let wasm_pvm_fallback () = emit wasm_pvm_fallback ()

let missing_chain_id () = emit missing_chain_id ()

let downloading_file ?size url = emit downloading_file (url, size)

let download_in_progress ~size ~remaining_size ~elapsed_time url =
  let progress =
    match (size, remaining_size) with
    | Some size, Some remain ->
        let percent =
          float_of_int (size - remain) *. 100. /. float_of_int size
        in
        Some (remain, percent)
    | _ -> None
  in
  emit download_in_progress (url, elapsed_time, progress)

let download_failed url reason = emit download_failed (url, reason)

let importing_snapshot () = emit importing_snapshot ()

let extract_snapshot_archive_in_progress ~archive_name ~elapsed_time =
  emit extract_snapshot_archive_in_progress (archive_name, elapsed_time)

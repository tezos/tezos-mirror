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

let pending_sequencer_upgrade =
  declare_3
    ~section
    ~name:"pending_sequencer_upgrade"
    ~msg:
      "pending sequencer upgrade to sequencer {sequencer} and pool address \
       {pool_address} expected to activate at {timestamp}"
    ~level:Notice
    ("sequencer", Signature.Public_key.encoding)
    ("pool_address", Ethereum_types.address_encoding)
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

let applied_sequencer_upgrade =
  declare_2
    ~section
    ~name:"applied_sequencer_upgrade"
    ~msg:
      "sequencer successfully upgraded to {sequencer} before blueprint {level}"
    ~level:Notice
    ("sequencer", Signature.Public_key.encoding)
    ("level", Data_encoding.n)

let failed_sequencer_upgrade =
  declare_3
    ~section
    ~name:"failed_sequencer_upgrade"
    ~msg:
      "sequencer failed to upgrade to {new_sequencer} before blueprint \
       {level}. Current sequencer is {current_sequencer}"
    ~level:Warning
    ("new_sequencer", Signature.Public_key.encoding)
    ("current_sequencer", Data_encoding.option Signature.Public_key.encoding)
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

let ignored_periodic_snapshot =
  declare_0
    ~section
    ~name:"ignored_periodic_snapshot_arg"
    ~msg:
      "ignored the periodic snapshot feature since the EVM node is running in \
       Archive mode"
    ~level:Warning
    ()

let ignored_preconfirmations =
  declare_0
    ~section
    ~name:"ignored_preconfirmations"
    ~msg:
      "Inclusion preconfirmations and preconfirmed receipts will be ignored \
       since the kernel does not support it"
    ~level:Warning
    ()

let assemble_block_diverged =
  declare_1
    ~section
    ~name:"assemble_block_diverged"
    ~msg:
      "Assemble block diverged on level {level}, node is going to re-execute \
       the full blueprint"
    ~level:Warning
    ("level", Data_encoding.n)

let seq_block_hash_missing =
  declare_1
    ~section
    ~name:"seq_block_hash_missing"
    ~msg:
      "Assemble block can not validate its output for levl {level} because \
       sequencer block hash is missing"
    ~level:Warning
    ("level", Data_encoding.n)

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

let importing_legacy_snapshot =
  Internal_event.Simple.declare_0
    ~section
    ~name:"importing_legacy_snapshot"
    ~level:Warning
    ~msg:
      "Importing a legacy snapshot. Consider using a snapshot generated from a \
       more recent node."
    ()

let spawn_rpc_is_ready =
  Internal_event.Simple.declare_0
    ~section
    ~name:"spawn_rpc_is_ready"
    ~level:Notice
    ~msg:"the rpc process is ready"
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

let drift_monitor_is_ready =
  declare_1
    ~section
    ~name:"drift_monitor_is_ready"
    ~msg:"the drift monitor is ready, initial drift is {drift}"
    ~level:Notice
    ("drift", Data_encoding.z)
    ~pp1:Z.pp_print

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

let replay_csv_available =
  Internal_event.Simple.declare_1
    ~section
    ~name:"replay_csv_available"
    ~msg:"Replay data will be streamlined to {path}."
    ~level:Notice
    ("path", Data_encoding.string)

let replicate_transaction_dropped =
  Internal_event.Simple.declare_2
    ~section
    ~name:"replicate_transaction_dropped"
    ~msg:"transaction {hash} was dropped because it is now invalid ({reason})"
    ~level:Warning
    ~pp1:Ethereum_types.pp_hash
    ("hash", Ethereum_types.hash_encoding)
    ("reason", Data_encoding.string)

let replicate_operation_dropped =
  Internal_event.Simple.declare_2
    ~section
    ~name:"replicate_operation_dropped"
    ~msg:"operation {hash} was dropped because it is now invalid ({reason})"
    ~level:Warning
    ~pp1:Operation_hash.pp
    ("hash", Operation_hash.encoding)
    ("reason", Data_encoding.string)

type kernel_log_kind = Application | Simulation

type kernel_log_level = Debug | Info | Error | Fatal

let string_from_kernel_log_level level =
  let level_verbosity =
    match level with Debug -> 3 | Info -> 2 | Error -> 1 | Fatal -> 0
  in
  let value = Bytes.make 1 '\000' in
  Bytes.set_uint8 value 0 level_verbosity ;
  (* Safe because the initial pointer to the bytes is lost *)
  Bytes.unsafe_to_string value

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

let missing_block =
  Internal_event.Simple.declare_1
    ~level:Warning
    ~section
    ~name:"missing_block"
    ~msg:
      "missing block: received event saying the block for {level} would be \
       available, but reading it from storage failed"
    ("level", Data_encoding.int32)

let importing_snapshot =
  Internal_event.Simple.declare_1
    ~level:Notice
    ~section
    ~name:"importing_snapshot"
    ~msg:"importing snapshot {filename}"
    ("filename", Data_encoding.string)
    ~pp1:Format.pp_print_string

let exporting_snapshot =
  Internal_event.Simple.declare_1
    ~level:Notice
    ~section
    ~pp1:(fun fmt filename ->
      Format.pp_print_string fmt (Filename.basename filename))
    ~name:"exporting_snapshot"
    ~msg:"exporting snapshot {filename}"
    ("filename", Data_encoding.string)

let still_exporting_snapshot =
  Internal_event.Simple.declare_3
    ~level:Notice
    ~section
    ~pp1:(fun fmt filename ->
      Format.pp_print_string fmt (Filename.basename filename))
    ~pp2:(fun fmt elapsed_time ->
      Format.fprintf fmt "%a" Ptime.Span.pp elapsed_time)
    ~pp3:(fun fmt p -> Format.fprintf fmt "%.1f" p)
    ~name:"still_exporting_snapshot"
    ~msg:
      "still exporting snapshot {filename} after {elapsed_time} ({progress}% \
       done)"
    ("filename", Data_encoding.string)
    ("elapsed_time", Time.System.Span.encoding)
    ("progress", Data_encoding.float)

let finished_exporting_snapshot =
  Internal_event.Simple.declare_1
    ~level:Notice
    ~section
    ~pp1:(fun fmt filename ->
      Format.pp_print_string fmt (Filename.basename filename))
    ~name:"finished_exporting_snapshot"
    ~msg:"finished exporting snapshot {filename}"
    ("filename", Data_encoding.string)

let compressing_snapshot =
  Internal_event.Simple.declare_1
    ~level:Notice
    ~section
    ~pp1:(fun fmt filename ->
      Format.pp_print_string fmt (Filename.basename filename))
    ~name:"compressing_snapshot"
    ~msg:"compressing snapshot {filename}"
    ("filename", Data_encoding.string)

let still_compressing_snapshot =
  Internal_event.Simple.declare_3
    ~level:Notice
    ~section
    ~pp1:(fun fmt filename ->
      Format.pp_print_string fmt (Filename.basename filename))
    ~pp2:(fun fmt elapsed_time ->
      Format.fprintf fmt "%a" Ptime.Span.pp elapsed_time)
    ~pp3:(fun fmt p -> Format.fprintf fmt "%.1f" p)
    ~name:"still_compressing_snapshot"
    ~msg:
      "still compressing snapshot {filename} after {elapsed_time} ({progress}% \
       done)"
    ("filename", Data_encoding.string)
    ("elapsed_time", Time.System.Span.encoding)
    ("progress", Data_encoding.float)

let import_finished =
  Internal_event.Simple.declare_0
    ~level:Notice
    ~section
    ~name:"import_finished"
    ~msg:"snapshot import is finished"
    ()

let import_snapshot_archive_in_progress =
  Internal_event.Simple.declare_2
    ~level:Notice
    ~section
    ~name:"import_snapshot_archive_in_progress"
    ~msg:"still importing archive {name} after {elapsed_time}"
    ~pp1:(fun fmt url -> Format.fprintf fmt "%s" (Filename.basename url))
    ~pp2:(fun fmt elapsed_time ->
      Format.fprintf fmt "%a" Ptime.Span.pp elapsed_time)
    ("name", Data_encoding.string)
    ("elapsed_time", Time.System.Span.encoding)

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

let predownload_kernel_failed =
  Internal_event.Simple.declare_2
    ~level:Warning
    ~section
    ~name:"predownload_kernel_failed"
    ~msg:"failed to predownload kernel {version} due to: {error}"
    ~pp2:Error_monad.pp_print_trace
    ("version", Data_encoding.string)
    ("error", trace_encoding)

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

let rpc_call_fallback =
  Internal_event.Simple.declare_2
    ~level:Warning
    ~section
    ~name:"rpc_call_fallback"
    ~msg:"using fallback for unavailable RPC service {service} because {error}"
    ("service", Data_encoding.string)
    ("error", trace_encoding)
    ~pp1:Format.pp_print_string
    ~pp2:pp_print_trace

let multichain_node_singlechain_kernel =
  Internal_event.Simple.declare_0
    ~level:Warning
    ~section
    ~name:"multichain_node_singlechain_kernel"
    ~msg:
      "the configuration for the `l2_chains` experimental feature was ignored \
       because the multichain feature is not yet enabled in the rollup"
    ()

let event_next_block_info =
  Internal_event.Simple.declare_3
    ~section
    ~name:"next_block_info"
    ~msg:
      "{op} info for the next block: level = {level}, timestamp = {timestamp}"
    ~level:Debug
    ("op", Data_encoding.string)
    ("level", Ethereum_types.quantity_encoding)
    ("timestamp", Time.Protocol.encoding)

let event_tx_inclusion =
  Internal_event.Simple.declare_2
    ~section
    ~name:"inclusion"
    ~msg:"{op} inclusion confirmation for the transaction {txn_hash}"
    ~level:Debug
    ("op", Data_encoding.string)
    ("txn_hash", Ethereum_types.hash_encoding)

let patched_sequencer_key =
  Internal_event.Simple.declare_2
    ~section
    ~name:"patched_sequencer_key"
    ~msg:"patched state with sequencer key {pk} ({pkh})"
    ~level:Warning
    ~pp1:Signature.Public_key.pp
    ~pp2:Signature.Public_key_hash.pp
    ("pk", Signature.Public_key.encoding)
    ("pkh", Signature.Public_key_hash.encoding)

let background_task_error =
  Internal_event.Simple.declare_2
    ~section
    ~name:"background_task_error"
    ~msg:"background task {name} failed with error {error}"
    ~level:Fatal
    ("name", Data_encoding.string)
    ("error", Data_encoding.string)

let forced_native_execution_instant_confirmation =
  Internal_event.Simple.declare_0
    ~section
    ~name:"instant_confirmation_requires_native_execution"
    ~msg:
      "instant confirmation is enabled, forcing native execution policy to \
       `always`"
    ~level:Warning
    ()

let received_upgrade payload = emit received_upgrade payload

let pending_upgrade (upgrade : Evm_events.Upgrade.t) =
  emit pending_upgrade (upgrade.hash, upgrade.timestamp)

let pending_sequencer_upgrade
    (sequencer_upgrade : Evm_events.Sequencer_upgrade.t) =
  emit
    pending_sequencer_upgrade
    ( sequencer_upgrade.sequencer,
      sequencer_upgrade.pool_address,
      sequencer_upgrade.timestamp )

let applied_upgrade root_hash Ethereum_types.(Qty level) =
  emit applied_upgrade (root_hash, level)

let failed_upgrade root_hash Ethereum_types.(Qty level) =
  emit failed_upgrade (root_hash, level)

let applied_sequencer_upgrade sequencer Ethereum_types.(Qty level) =
  emit applied_sequencer_upgrade (sequencer, level)

let failed_sequencer_upgrade ~new_sequencer ~found_sequencer
    Ethereum_types.(Qty level) =
  emit failed_sequencer_upgrade (new_sequencer, found_sequencer, level)

let ignored_kernel_arg () = emit ignored_kernel_arg ()

let ignored_periodic_snapshot () = emit ignored_periodic_snapshot ()

let ignored_preconfirmations () = emit ignored_preconfirmations ()

let assemble_block_diverged level = emit assemble_block_diverged level

let seq_block_hash_missing level = emit seq_block_hash_missing level

let catching_up_evm_event ~from ~to_ = emit catching_up_evm_event (from, to_)

let is_ready ~rpc_addr ~rpc_port ~websockets ~backend =
  emit event_is_ready (rpc_addr, rpc_port, backend, websockets)

let spawn_rpc_is_ready () = emit spawn_rpc_is_ready ()

let private_server_is_ready ~rpc_addr ~rpc_port ~websockets ~backend =
  emit event_private_server_is_ready (rpc_addr, rpc_port, backend, websockets)

let drift_monitor_is_ready drift = emit drift_monitor_is_ready drift

let rpc_server_error exn =
  emit__dont_wait__use_with_care event_rpc_server_error (Printexc.to_string exn)

let background_task_error ~name exn =
  emit background_task_error (name, Printexc.to_string exn)

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

let predownload_kernel_failed root_hash error =
  emit predownload_kernel_failed (Hex.show root_hash, error)

let predownload_kernel root_hash = emit predownload_kernel (Hex.show root_hash)

let sandbox_started level = emit sandbox_started level

let cannot_fetch_time_between_blocks fallback trace =
  emit cannot_fetch_time_between_blocks (fallback, trace)

let invalid_node_da_fees ~node_da_fees ~kernel_da_fees ~block_number ~call =
  emit invalid_node_da_fees (node_da_fees, kernel_da_fees, block_number, call)

let deprecation_note msg = emit event_deprecation_note msg

let replay_csv_available msg = emit replay_csv_available msg

let wasm_pvm_fallback () = emit wasm_pvm_fallback ()

let rpc_call_fallback name error = emit rpc_call_fallback (name, error)

let missing_chain_id () = emit missing_chain_id ()

let missing_block level = emit missing_block level

let multichain_node_singlechain_kernel () =
  emit multichain_node_singlechain_kernel ()

let importing_snapshot f = emit importing_snapshot f

let importing_legacy_snapshot () = emit importing_legacy_snapshot ()

let exporting_snapshot snapshot = emit exporting_snapshot snapshot

let still_exporting_snapshot ~total ~progress snapshot elapsed_time =
  emit
    still_exporting_snapshot
    (snapshot, elapsed_time, 100. *. float_of_int progress /. float_of_int total)

let finished_exporting_snapshot snapshot =
  emit finished_exporting_snapshot snapshot

let compressing_snapshot snapshot = emit compressing_snapshot snapshot

let still_compressing_snapshot ~total ~progress snapshot elapsed_time =
  emit
    still_compressing_snapshot
    (snapshot, elapsed_time, 100. *. float_of_int progress /. float_of_int total)

let import_finished () = emit import_finished ()

let import_snapshot_archive_in_progress ~archive_name ~elapsed_time =
  emit import_snapshot_archive_in_progress (archive_name, elapsed_time)

let replicate_transaction_dropped hash reason =
  emit replicate_transaction_dropped (hash, reason)

let replicate_operation_dropped hash reason =
  emit replicate_operation_dropped (hash, reason)

let next_block_info timestamp level =
  emit event_next_block_info ("received", level, timestamp)

let inclusion hash = emit event_tx_inclusion ("received", hash)

let sent_next_block_info timestamp level =
  emit event_next_block_info ("sent", level, timestamp)

let sent_inclusion hash = emit event_tx_inclusion ("sent", hash)

let patched_sequencer_key pk =
  emit patched_sequencer_key (pk, Signature.Public_key.hash pk)

let forced_native_execution_instant_confirmation () =
  emit forced_native_execution_instant_confirmation ()

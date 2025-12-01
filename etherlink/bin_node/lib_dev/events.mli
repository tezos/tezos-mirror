(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** General purposes events. *)

(** Default section for events. *)
val section : string list

(** Error encoding specific to the EVM node that wraps the default error encoding for event.

    Use it to prevent the command `list events` to show all existing octez error.
 *)
val trace_encoding : tztrace Data_encoding.t

(** [received_upgrade payload] advertises that the sequencer received an
    upgrade of payload [payload]. *)
val received_upgrade : string -> unit Lwt.t

(** [pending_upgrade upgrade] advertises that the EVM node is aware that an
    upgrade is pending. *)
val pending_upgrade : Evm_events.Upgrade.t -> unit Lwt.t

(** [pending_sequencer_upgrade upgrade] advertises that the EVM node
    is aware that a sequencer upgrade is pending. *)
val pending_sequencer_upgrade : Evm_events.Sequencer_upgrade.t -> unit Lwt.t

(** [applied_sequencer_upgrade sequencer level] advertises that the
    sequencer of the EVM node successfully upgraded to [sequencer]
    before the [level]th blueprint. *)
val applied_sequencer_upgrade :
  Signature.Public_key.t -> Ethereum_types.quantity -> unit Lwt.t

(** [applied_sequencer_upgrade sequencer level] advertises that the
    sequencer of the EVM node failed to upgrade to [sequencer] before
    the [level]th blueprint. *)
val failed_sequencer_upgrade :
  new_sequencer:Signature.Public_key.t ->
  found_sequencer:Signature.Public_key.t option ->
  Ethereum_types.quantity ->
  unit Lwt.t

(** [applied_upgrade root_hash level] advertises that the kernel of the EVM
    node successfully upgraded to [root_hash] with the [level]th blueprint. *)
val applied_upgrade :
  Ethereum_types.hash -> Ethereum_types.quantity -> unit Lwt.t

(** [failed_upgrade root_hash level] advertises that the kernel of the EVM
    node failed to upgrade to [root_hash] with the [level]th blueprint. *)
val failed_upgrade :
  Ethereum_types.hash -> Ethereum_types.quantity -> unit Lwt.t

(** [ignored_kernel_arg ()] advertises that the EVM node has ignored
    the path to the initial kernel given as a command-line argument
    since its EVM state was already initialized. *)
val ignored_kernel_arg : unit -> unit Lwt.t

(** [ignored_periodic_snapshot_arg ()] advertises that the EVM node has
    ignored the request to create periodic snapshots when garbage collecting
    since it was started in Archive mode. *)
val ignored_periodic_snapshot : unit -> unit Lwt.t

(** [ignored_periodic_snapshot_arg ()] advertises that the EVM node will
    be ignoring the all the incoming preconfirmation data. *)
val ignored_preconfirmations : unit -> unit Lwt.t

(** [assemble_block_diverged level] advertises that the assembled block has
    diverged from the expected one and that the node will re-execute
    the full blueprint to recover consistency. *)
val assemble_block_diverged : Z.t -> unit Lwt.t

(** [seq_block_hash_missing level] advertises that the assembled block cannot
    be validated because the sequencer block hash is missing. *)
val seq_block_hash_missing : Z.t -> unit Lwt.t

(** [catching_up_evm_event ~from ~to_] advertises that the sequencer
    is catching up on event produced by the evm kernel in the rollup
    node from L1 level [from] to [to_]. *)
val catching_up_evm_event : from:int32 -> to_:int32 -> unit Lwt.t

(** [is_ready ~rpc_addr ~rpc_port ~websockets ~backend] advertises that the
    sequencer is ready and listens to [rpc_addr]:[rpc_port]. *)
val is_ready :
  rpc_addr:string ->
  rpc_port:int ->
  websockets:bool ->
  backend:Configuration.rpc_server ->
  unit Lwt.t

(** [spawn_rpc_is_ready ()] advertises that the RPC spawned with experimental
    feature [spawn_rpc] has made its public endpoint available. *)
val spawn_rpc_is_ready : unit -> unit Lwt.t

(** [private_server_is_ready ~rpc_addr ~rpc_port ~websockets ~backend]
    advertises that the private rpc server is ready and listens to
    [rpc_addr]:[rpc_port]. *)
val private_server_is_ready :
  rpc_addr:string ->
  rpc_port:int ->
  websockets:bool ->
  backend:Configuration.rpc_server ->
  unit Lwt.t

val rpc_server_error : exn -> unit

val background_task_error : name:string -> exn -> unit Lwt.t

(** [shutdown_rpc_server ~private_ ()] advertises that the RPC server
    was shut down, [private_] tells whether it is the private server
    or not. *)
val shutdown_rpc_server : private_:bool -> unit Lwt.t

(** [shutdown_node ~exit_status] advertises that the sequencer was
    shutdown, and exits with [exit_status]. *)
val shutdown_node : exit_status:int -> unit Lwt.t

(** [callback_log ~uri ~meth ~body] is used as the debug event used as
    callback for resto to logs the requests. *)
val callback_log : uri:string -> meth:string -> body:string -> unit Lwt.t

type kernel_log_kind = Application | Simulation

type kernel_log_level = Debug | Info | Error | Fatal

val string_from_kernel_log_level : kernel_log_level -> string

(** Logs kernel log [Debug]. *)
val event_kernel_log :
  level:kernel_log_level -> kind:kernel_log_kind -> msg:string -> unit Lwt.t

val retrying_connect : endpoint:Uri.t -> delay:float -> unit Lwt.t

(** [preload_kernel version] advertizes the EVM node has preloaded in the
    module cache the kernel [version]. *)
val preload_kernel : string -> unit Lwt.t

val patched_state : string -> Ethereum_types.quantity -> unit Lwt.t

(** [predownload_kernel root_hash] advertizes the EVM node has
    downloaded all preimages under [root_hash]. *)
val predownload_kernel : Hex.t -> unit Lwt.t

(** [predownload_kernel_failed root_hash error] advertizes the EVM node has
    failed to download preimages under [root_hash] with [error]. *)
val predownload_kernel_failed : Hex.t -> tztrace -> unit Lwt.t

(** [sandbox_started level] advertizes that sandbox mode started on top of
    level [level]. *)
val sandbox_started : Z.t -> unit Lwt.t

val cannot_fetch_time_between_blocks :
  Configuration.time_between_blocks -> tztrace -> unit Lwt.t

val invalid_node_da_fees :
  node_da_fees:Z.t ->
  kernel_da_fees:Z.t ->
  block_number:Ethereum_types.quantity option ->
  call:Data_encoding.json ->
  unit Lwt.t

val deprecation_note : string -> unit Lwt.t

(** [replay_csv_available filename] advertises that [filename]
    is now available for analysis or debugging purposes. *)
val replay_csv_available : string -> unit Lwt.t

(** [wasm_pvm_fallback ()] advertises that the node has to fallback to the PVM
    to execute a block, which is slow. *)
val wasm_pvm_fallback : unit -> unit Lwt.t

(** [rpc_call_fallback service_name error] advertises that the node has to
    fallback to an alternative RPC because one is unavailable. *)
val rpc_call_fallback : string -> tztrace -> unit Lwt.t

(** [missing_chain_id ()] advertises that the node could not check the
    consistency of the stored chain id with the selected network. *)
val missing_chain_id : unit -> unit Lwt.t

(** [missing_block level] advertises that the node could not find the block in level despite having received an event saying it would be available. *)
val missing_block : int32 -> unit Lwt.t

(** [multichain_node_singlechain_kernel ()] warns that the node
    was configured to be executed in a multichain environment, but was given
    a kernel for a single chain environment. *)
val multichain_node_singlechain_kernel : unit -> unit Lwt.t

val importing_snapshot : string -> unit Lwt.t

(** Emit a warning that the imported snapshot uses the legacy block storage. *)
val importing_legacy_snapshot : unit -> unit Lwt.t

val exporting_snapshot : string -> unit Lwt.t

val still_exporting_snapshot :
  total:int -> progress:int -> string -> Ptime.Span.t -> unit Lwt.t

val finished_exporting_snapshot : string -> unit Lwt.t

val compressing_snapshot : string -> unit Lwt.t

val still_compressing_snapshot :
  total:int -> progress:int -> string -> Ptime.Span.t -> unit Lwt.t

val import_finished : unit -> unit Lwt.t

(** [import_snapshot_archive_in_progress ~archive_name ~elapsed_time] advertises
    that the node is importing the snapshot archive named [archive_name], and
    explicitly mentions the time elapsed since the extraction started. *)
val import_snapshot_archive_in_progress :
  archive_name:string -> elapsed_time:Time.System.Span.t -> unit Lwt.t

(** [replicate_transaction_dropped hash reason] advertises that the transaction
    [hash] was dropped because it is now invalid in the sandbox. *)
val replicate_transaction_dropped : Ethereum_types.hash -> string -> unit Lwt.t

(** [replicate_operation_dropped hash reason] advertises that the operation
    [hash] was dropped because it is now invalid in the sandbox. *)
val replicate_operation_dropped : Operation_hash.t -> string -> unit Lwt.t

val next_block_info : Time.Protocol.t -> Ethereum_types.quantity -> unit Lwt.t

val inclusion : Ethereum_types.hash -> unit Lwt.t

val sent_next_block_info :
  Time.Protocol.t -> Ethereum_types.quantity -> unit Lwt.t

val sent_inclusion : Ethereum_types.hash -> unit Lwt.t

(** [patched_sequencer_key pk] advertises that the sequencer key in
    state was patched with [pk] . *)
val patched_sequencer_key : Signature.Public_key.t -> unit Lwt.t

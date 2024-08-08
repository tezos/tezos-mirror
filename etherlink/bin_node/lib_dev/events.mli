(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** General purposes events. *)

(** Default section for events. *)
val section : string list

(** [received_upgrade payload] advertises that the sequencer received an
    upgrade of payload [payload]. *)
val received_upgrade : string -> unit Lwt.t

(** [pending_upgrade upgrade] advertises that the EVM node is aware that an
    upgrade is pending. *)
val pending_upgrade : Evm_events.Upgrade.t -> unit Lwt.t

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

(** [catching_up_evm_event ~from ~to_] advertises that the sequencer
    is catching up on event produced by the evm kernel in the rollup
    node from L1 level [from] to [to_]. *)
val catching_up_evm_event : from:int32 -> to_:int32 -> unit Lwt.t

(** [is_ready ~rpc_addr ~rpc_port] advertises that the sequencer is
    ready and listens to [rpc_addr]:[rpc_port]. *)
val is_ready : rpc_addr:string -> rpc_port:int -> unit Lwt.t

(** [private_server_is_ready ~rpc_addr ~rpc_port] advertises that the
    private rpc server is ready and listens to [rpc_addr]:[rpc_port]. *)
val private_server_is_ready : rpc_addr:string -> rpc_port:int -> unit Lwt.t

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

(** [sandbox_started level] advertizes that sandbox mode started on top of
    level [level]. *)
val sandbox_started : Z.t -> unit Lwt.t

val cannot_fetch_time_between_blocks :
  Configuration.time_between_blocks -> tztrace -> unit Lwt.t

val invalid_node_da_fees : node_da_fees:Z.t -> kernel_da_fees:Z.t -> unit Lwt.t

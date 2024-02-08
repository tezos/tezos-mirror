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

(** [ignored_kernel_arg ()] advertises that the EVM node has ignored
    the path to the initial kernel given as a command-line argument
    since its EVM state was already initialized. *)
val ignored_kernel_arg : unit -> unit Lwt.t

(** [is_ready ~rpc_addr ~rpc_port] advertises that the sequencer is
    ready and listens to [rpc_addr]:[rpc_port]. *)
val is_ready : rpc_addr:string -> rpc_port:int -> unit Lwt.t

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

(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_base
open Tezos_base.TzPervasives
open Tezos_base_unix

(** Information required to start and manage an RPC server process.
    @param config Configuration for the RPC server
    @param internal_events Configuration for internal event handling
    @param rpc_comm_socket_path Path to the socket used for RPC communication
    @param node_version Version of the Octez node
*)
type process_info = {
  config : Config_file.t;
  internal_events : Internal_event_config.t;
  rpc_comm_socket_path : string;
  node_version : Tezos_version.Octez_node_version.t;
}

(** Parameters for the watchdog process.
    @param internal_events Configuration for internal event handling
*)
type parameters = {internal_events : Internal_event_config.t}

(** Request type for watchdog operations:
    - Start_server: Starts a new RPC server with the provided configuration
    - Close_server: Closes a running server identified by the socket path
    - Terminate: Gracefully shuts down the watchdog process
    - Reconfigure_event_logging: Updates the event logging configuration
*)
type _ request =
  | Start_server : process_info -> unit request
  | Close_server : string -> unit request
  | Terminate : unit request
  | Reconfigure_event_logging :
      Internal_event_unix.Configuration.t
      -> unit request

val name : string

val request_pp : Format.formatter -> 'response request -> unit

val internal_events : parameters -> Internal_event_config.t

val magic : Bytes.t

val parameters_encoding : parameters Data_encoding.t

type packed_request = Erequest : _ request -> packed_request

val request_encoding : packed_request Data_encoding.t

val result_encoding : 'response request -> 'response Data_encoding.t

val reconfigure_event_logging_request : Internal_event_config.t -> unit request

val terminate_request : packed_request

val socket_path_prefix : string

val socket_path : socket_dir:string -> pid:int -> string

val command_line_args : socket_dir:string -> string * string list

val hypervisor_name : string

(**
 * When true, indicates that the sink should be shared between
 * the parent process and child processes. This affects how
 * environment variables are handled when starting external processes.
 *)
val share_sink : bool

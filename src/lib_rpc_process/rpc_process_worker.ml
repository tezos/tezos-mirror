(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023-2024 Nomadic Labs <contact@nomadic-labs.com>           *)
(*                                                                           *)
(*****************************************************************************)

module Name : Lwt_process_watchdog.NAME = struct
  let base = ["node"; "main"]

  let component = ["rpc"; "process"]
end

module Event : Lwt_process_watchdog.EVENTS =
  Lwt_process_watchdog.MakeEvent (Name)

type process = Parameters.t Lwt_process_watchdog.t

let create ~comm_socket_path (config : Config_file.t) node_version events_config
    =
  let parameters =
    Parameters.
      {
        internal_events = events_config;
        config;
        rpc_comm_socket_path = comm_socket_path;
        node_version;
      }
  in
  Lwt_process_watchdog.create
    ~parameters
    ~parameters_encoding:Parameters.parameters_encoding

let rpc_process_socket_magic = Bytes.of_string "TEZOS_RPC_SERVER_MAGIC_0"

let rpc_process_socket_prefix = "init-rpc-socket"

module Watchdog = Lwt_process_watchdog.Daemon (Event)

let stop (t : process) = Watchdog.stop t

let start config events_config comm_socket_path node_version =
  let parameters =
    Parameters.
      {
        internal_events = events_config;
        config;
        rpc_comm_socket_path = comm_socket_path;
        node_version;
      }
  in
  let process =
    Lwt_process_watchdog.create
      ~parameters
      ~parameters_encoding:Parameters.parameters_encoding
  in

  let open Lwt_result_syntax in
  let run_process =
    Watchdog.run_process_with_sockets
      process
      ~socket_prefix:rpc_process_socket_prefix
      ~process_name:"octez-rpc-process"
      ~handshake:rpc_process_socket_magic
  in
  let* new_server = run_process () in
  let _ = Watchdog.watch_dog ~start_new_server:run_process new_server in
  return process

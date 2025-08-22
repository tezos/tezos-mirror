(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_base
open Tezos_base.TzPervasives
open Tezos_base_unix

type process_info = {
  config : Config_file.t;
  internal_events : Internal_event_config.t;
  rpc_comm_socket_path : string;
  node_version : Tezos_version.Octez_node_version.t;
}

type parameters = {internal_events : Internal_event_config.t}

type _ request =
  | Start_server : process_info -> unit request
  | Close_server : string -> unit request
  | Terminate : unit request
  | Reconfigure_event_logging :
      Internal_event_unix.Configuration.t
      -> unit request

let name = "rpc-process-watchdog"

let request_pp : type a. Format.formatter -> a request -> unit =
 fun ppf -> function
  | Start_server {config; rpc_comm_socket_path; _} ->
      Format.fprintf
        ppf
        "Start_server {config = %s; rpc_comm_socket_path = %s}"
        (Config_file.to_string config)
        rpc_comm_socket_path
  | Close_server s -> Format.fprintf ppf "Close_server %s" s
  | Terminate -> Format.fprintf ppf "Terminate"
  | Reconfigure_event_logging _ ->
      Format.fprintf ppf "Reconfigure_event_logging"

let magic = Bytes.of_string "TEZOS_RPC_PROCESS_WATCHDOG_MAGIC_0"

let parameters_encoding =
  let open Data_encoding in
  conv
    (fun {internal_events} -> internal_events)
    (fun internal_events -> {internal_events})
    Internal_event_config.encoding

type packed_request = Erequest : _ request -> packed_request

let request_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"start_server"
        (obj4
           (req "config" Config_file.encoding)
           (req "internal_events" Internal_event_config.encoding)
           (req "rpc_comm_socket_path" string)
           (req "node_version" Tezos_version.Octez_node_version.encoding))
        (function
          | Erequest
              (Start_server
                 {config; internal_events; rpc_comm_socket_path; node_version})
            ->
              Some (config, internal_events, rpc_comm_socket_path, node_version)
          | _ -> None)
        (fun (config, internal_events, rpc_comm_socket_path, node_version) ->
          Erequest
            (Start_server
               {config; internal_events; rpc_comm_socket_path; node_version}));
      case
        (Tag 1)
        ~title:"close_server"
        string
        (function Erequest (Close_server s) -> Some s | _ -> None)
        (fun s -> Erequest (Close_server s));
      case
        (Tag 2)
        ~title:"terminate"
        unit
        (function Erequest Terminate -> Some () | _ -> None)
        (fun () -> Erequest Terminate);
      case
        (Tag 3)
        ~title:"reconfigure_event_logging"
        Internal_event_unix.Configuration.encoding
        (function
          | Erequest (Reconfigure_event_logging x) -> Some x | _ -> None)
        (fun x -> Erequest (Reconfigure_event_logging x));
    ]

let result_encoding : type a. a request -> a Data_encoding.t = function
  | Start_server _ -> Data_encoding.unit
  | Close_server _ -> Data_encoding.unit
  | Terminate -> Data_encoding.unit
  | Reconfigure_event_logging _ -> Data_encoding.unit

let socket_path_prefix = "tezos-rpc-process-watchdog-socket-"

let internal_events {internal_events; _} = internal_events

let socket_path ~socket_dir ~pid =
  Filename.concat socket_dir (socket_path_prefix ^ string_of_int pid)

let reconfigure_event_logging_request config = Reconfigure_event_logging config

let terminate_request = Erequest Terminate

let command_line_args ~socket_dir =
  ("octez-rpc-process-watchdog", ["--socket-dir"; socket_dir])

let hypervisor_name = "octez-rpc-process-watchdog-hypervisor"

let share_sink = true

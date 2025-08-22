(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* External watchdog process.

   This process is responsible for starting and stopping the RPC process. It
   also launches a watchdog lwt thread for each RPC server process launched
   ensuring they are automatically restarted if needed. *)

type process_info = {
  config : Config_file.t;
  internal_events : Tezos_base.Internal_event_config.t;
  rpc_comm_socket_path : string;
  node_version : Tezos_version.Octez_node_version.t;
}

type error += No_server_with_comm_socket_path of string

let () =
  Error_monad.register_error_kind
    `Temporary
    ~id:"no_server_with_comm_socket_path"
    ~title:"No server with comm_socket_path"
    ~description:"No server with comm_socket_path"
    ~pp:(fun ppf s -> Format.fprintf ppf "No server with comm_socket_path %s" s)
    Data_encoding.(obj1 (req "comm_socket_path" string))
    (function No_server_with_comm_socket_path s -> Some s | _ -> None)
    (fun s -> No_server_with_comm_socket_path s)

module Events = struct
  include Internal_event.Simple

  let section = ["external_watchdog"]

  let init =
    declare_0
      ~level:Info
      ~section
      ~name:"init"
      ~msg:"initializing external watchdog"
      ()

  let start_server =
    declare_1
      ~level:Info
      ~section
      ~name:"start_server"
      ~msg:"starting server with comm_socket_path {comm_socket_path}"
      ("comm_socket_path", Data_encoding.string)

  let close_server =
    declare_1
      ~level:Info
      ~section
      ~name:"close_server"
      ~msg:"closing server with comm_socket_path {comm_socket_path}"
      ("comm_socket_path", Data_encoding.string)

  let error =
    declare_1
      ~level:Error
      ~section
      ~name:"error"
      ~msg:"error: {error}"
      ("error", Data_encoding.string)

  let close =
    declare_0
      ~level:Info
      ~section
      ~name:"close"
      ~msg:"closing rpc server watchdog process "
      ()
end

module Processing :
  External_process_main.EXTERNAL_PROCESSING
    with type parameters = Watchdog_parameters.parameters
     and type 'a request = 'a Watchdog_parameters.request = struct
  (* RPC_process should always be restarted with the same socket path, we thus
     use the socket path as the key in the state map. *)
  type state = Rpc_process_worker.process String.Map.t

  type parameters = Watchdog_parameters.parameters

  type 'a request = 'a Watchdog_parameters.request

  let initial_state _parameters =
    let open Lwt_result_syntax in
    return String.Map.empty

  let handle_request : type a.
      parameters ->
      state ->
      a request ->
      [ `Continue of
        (a
        * (Tezos_profiler.Profiler.report option
          * Tezos_profiler.Profiler.report option)
          option)
        tzresult
        * state
      | `Stop ]
      Lwt.t =
    let open Lwt_result_syntax in
    fun {internal_events = _} state ->
      let continue res state report :
          [> `Continue of
             (a
             * (Tezos_profiler.Profiler.report option
               * Tezos_profiler.Profiler.report option)
               option)
             tzresult
             * state ]
          Lwt.t =
        let res =
          match res with Error errs -> Error errs | Ok res -> Ok (res, report)
        in
        Lwt.return (`Continue (res, state))
      in
      function
      | Start_server
          {config; internal_events; rpc_comm_socket_path; node_version} -> (
          let*! () = Events.(emit start_server rpc_comm_socket_path) in
          match String.Map.find_opt rpc_comm_socket_path state with
          | Some _ ->
              (* The server is already running, we don't need to start it again *)
              continue (Ok ()) state None
          | None -> (
              (* The server is not running, we need to start it *)
              let*! res =
                (* launch the RPC server process and associate it with a watchdog
                   thread *)
                Rpc_process_worker.start
                  config
                  internal_events
                  rpc_comm_socket_path
                  node_version
              in
              match res with
              | Error errs -> continue (Error errs) state None
              | Ok res ->
                  let state : state =
                    String.Map.add rpc_comm_socket_path res state
                  in
                  continue (Ok ()) state None))
      | Close_server comm_socket_path -> (
          let*! () = Events.(emit close_server comm_socket_path) in
          match String.Map.find_opt comm_socket_path state with
          | None ->
              continue
                (Error [No_server_with_comm_socket_path comm_socket_path])
                state
                None
          | Some t ->
              let*! res = Rpc_process_worker.stop t in
              let state = String.Map.remove comm_socket_path state in
              continue (Ok res) state None)
      | Terminate ->
          let*! () = Events.(emit close ()) in
          Lwt.return `Stop
      | Reconfigure_event_logging config ->
          let*! res =
            Tezos_base_unix.Internal_event_unix.Configuration.reapply config
          in
          continue res state None
end

module Process = struct
  include Tezos_base_unix.External_process.Make (Watchdog_parameters)

  let init config ~process_path =
    let open Lwt_syntax in
    let* () = Events.(emit init ()) in
    init ~process_path config

  let start_server t
      {config; internal_events; rpc_comm_socket_path; node_version} =
    send_request
      t
      (Start_server
         {config; internal_events; rpc_comm_socket_path; node_version})

  let stop_server t comm_socket_path =
    let open Lwt_result_syntax in
    let* (), _report = send_request t (Close_server comm_socket_path) in
    return_unit

  let terminate t = send_request t Terminate

  let reconfigure_event_logging_request t config =
    let open Lwt_result_syntax in
    let* (), _report = send_request t (Reconfigure_event_logging config) in
    return_unit
end

include
  Tezos_base_unix.External_process_main.Make (Watchdog_parameters) (Processing)
module Hypervisor =
  Tezos_base_unix.Hypervisor_process_main.Make (Watchdog_parameters)

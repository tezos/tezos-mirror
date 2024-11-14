(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

type error +=
  | RPC_Process_Port_already_in_use of P2p_point.Id.t list
  | Missing_socket_dir

let () =
  register_error_kind
    `Permanent
    ~id:"rpc_process.main.process_port_already_in_use"
    ~title:"Cannot start RPC process: RPC port already in use"
    ~description:
      "Another octez RPC process is probably running on the same RPC port."
    ~pp:(fun ppf addrlist ->
      Format.fprintf
        ppf
        "Another octez RPC process is probably running on one of these \
         addresses (%a). Please choose another RPC port."
        (Format.pp_print_list P2p_point.Id.pp)
        addrlist)
    Data_encoding.(obj1 (req "addrlist" (list P2p_point.Id.encoding)))
    (function
      | RPC_Process_Port_already_in_use addrlist -> Some addrlist | _ -> None)
    (fun addrlist -> RPC_Process_Port_already_in_use addrlist) ;
  register_error_kind
    `Permanent
    ~id:"rpc_process.main.missing_socket_dir"
    ~title:"Cannot start RPC process: missing socket dir"
    ~description:"Cannot start RPC process without socket dir."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "The path to the communication socket is missing. Use the --socket-dir \
         option to specify it when running the RPC process. ")
    Data_encoding.empty
    (function Missing_socket_dir -> Some () | _ -> None)
    (fun () -> Missing_socket_dir)

let rpc_metrics =
  Prometheus.Summary.v_labels
    ~label_names:["endpoint"; "method"]
    ~help:"External RPC endpoint call counts and sum of execution times."
    ~namespace:Tezos_version.Octez_node_version.namespace
    ~subsystem:"external_rpc_process"
    "calls"

module Metrics_server = Prometheus_app.Cohttp (Cohttp_lwt_unix.Server)

(* Add default accepted CORS headers *)
let sanitize_cors_headers ~default headers =
  List.map String.lowercase_ascii headers
  |> String.Set.of_list
  |> String.Set.(union (of_list default))
  |> String.Set.elements

let launch_rpc_server dynamic_store (params : Parameters.t) (addr, port)
    head_watcher applied_blocks_watcher =
  let open Lwt_result_syntax in
  let media_types = params.config.rpc.media_type in
  let*! acl_policy =
    RPC_server.Acl.resolve_domain_names params.config.rpc.acl
  in
  let host = Ipaddr.V6.to_string addr in
  let mode =
    match params.config.rpc.tls with
    | None -> `TCP (`Port port)
    | Some {cert; key} ->
        `TLS (`Crt_file_path cert, `Key_file_path key, `No_password, `Port port)
  in
  let acl =
    let open RPC_server.Acl in
    find_policy acl_policy (Ipaddr.V6.to_string addr, Some port)
    |> Option.value_f ~default:(fun () -> default addr)
  in
  let*! () =
    Rpc_process_events.(emit starting_rpc_server)
      (host, port, params.config.rpc.tls <> None, RPC_server.Acl.policy_type acl)
  in
  let cors_headers =
    sanitize_cors_headers
      ~default:["Content-Type"]
      params.config.rpc.cors_headers
  in
  let cors =
    Resto_cohttp.Cors.
      {
        allowed_origins = params.config.rpc.cors_origins;
        allowed_headers = cors_headers;
      }
  in
  let dir =
    Directory.build_rpc_directory
      params.node_version
      params.config
      dynamic_store
      ~head_watcher
      ~applied_blocks_watcher
  in
  let server =
    RPC_server.init_server
      ~cors
      ~acl
      ~media_types:(Media_type.Command_line.of_command_line media_types)
      dir
  in
  let forwarder_resources = RPC_middleware.init_forwarder () in
  let callback (conn : Cohttp_lwt_unix.Server.conn) req body =
    let path = Cohttp.Request.uri req |> Uri.path in
    if path = "/metrics" then
      let*! response = Metrics_server.callback conn req body in
      Lwt.return (`Response response)
    else
      Forward_handler.callback
        ~acl
        server
        forwarder_resources
        params.rpc_comm_socket_path
        conn
        req
        body
  in
  let conn_closed = RPC_middleware.forwarding_conn_closed forwarder_resources in
  let update_metrics uri meth callback =
    Prometheus.Summary.(time (labels rpc_metrics [uri; meth]) Sys.time) callback
  in
  let callback =
    RPC_middleware.rpc_metrics_transform_callback ~update_metrics dir callback
  in
  let* callback =
    if params.config.rpc.enable_http_cache_headers then
      let Http_cache_headers.{get_estimated_time_to_next_level; get_block_hash}
          =
        Http_cache_headers.make_tools (fun () ->
            let store_opt = !dynamic_store in
            Option.map Store.main_chain_store store_opt)
      in
      let*! () =
        Rpc_process_events.(emit enable_http_cache_headers_for_external ())
      in
      return
        (RPC_middleware.Http_cache_headers.make
           ~get_estimated_time_to_next_level
           ~get_block_hash
           callback)
    else return callback
  in
  Lwt.catch
    (fun () ->
      let*! () =
        RPC_server.launch
          ~host
          server
          ~conn_closed
          ~callback
          ~max_active_connections:params.config.rpc.max_active_rpc_connections
          mode
      in
      return server)
    (function
      (* FIXME: https://gitlab.com/tezos/tezos/-/issues/1312
         This exception seems to be unreachable.
      *)
      | Unix.Unix_error (Unix.EADDRINUSE, "bind", "") ->
          tzfail (RPC_Process_Port_already_in_use [(addr, port)])
      | exn -> fail_with_exn exn)

let init_rpc dynamic_store parameters head_watcher applied_blocks_watcher =
  let open Lwt_result_syntax in
  let* server =
    let* p2p_point =
      match
        parameters.Parameters.config.Config_file.rpc.external_listen_addrs
      with
      | [addr] -> Config_file.resolve_rpc_listening_addrs addr
      | _ ->
          (* We assume that the config contains only one listening
             address. This should hold thanks to the `init_rpc`
             function from `bin_node/Node_run_command`. *)
          assert false
    in
    match p2p_point with
    | [point] ->
        launch_rpc_server
          dynamic_store
          parameters
          point
          head_watcher
          applied_blocks_watcher
    | _ ->
        (* Same as above: only one p2p_point is expected here. *)
        assert false
  in
  (* Add a cleanup call back to shutdown the RPC_server gracefully
     when an exit signal is received. *)
  let (_ccid : Lwt_exit.clean_up_callback_id) =
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
        RPC_server.shutdown server)
  in
  return_unit

let get_init_socket_path socket_dir pid =
  let filename = Format.sprintf "init-rpc-socket-%d" pid in
  Filename.concat socket_dir filename

(* Magic bytes used for the external RPC process handshake. *)
let socket_magic = Bytes.of_string "TEZOS_RPC_SERVER_MAGIC_0"

let create_init_socket socket_dir =
  let open Lwt_result_syntax in
  let* socket_dir =
    match socket_dir with
    | Some sd -> return sd
    | None -> tzfail Missing_socket_dir
  in
  let pid = Unix.getpid () in
  let init_socket_path = get_init_socket_path socket_dir pid in
  let* init_socket_fd = Socket.connect (Unix init_socket_path) in
  (* Unlink the socket as soon as both sides have opened it.*)
  let*! () = Lwt_unix.unlink init_socket_path in
  let* () = Socket.handshake init_socket_fd socket_magic in
  return init_socket_fd

let run socket_dir =
  let open Lwt_result_syntax in
  let* init_socket_fd = create_init_socket socket_dir in
  let* parameters = Socket.recv init_socket_fd Parameters.parameters_encoding in
  let*! () =
    Tezos_base_unix.Internal_event_unix.init
      ~config:parameters.Parameters.internal_events
      ()
  in
  (* Updater needs to be initialized to be able to read the protocol
     sources from the store when a protocol is injected and
     compiled. *)
  Updater.init (Data_version.protocol_dir parameters.config.data_dir) ;
  let head_watcher = Lwt_watcher.create_input () in
  let dynamic_store : Store.t option ref = ref None in
  let applied_blocks_watcher = ref Directory.Empty in
  let* daemon =
    Head_daemon.init
      dynamic_store
      parameters
      head_watcher
      applied_blocks_watcher
  in
  let (_ccid : Lwt_exit.clean_up_callback_id) =
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
        Head_daemon.Daemon.shutdown daemon)
  in
  let* () =
    init_rpc dynamic_store parameters head_watcher applied_blocks_watcher
  in
  (* Send the config ack as synchronisation barrier for the init_rpc
     phase. *)
  let* () = Socket.send init_socket_fd Data_encoding.unit () in
  Lwt_utils.never_ending ()

let process socket_dir =
  let open Lwt_result_syntax in
  let main_promise =
    Lwt.catch (fun () -> run socket_dir) (function exn -> fail_with_exn exn)
  in
  Lwt_main.run
    (let*! r = Lwt_exit.wrap_and_exit main_promise in
     match r with
     | Ok () ->
         let*! _ = Lwt_exit.exit_and_wait 0 in
         Lwt.return (`Ok ())
     | Error err ->
         let*! _ = Lwt_exit.exit_and_wait 1 in
         Lwt.return @@ `Error (false, Format.asprintf "%a" pp_print_trace err))

module Term = struct
  let socket_dir =
    let open Cmdliner in
    let doc = "Socket directory to communicate with node process" in
    Arg.(
      value
      & opt (some string) None
      & info ~docs:Shared_arg.Manpage.misc_section ~doc ["socket-dir"])

  let term = Cmdliner.Term.(ret (const process $ socket_dir))
end

module Manpage = struct
  let command_description =
    "The $(b, octez-rpc-process) starts the RPC process that aims to serve as \
     the default endpoint for RPC queries. This server may communicate with an \
     Octez node."

  let description = [`S "DESCRIPTION"; `P command_description]

  let man = description

  let info =
    Cmdliner.Cmd.info ~doc:"Run the Octez rpc process" ~man "octez-rpc-process"
end

let cmd = Cmdliner.Cmd.v Manpage.info Term.term

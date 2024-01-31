(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

type error += Non_private_sandbox of P2p_addr.t

type error += RPC_Port_already_in_use of P2p_point.Id.t list

type error += Invalid_sandbox_file of string

let () =
  register_error_kind
    `Permanent
    ~id:"main.run.non_private_sandbox"
    ~title:"Forbidden public sandbox"
    ~description:"A sandboxed node should not listen on a public address."
    ~pp:(fun ppf addr ->
      Format.fprintf
        ppf
        "The node is configured to listen on a public address (%a), while only \
         'private' networks are authorised with `--sandbox`.\n\
        \           See `%s run --help` on how to change the listening address."
        Ipaddr.V6.pp
        addr
        Sys.argv.(0))
    Data_encoding.(obj1 (req "addr" P2p_addr.encoding))
    (function Non_private_sandbox addr -> Some addr | _ -> None)
    (fun addr -> Non_private_sandbox addr) ;
  register_error_kind
    `Permanent
    ~id:"main.run.port_already_in_use"
    ~title:"Cannot start node: RPC port already in use"
    ~description:"Another tezos node is probably running on the same RPC port."
    ~pp:(fun ppf addrlist ->
      Format.fprintf
        ppf
        "Another tezos node is probably running on one of these addresses \
         (%a). Please choose another RPC port."
        (Format.pp_print_list P2p_point.Id.pp)
        addrlist)
    Data_encoding.(obj1 (req "addrlist" (list P2p_point.Id.encoding)))
    (function RPC_Port_already_in_use addrlist -> Some addrlist | _ -> None)
    (fun addrlist -> RPC_Port_already_in_use addrlist) ;
  register_error_kind
    `Permanent
    ~id:"main.run.invalid_sandbox_file"
    ~title:"Invalid sandbox file"
    ~description:"The provided sandbox file is not a valid sandbox JSON file."
    ~pp:(fun ppf s ->
      Format.fprintf ppf "The file '%s' is not a valid JSON sandbox file" s)
    Data_encoding.(obj1 (req "sandbox_file" string))
    (function Invalid_sandbox_file s -> Some s | _ -> None)
    (fun s -> Invalid_sandbox_file s)

module Event = struct
  include Internal_event.Simple

  let section = ["node"; "main"]

  let disabled_discovery_addr =
    declare_0
      ~section
      ~name:"disabled_discovery_addr"
      ~msg:"disabled local peer discovery"
      ~level:Notice
      ()

  let disabled_listen_addr =
    declare_0
      ~section
      ~name:"disabled_listen_addr"
      ~msg:"disabled P2P server"
      ~level:Notice
      ()

  let disabled_config_validation =
    declare_0
      ~section
      ~name:"disabled_config_validation"
      ~msg:"disabled node configuration validation"
      ~level:Warning
      ()

  let starting_local_rpc_server =
    declare_3
      ~section
      ~name:"starting_local_rpc_server"
      ~msg:"starting local RPC server on {host}:{port} (acl = {acl_policy})"
      ~level:Notice
      ("host", Data_encoding.string)
      ("port", Data_encoding.uint16)
      ("acl_policy", Data_encoding.string)

  let starting_internal_rpc_server =
    declare_0
      ~section
      ~name:"starting_internal_rpc_server"
      ~msg:"starting internal RPC server"
      ~level:Info
      ()

  let starting_metrics_server =
    declare_2
      ~section
      ~name:"starting_metrics_server"
      ~msg:"starting metrics server on {host}:{port}"
      ~level:Notice
      ("host", Data_encoding.string)
      ("port", Data_encoding.uint16)

  let starting_node =
    declare_3
      ~section
      ~name:"starting_node"
      ~msg:"starting the Tezos node v{version} ({git_info})"
      ~level:Notice
      ("chain", Distributed_db_version.Name.encoding)
      ~pp2:Tezos_version.Version.pp
      ("version", Tezos_version.Node_version.version_encoding)
      ("git_info", Data_encoding.string)

  let node_is_ready =
    declare_0
      ~section
      ~name:"node_is_ready"
      ~msg:"the Tezos node is now running"
      ~level:Notice
      ()

  let shutting_down_node =
    declare_0
      ~section
      ~name:"shutting_down_node"
      ~msg:"shutting down the Tezos node"
      ~level:Notice
      ()

  let shutting_down_local_rpc_server =
    declare_0
      ~section
      ~name:"shutting_down_local_rpc_server"
      ~msg:"shutting down the local RPC server"
      ~level:Info
      ()

  let bye =
    (* Note that "exit_code" may be negative in case of signals. *)
    declare_1
      ~section
      ~name:"bye"
      ~msg:"bye"
      ~level:Notice
      (* may be negative in case of signals *)
      ("exit_code", Data_encoding.int31)

  let metrics_ended =
    declare_1
      ~section
      ~name:"metrics_ended"
      ~level:Error
      ~msg:"metrics server ended with error {stacktrace}"
      ("stacktrace", Data_encoding.string)

  let incorrect_history_mode =
    declare_2
      ~section
      ~name:"incorrect_history_mode"
      ~msg:
        "The given history mode {given_history_mode} does not correspond to \
         the stored history mode {stored_history_mode}. If you wish to force \
         the switch, use the flag '--force-history-mode-switch'."
      ~level:Error
      ~pp1:History_mode.pp
      ("given_history_mode", History_mode.encoding)
      ~pp2:History_mode.pp
      ("stored_history_mode", History_mode.encoding)
end

open Filename.Infix

let init_identity_file (config : Config_file.t) =
  let check_data_dir ~data_dir =
    let dummy_genesis =
      {
        Genesis.time = Time.Protocol.epoch;
        block = Block_hash.zero;
        protocol = Protocol_hash.zero;
      }
    in
    Data_version.ensure_data_dir ~mode:Exists dummy_genesis data_dir
  in
  let identity_file =
    config.data_dir // Data_version.default_identity_file_name
  in
  Identity_file.init
    ~check_data_dir
    ~identity_file
    ~expected_pow:config.p2p.expected_pow

let init_node ?sandbox ?target ~identity ~singleprocess ~internal_events
    ~force_history_mode_switch (config : Config_file.t) =
  let open Lwt_result_syntax in
  (* TODO "WARN" when pow is below our expectation. *)
  let*! () =
    if config.disable_config_validation then
      Event.(emit disabled_config_validation) ()
    else Lwt.return_unit
  in
  let* discovery_addr, discovery_port =
    match config.p2p.discovery_addr with
    | None ->
        let*! () = Event.(emit disabled_discovery_addr) () in
        return (None, None)
    | Some addr -> (
        let* addrs = Config_file.resolve_discovery_addrs addr in
        match addrs with
        | [] -> failwith "Cannot resolve P2P discovery address: %S" addr
        | (addr, port) :: _ -> return (Some addr, Some port))
  in
  let* listening_addr, listening_port =
    match config.p2p.listen_addr with
    | None ->
        let*! () = Event.(emit disabled_listen_addr) () in
        return (None, None)
    | Some addr -> (
        let* addrs = Config_file.resolve_listening_addrs addr in
        match addrs with
        | [] -> failwith "Cannot resolve P2P listening address: %S" addr
        | (addr, port) :: _ -> return (Some addr, Some port))
  in
  let* p2p_config =
    match (listening_addr, sandbox) with
    | Some addr, Some _ when Ipaddr.V6.(compare addr unspecified) = 0 ->
        return_none
    | Some addr, Some _ when not (Ipaddr.V6.is_private addr) ->
        tzfail (Non_private_sandbox addr)
    | None, Some _ -> return_none
    | _ ->
        let* trusted_points =
          Config_file.resolve_bootstrap_addrs
            (Config_file.bootstrap_peers config)
        in
        let advertised_port : P2p_addr.port option =
          Option.either config.p2p.advertised_net_port listening_port
        in
        let p2p_config : P2p.config =
          {
            listening_addr;
            listening_port;
            advertised_port;
            discovery_addr;
            discovery_port;
            trusted_points;
            peers_file = config.data_dir // Data_version.default_peers_file_name;
            private_mode = config.p2p.private_mode;
            reconnection_config = config.p2p.reconnection_config;
            identity;
            proof_of_work_target =
              Tezos_crypto.Crypto_box.make_pow_target config.p2p.expected_pow;
            trust_discovered_peers = sandbox <> None;
            disable_peer_discovery = config.p2p.disable_peer_discovery;
          }
        in
        return_some (p2p_config, config.p2p.limits)
  in
  let* sandbox_param =
    match (config.blockchain_network.genesis_parameters, sandbox) with
    | None, None -> return_none
    | Some parameters, None ->
        return_some (parameters.context_key, parameters.values)
    | _, Some filename ->
        let* json =
          trace (Invalid_sandbox_file filename)
          @@ Lwt_utils_unix.Json.read_file filename
        in
        return_some ("sandbox_parameter", json)
  in
  let genesis = config.blockchain_network.genesis in
  let patch_context =
    Some (Patch_context.patch_context genesis sandbox_param)
  in
  let node_config : Node.config =
    {
      genesis;
      chain_name = config.blockchain_network.chain_name;
      sandboxed_chain_name = config.blockchain_network.sandboxed_chain_name;
      user_activated_upgrades =
        config.blockchain_network.user_activated_upgrades;
      user_activated_protocol_overrides =
        config.blockchain_network.user_activated_protocol_overrides;
      operation_metadata_size_limit =
        config.shell.block_validator_limits.operation_metadata_size_limit;
      patch_context;
      data_dir = config.data_dir;
      internal_events;
      store_root = Data_version.store_dir config.data_dir;
      context_root = Data_version.context_dir config.data_dir;
      protocol_root = Data_version.protocol_dir config.data_dir;
      p2p = p2p_config;
      target;
      enable_testchain = config.p2p.enable_testchain;
      disable_mempool = config.p2p.disable_mempool;
      dal_config = config.blockchain_network.dal_config;
    }
  in
  let* () =
    match config.shell.history_mode with
    | Some history_mode when force_history_mode_switch ->
        Store.may_switch_history_mode
          ~store_dir:node_config.store_root
          ~context_dir:node_config.context_root
          genesis
          ~new_history_mode:history_mode
    | _ -> return_unit
  in
  let version =
    Tezos_version.Version.to_string Tezos_version_value.Current_git_info.version
  in
  let commit_info =
    ({
       commit_hash = Tezos_version_value.Current_git_info.commit_hash;
       commit_date = Tezos_version_value.Current_git_info.committer_date;
     }
      : Tezos_version.Node_version.commit_info)
  in
  Node.create
    ~sandboxed:(sandbox <> None)
    ?sandbox_parameters:(Option.map snd sandbox_param)
    ~singleprocess
    ~version
    ~commit_info
    node_config
    config.shell.peer_validator_limits
    config.shell.block_validator_limits
    config.shell.prevalidator_limits
    config.shell.chain_validator_limits
    config.shell.history_mode

let rpc_metrics =
  Prometheus.Summary.v_labels
    ~label_names:["endpoint"; "method"]
    ~help:"RPC endpoint call counts and sum of execution times."
    ~namespace:Tezos_version.Node_version.namespace
    ~subsystem:"rpc"
    "calls"

module Metrics_server = Prometheus_app.Cohttp (Cohttp_lwt_unix.Server)

type port = int

type socket_file = string

type single_server_kind =
  | Process of socket_file
  | Local of Conduit_lwt_unix.server * port

let extract_mode = function
  | Process socket_file -> `Unix_domain_socket (`File socket_file)
  | Local (mode, _) -> mode

(* Add default accepted CORS headers *)
let sanitize_cors_headers ~default headers =
  List.map String.lowercase_ascii headers
  |> String.Set.of_list
  |> String.Set.(union (of_list default))
  |> String.Set.elements

(* Launches an RPC server depending on the given server kind *)
let launch_rpc_server (config : Config_file.t) dir rpc_server_kind addr =
  let open Lwt_result_syntax in
  let rpc_config = config.rpc in
  let media_types = rpc_config.media_type in
  let host = Ipaddr.V6.to_string addr in
  let* acl =
    (* Also emits events depending on server kind *)
    match rpc_server_kind with
    | Process _ ->
        let*! () = Event.(emit starting_internal_rpc_server) () in
        return_none
    | Local (mode, port) ->
        let*! acl_policy = RPC_server.Acl.resolve_domain_names rpc_config.acl in
        let acl =
          let open RPC_server.Acl in
          find_policy acl_policy (Ipaddr.V6.to_string addr, Some port)
          |> Option.value_f ~default:(fun () -> default addr)
        in
        let*! () =
          match (mode : Conduit_lwt_unix.server) with
          | `TCP _ | `TLS _ | `Unix_domain_socket _ ->
              Event.(emit starting_local_rpc_server)
                (host, port, RPC_server.Acl.policy_type acl)
          | _ -> Lwt.return_unit
        in
        return_some acl
  in
  let cors =
    let cors_headers =
      sanitize_cors_headers ~default:["Content-Type"] rpc_config.cors_headers
    in
    Resto_cohttp.Cors.
      {
        allowed_origins = rpc_config.cors_origins;
        allowed_headers = cors_headers;
      }
  in
  let server =
    RPC_server.init_server
      ~cors
      ?acl
      ~media_types:(Media_type.Command_line.of_command_line media_types)
      dir
  in
  let callback (conn : Cohttp_lwt_unix.Server.conn) req body =
    let path = Cohttp.Request.uri req |> Uri.path in
    if path = "/metrics" then
      let*! response = Metrics_server.callback conn req body in
      Lwt.return (`Response response)
    else Tezos_rpc_http_server.RPC_server.resto_callback server conn req body
  in
  let update_metrics uri meth =
    Prometheus.Summary.(time (labels rpc_metrics [uri; meth]) Sys.time)
  in
  let callback =
    RPC_middleware.rpc_metrics_transform_callback ~update_metrics dir callback
  in
  let mode = extract_mode rpc_server_kind in
  Lwt.catch
    (fun () ->
      let*! () =
        RPC_server.launch
          ~host
          server
          ~callback
          ~max_active_connections:config.rpc.max_active_rpc_connections
          mode
      in
      return server)
    (function
      (* FIXME: https://gitlab.com/tezos/tezos/-/issues/1312
         This exception seems to be unreachable.
      *)
      | Unix.Unix_error (Unix.EADDRINUSE, "bind", "") as exn -> (
          match rpc_server_kind with
          | Process _ -> fail_with_exn exn
          | Local (_, port) -> tzfail (RPC_Port_already_in_use [(addr, port)]))
      | exn -> fail_with_exn exn)

(* Describes the kind of servers that can be handled by the node.
   - Local_rpc_server: RPC server is run by the node itself
     (this may block the node in case of heavy RPC load),
   - External_rpc_server: RPC server is spawned as an external
     process,
   - No_server: the node is not responding to any RPC. *)
type rpc_server_kind =
  | Local_rpc_server of RPC_server.server list
  | External_rpc_server of (RPC_server.server * Rpc_process_worker.t) list
  | No_server

(* Initializes an RPC server handled by the node main process. *)
let init_local_rpc_server (config : Config_file.t) dir =
  let open Lwt_result_syntax in
  let* servers =
    List.concat_map_es
      (fun addr ->
        let* addrs = Config_file.resolve_rpc_listening_addrs addr in
        match addrs with
        | [] -> failwith "Cannot resolve listening address: %S" addr
        | addrs ->
            List.map_es
              (fun (addr, port) ->
                let mode =
                  match config.rpc.tls with
                  | None -> `TCP (`Port port)
                  | Some {cert; key} ->
                      `TLS
                        ( `Crt_file_path cert,
                          `Key_file_path key,
                          `No_password,
                          `Port port )
                in
                launch_rpc_server config dir (Local (mode, port)) addr)
              addrs)
      config.rpc.local_listen_addrs
  in
  return (Local_rpc_server servers)

let rpc_socket_path ~socket_dir ~id ~pid =
  let filename = Format.sprintf "octez-external-rpc-socket-%d-%d" pid id in
  Filename.concat socket_dir filename

(* Initializes an RPC server handled by the node process. It will be
   used by an external RPC process, identified by [id], to forward
   RPCs to the node through a Unix socket. *)
let init_local_rpc_server_for_external_process id (config : Config_file.t) dir
    addr =
  let open Lwt_result_syntax in
  let socket_dir = Tezos_base_unix.Socket.get_temporary_socket_dir () in
  let pid = Unix.getpid () in
  let comm_socket_path = rpc_socket_path ~id ~socket_dir ~pid in
  (* Register a clean up callback to clean the comm_socket_path when
     shutting down. Indeed, the socket file is created by the
     Conduit-lwt-unix.Conduit_lwt_server.listen function, but the
     resource is not cleaned. *)
  let _ =
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
        Lwt_unix.unlink comm_socket_path)
  in
  let* rpc_server =
    launch_rpc_server config dir (Process comm_socket_path) addr
  in
  return (rpc_server, comm_socket_path)

let init_external_rpc_server config node_version dir internal_events =
  let open Lwt_result_syntax in
  (* Start one rpc_process for each rpc endpoint. *)
  let id = ref 0 in
  let* rpc_servers =
    List.concat_map_ep
      (fun addr ->
        let* addrs = Config_file.resolve_rpc_listening_addrs addr in
        match addrs with
        | [] -> failwith "Cannot resolve listening address: %S" addr
        | addrs ->
            List.map_ep
              (fun (p2p_point : P2p_point.Id.t) ->
                let id =
                  let curid = !id in
                  incr id ;
                  curid
                in
                let* local_rpc_server, comm_socket_path =
                  init_local_rpc_server_for_external_process
                    id
                    config
                    dir
                    (fst p2p_point)
                in
                let addr = P2p_point.Id.to_string p2p_point in
                (* Update the config sent to the rpc_process to
                   start so that it contains a single listen
                   address. *)
                let config =
                  {config with rpc = {config.rpc with listen_addrs = [addr]}}
                in
                let rpc_process =
                  Octez_rpc_process.Rpc_process_worker.create
                    ~comm_socket_path
                    config
                    node_version
                    internal_events
                in
                let* () =
                  Octez_rpc_process.Rpc_process_worker.start rpc_process
                in
                return (local_rpc_server, rpc_process))
              addrs)
      config.rpc.listen_addrs
  in
  return (External_rpc_server rpc_servers)

let metrics_serve metrics_addrs =
  let open Lwt_result_syntax in
  let* addrs = List.map_ep Config_file.resolve_metrics_addrs metrics_addrs in
  let*! servers =
    List.map_p
      (fun (addr, port) ->
        let host = Ipaddr.V6.to_string addr in
        let*! () = Event.(emit starting_metrics_server) (host, port) in
        let*! ctx = Conduit_lwt_unix.init ~src:host () in
        let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
        let mode = `TCP (`Port port) in
        let callback = Metrics_server.callback in
        Cohttp_lwt_unix.Server.create
          ~ctx
          ~mode
          (Cohttp_lwt_unix.Server.make ~callback ()))
      (List.flatten addrs)
  in
  return servers

(* This call is not strictly necessary as the parameters are initialized
   lazily the first time a Sapling operation (validation or forging) is
   done. This is what the client does.
   For a long running binary however it is important to make sure that the
   parameters files are there at the start and avoid failing much later while
   validating an operation. Plus paying this cost upfront means that the first
   validation will not be more expensive. *)
let init_zcash () =
  try
    Tezos_sapling.Core.Validator.init_params () ;
    Lwt.return_unit
  with exn ->
    Lwt.fail_with
      (Printf.sprintf
         "Failed to initialize Zcash parameters: %s"
         (Printexc.to_string exn))

let init_rpc (config : Config_file.t) (node : Node.t) internal_events =
  let open Lwt_result_syntax in
  (* Start local RPC server (handled by the node main process) only
     when at least one local listen addr is given. *)
  let commit_info =
    ({
       commit_hash = Tezos_version_value.Current_git_info.commit_hash;
       commit_date = Tezos_version_value.Current_git_info.committer_date;
     }
      : Tezos_version.Node_version.commit_info)
  in
  let node_version = Node.get_version node in

  let dir = Node.build_rpc_directory ~node_version ~commit_info node in
  let dir = Node_directory.build_node_directory config dir in
  let dir =
    Tezos_rpc.Directory.register_describe_directory_service
      dir
      Tezos_rpc.Service.description_service
  in

  let* local_rpc_server =
    if config.rpc.local_listen_addrs = [] then return No_server
    else init_local_rpc_server config dir
  in
  (* Start RPC process only when at least one listen addr is given. *)
  let* rpc_server =
    if config.rpc.listen_addrs = [] then return No_server
    else
      (* Starts the node's local RPC server that aims to handle the
         RPCs forwarded by the rpc_process, if they cannot be
         processed by the rpc_process itself. *)
      init_external_rpc_server config node_version dir internal_events
  in
  return (local_rpc_server :: [rpc_server])

let run ?verbosity ?sandbox ?target ?(cli_warnings = [])
    ?ignore_testchain_warning ~singleprocess ~force_history_mode_switch
    (config : Config_file.t) =
  let open Lwt_result_syntax in
  (* Main loop *)
  let internal_events =
    match config.internal_events with
    | Some ie -> ie
    | None ->
        Tezos_base_unix.Internal_event_unix.make_with_defaults
          ~enable_default_daily_logs_at:
            Filename.Infix.(config.data_dir // "daily_logs")
          ?verbosity
          ~log_cfg:config.log
          ()
  in
  let*! () =
    Tezos_base_unix.Internal_event_unix.init ~config:internal_events ()
  in
  let*! () =
    Lwt_list.iter_s (fun evt -> Internal_event.Simple.emit evt ()) cli_warnings
  in
  let* () =
    Data_version.ensure_data_dir
      ~mode:Is_compatible
      config.blockchain_network.genesis
      config.data_dir
  in
  let* () = Config_validation.check ?ignore_testchain_warning config in
  let* identity = init_identity_file config in
  Updater.init (Data_version.protocol_dir config.data_dir) ;
  let*! () =
    Event.(emit starting_node)
      ( config.blockchain_network.chain_name,
        Tezos_version_value.Current_git_info.version,
        Tezos_version_value.Current_git_info.abbreviated_commit_hash )
  in
  let*! () = init_zcash () in
  let* () =
    let find_srs_files () = Tezos_base.Dal_srs.find_trusted_setup_files () in
    Tezos_crypto_dal.Cryptobox.Config.init_dal
      ~find_srs_files
      config.blockchain_network.dal_config
  in
  let*! node =
    init_node
      ?sandbox
      ?target
      ~internal_events
      ~identity
      ~singleprocess
      ~force_history_mode_switch
      config
  in
  let*! () =
    Result.iter_error_s
      (function
        | Store_errors.Cannot_switch_history_mode {previous_mode; next_mode}
          :: _ ->
            Event.(emit incorrect_history_mode) (previous_mode, next_mode)
        | _ -> Lwt.return_unit)
      node
  in
  let*? node in
  let log_node_downer =
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
        Event.(emit shutting_down_node) ())
  in
  let* rpc_servers = init_rpc config node internal_events in
  let rpc_downer =
    Lwt_exit.register_clean_up_callback
      ~loc:__LOC__
      ~after:[log_node_downer]
      (fun _ ->
        let*! () = Event.(emit shutting_down_local_rpc_server) () in
        List.iter_s
          (function
            | No_server -> Lwt.return_unit
            | External_rpc_server rpc_servers ->
                List.iter_p
                  (fun (local_server, rpc_process) ->
                    (* Stop the RPC_process first to avoid requests to
                       be forwarded to the note with a RPC_server that
                       is down. *)
                    let*! () =
                      Octez_rpc_process.Rpc_process_worker.stop rpc_process
                    in
                    let*! () = RPC_server.shutdown local_server in
                    Lwt.return_unit)
                  rpc_servers
            | Local_rpc_server rpc_server ->
                List.iter_p RPC_server.shutdown rpc_server)
          rpc_servers)
  in
  let node_downer =
    Lwt_exit.register_clean_up_callback
      ~loc:__LOC__
      ~after:[rpc_downer]
      (fun _ -> Node.shutdown node)
  in
  let*! () = Event.(emit node_is_ready) () in
  let _ =
    Lwt_exit.register_clean_up_callback
      ~loc:__LOC__
      ~after:[node_downer]
      (fun exit_status ->
        let*! () = Event.(emit bye) exit_status in
        Tezos_base_unix.Internal_event_unix.close ())
  in
  Lwt.dont_wait
    (fun () ->
      let*! r = metrics_serve config.metrics_addr in
      match r with
      | Ok _ -> Lwt.return_unit
      | Error err ->
          Event.(emit metrics_ended (Format.asprintf "%a" pp_print_trace err)))
    (fun exn ->
      Event.(
        emit__dont_wait__use_with_care metrics_ended (Printexc.to_string exn))) ;
  Lwt_utils.never_ending ()

let process sandbox verbosity target singleprocess force_history_mode_switch
    args =
  let open Lwt_result_syntax in
  let verbosity =
    let open Internal_event in
    match verbosity with [] -> None | [_] -> Some Info | _ -> Some Debug
  in
  let main_promise =
    let cli_warnings = ref [] in
    let* config =
      Shared_arg.read_and_patch_config_file
        ~ignore_bootstrap_peers:
          (match sandbox with Some _ -> true | None -> false)
        ~emit:(fun event () ->
          cli_warnings := event :: !cli_warnings ;
          Lwt.return_unit)
        args
    in
    let* () =
      match sandbox with
      | Some _ when config.data_dir = Config_file.default_data_dir ->
          failwith "Cannot use default data directory while in sandbox mode"
      | _ -> return_unit
    in
    let* target =
      match target with
      | None -> return_none
      | Some s ->
          let l = String.split_on_char ',' s in
          Lwt.catch
            (fun () ->
              assert (Compare.List_length_with.(l = 2)) ;
              let target =
                match l with
                | [block_hash; level] ->
                    ( Block_hash.of_b58check_exn block_hash,
                      Int32.of_string level )
                | _ -> assert false
              in
              return_some target)
            (fun _ ->
              failwith
                "Failed to parse the provided target. A '<block_hash>,<level>' \
                 value was expected.")
    in
    Lwt_lock_file.try_with_lock
      ~when_locked:(fun () ->
        failwith "Data directory is locked by another process")
      ~filename:(Data_version.lock_file config.data_dir)
    @@ fun () ->
    Lwt.catch
      (fun () ->
        run
          ?sandbox
          ?verbosity
          ?target
          ~singleprocess
          ~force_history_mode_switch
          ~cli_warnings:!cli_warnings
          ~ignore_testchain_warning:args.enable_testchain
          config)
      (function exn -> fail_with_exn exn)
  in
  Lwt.Exception_filter.(set handle_all_except_runtime) ;
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
  let verbosity =
    let open Cmdliner in
    let doc =
      "Increase log level. Using $(b,-v) is equivalent to using \
       $(b,TEZOS_LOG='* -> info'), and $(b,-vv) is equivalent to using \
       $(b,TEZOS_LOG='* -> debug')."
    in
    Arg.(
      value & flag_all & info ~docs:Shared_arg.Manpage.misc_section ~doc ["v"])

  let sandbox =
    let open Cmdliner in
    let doc =
      "Run the daemon in sandbox mode. P2P to non-localhost addresses are \
       disabled, and constants of the economic protocol can be altered with a \
       JSON file which overrides the $(b,genesis_parameters) field of the \
       network configuration (e.g. scripts/sandbox.json). $(b,IMPORTANT): \
       Using sandbox mode affects the node state and subsequent runs of Tezos \
       node must also use sandbox mode. In order to run the node in normal \
       mode afterwards, a full reset must be performed (by removing the node's \
       data directory)."
    in
    Arg.(
      value
      & opt (some non_dir_file) None
      & info
          ~docs:Shared_arg.Manpage.misc_section
          ~doc
          ~docv:"FILE.json"
          ["sandbox"])

  let target =
    let open Cmdliner in
    let doc =
      "When asked to take a block as a target, the daemon will only accept the \
       chains that contains that block and those that might reach it."
    in
    Arg.(
      value
      & opt (some string) None
      & info
          ~docs:Shared_arg.Manpage.misc_section
          ~doc
          ~docv:"<block_hash>,<level>"
          ["target"])

  let singleprocess =
    let open Cmdliner in
    let doc =
      "When enabled, it deactivates block validation using an external \
       process. Thus, the validation procedure is done in the same process as \
       the node and might not be responding when doing extensive I/Os."
    in
    Arg.(
      value & flag
      & info ~docs:Shared_arg.Manpage.misc_section ~doc ["singleprocess"])

  let force_history_mode_switch =
    let open Cmdliner in
    let doc =
      Format.sprintf
        "Forces the switch of history modes when a different history mode is \
         found between the written configuration and the given history mode.  \
         Warning: this option will modify the storage irremediably. Please \
         refer to the Tezos node documentation for more details."
    in
    Arg.(
      value & flag
      & info
          ~docs:Shared_arg.Manpage.misc_section
          ~doc
          ["force-history-mode-switch"])

  let term =
    Cmdliner.Term.(
      ret
        (const process $ sandbox $ verbosity $ target $ singleprocess
       $ force_history_mode_switch $ Shared_arg.Term.args))
end

module Manpage = struct
  let command_description =
    "The $(b,run) command is meant to run the Tezos node. Most of its command \
     line arguments corresponds to config file entries, and will have priority \
     over the latter if used."

  let description = [`S "DESCRIPTION"; `P command_description]

  let debug =
    let log_sections =
      String.concat
        " "
        (List.of_seq (Internal_event.get_registered_sections ()))
    in
    [
      `S "DEBUG";
      `P
        ("The environment variable $(b,TEZOS_LOG) is used to fine-tune what is \
          going to be logged. The syntax is \
          $(b,TEZOS_LOG='<section> -> <level> [ ; ...]') where section is one \
          of $(i," ^ log_sections
       ^ ") and level is one of $(i,fatal), $(i,error), $(i,warn), \
          $(i,notice), $(i,info) or $(i,debug). A $(b,*) can be used as a \
          wildcard in sections, i.e. $(b, node* -> debug). The rules are \
          matched left to right, therefore the leftmost rule is highest \
          priority .");
    ]

  let examples =
    [
      `S "EXAMPLES";
      `I
        ( "$(b,Run in sandbox mode listening to RPC commands at localhost port \
           8732)",
          "$(mname) run \
           --sandbox=src/proto_alpha/parameters/sandbox-parameters.json \
           --data-dir /custom/data/dir --rpc-addr localhost:8732" );
      `I ("$(b,Run a node that accepts network connections)", "$(mname) run");
    ]

  let man =
    description @ Shared_arg.Manpage.args @ debug @ examples
    @ Shared_arg.Manpage.bugs

  let info = Cmdliner.Cmd.info ~doc:"Run the Tezos node" ~man "run"
end

let cmd = Cmdliner.Cmd.v Manpage.info Term.term

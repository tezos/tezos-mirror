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

  let read_identity =
    declare_1
      ~section
      ~name:"read_identity"
      ~msg:"read identity file"
      ~level:Notice
      ("peer_id", P2p_peer.Id.encoding)

  let generating_identity =
    declare_0
      ~section
      ~name:"generating_identity"
      ~msg:"generating an identity file"
      ~level:Notice
      ()

  let identity_generated =
    declare_1
      ~section
      ~name:"identity_generated"
      ~msg:"identity file generated"
      ~level:Notice
      ("peer_id", P2p_peer.Id.encoding)

  let disabled_config_validation =
    declare_0
      ~section
      ~name:"disabled_config_validation"
      ~msg:"disabled node configuration validation"
      ~level:Warning
      ()

  let starting_rpc_server =
    declare_4
      ~section
      ~name:"starting_rpc_server"
      ~msg:"starting RPC server on {host}:{port} (acl = {acl_policy})"
      ~level:Notice
      ("host", Data_encoding.string)
      ("port", Data_encoding.uint16)
      ("tls", Data_encoding.bool)
      ("acl_policy", Data_encoding.string)

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

  let shutting_down_rpc_server =
    declare_0
      ~section
      ~name:"shutting_down_rpc_server"
      ~msg:"shutting down the RPC server"
      ~level:Notice
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
  let open Lwt_result_syntax in
  let identity_file =
    config.data_dir // Data_version.default_identity_file_name
  in
  if Sys.file_exists identity_file then
    let* identity = Node_identity_file.read identity_file in
    let*! () = Event.(emit read_identity) identity.peer_id in
    return identity
  else
    let*! () = Event.(emit generating_identity) () in
    let* identity =
      Node_identity_file.generate identity_file config.p2p.expected_pow
    in
    let*! () = Event.(emit identity_generated) identity.peer_id in
    return identity

let init_node ?sandbox ?target ~identity ~singleprocess
    ~external_validator_log_config ~force_history_mode_switch
    (config : Config_file.t) =
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
      external_validator_log_config;
      store_root = Data_version.store_dir config.data_dir;
      context_root = Data_version.context_dir config.data_dir;
      protocol_root = Data_version.protocol_dir config.data_dir;
      p2p = p2p_config;
      target;
      enable_testchain = config.p2p.enable_testchain;
      disable_mempool = config.p2p.disable_mempool;
      dal = config.dal;
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
  Node.create
    ~sandboxed:(sandbox <> None)
    ?sandbox_parameters:(Option.map snd sandbox_param)
    ~singleprocess
    node_config
    config.shell.peer_validator_limits
    config.shell.block_validator_limits
    config.shell.prevalidator_limits
    config.shell.chain_validator_limits
    config.shell.history_mode

(* Add default accepted CORS headers *)
let sanitize_cors_headers ~default headers =
  List.map String.lowercase_ascii headers
  |> String.Set.of_list
  |> String.Set.(union (of_list default))
  |> String.Set.elements

let rpc_metrics =
  Prometheus.Summary.v_labels
    ~label_names:["endpoint"; "method"]
    ~help:"RPC endpoint call counts and sum of execution times."
    ~namespace:Tezos_version.Node_version.namespace
    ~subsystem:"rpc"
    "calls"

module Metrics_server = Prometheus_app.Cohttp (Cohttp_lwt_unix.Server)

let launch_rpc_server ~acl_policy ~media_types (config : Config_file.t) node
    (addr, port) =
  let open Lwt_result_syntax in
  let rpc_config = config.rpc in
  let host = Ipaddr.V6.to_string addr in
  let dir = Node.build_rpc_directory node in
  let dir = Node_directory.build_node_directory config dir in
  let dir =
    Tezos_rpc.Directory.register_describe_directory_service
      dir
      Tezos_rpc.Service.description_service
  in
  let mode =
    match rpc_config.tls with
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
    Event.(emit starting_rpc_server)
      (host, port, rpc_config.tls <> None, RPC_server.Acl.policy_type acl)
  in
  let cors_headers =
    sanitize_cors_headers ~default:["Content-Type"] rpc_config.cors_headers
  in
  let cors =
    RPC_server.
      {
        allowed_origins = rpc_config.cors_origins;
        allowed_headers = cors_headers;
      }
  in
  let server =
    RPC_server.init_server
      ~cors
      ~acl
      ~media_types:(Media_type.Command_line.of_command_line media_types)
      dir
  in
  let callback (conn : Cohttp_lwt_unix.Server.conn) req body =
    let path = Cohttp_lwt.Request.uri req |> Uri.path in
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
  Lwt.catch
    (fun () ->
      let*! () = RPC_server.launch ~host server ~callback mode in
      return server)
    (function
      (* FIXME: https://gitlab.com/tezos/tezos/-/issues/1312
         This exception seems to be unreachable.
      *)
      | Unix.Unix_error (Unix.EADDRINUSE, "bind", "") ->
          tzfail (RPC_Port_already_in_use [(addr, port)])
      | exn -> fail_with_exn exn)

let init_rpc (config : Config_file.t) node =
  let open Lwt_result_syntax in
  let media_types = config.rpc.media_type in
  List.concat_map_es
    (fun addr ->
      let* addrs = Config_file.resolve_rpc_listening_addrs addr in
      match addrs with
      | [] -> failwith "Cannot resolve listening address: %S" addr
      | addrs ->
          let*! acl_policy =
            RPC_server.Acl.resolve_domain_names config.rpc.acl
          in
          List.map_es
            (fun addr ->
              launch_rpc_server ~acl_policy ~media_types config node addr)
            addrs)
    config.rpc.listen_addrs

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

let run ?verbosity ?sandbox ?target ?(cli_warnings = [])
    ?ignore_testchain_warning ~singleprocess ~force_history_mode_switch
    (config : Config_file.t) =
  let open Lwt_result_syntax in
  (* Main loop *)
  let log_cfg =
    {
      config.log with
      default_level = Option.value verbosity ~default:config.log.default_level;
    }
  in
  let internal_events =
    Tezos_base_unix.Internal_event_unix.make_with_defaults
      ~enable_default_daily_logs_at:
        Filename.Infix.(config.data_dir // "daily_logs")
      ?internal_events:config.internal_events
      ()
  in
  let*! () =
    Tezos_base_unix.Internal_event_unix.init ~log_cfg ~internal_events ()
  in
  let external_validator_log_config =
    External_validation.{internal_events; lwt_log_sink_unix = log_cfg}
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
        Tezos_version.Current_git_info.version,
        Tezos_version.Current_git_info.abbreviated_commit_hash )
  in
  let*! () = init_zcash () in
  let* () =
    let find_srs_files () = Tezos_base.Dal_srs.find_trusted_setup_files () in
    Tezos_crypto_dal.Cryptobox.Config.init_dal ~find_srs_files config.dal
  in
  let*! node =
    init_node
      ?sandbox
      ?target
      ~external_validator_log_config
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
  let node_downer =
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
        let*! () = Event.(emit shutting_down_node) () in
        Node.shutdown node)
  in
  let* rpc = init_rpc config node in
  let rpc_downer =
    Lwt_exit.register_clean_up_callback
      ~loc:__LOC__
      ~after:[node_downer]
      (fun _ ->
        let*! () = Event.(emit shutting_down_rpc_server) () in
        List.iter_p RPC_server.shutdown rpc)
  in
  let*! () = Event.(emit node_is_ready) () in
  let _ =
    Lwt_exit.register_clean_up_callback
      ~loc:__LOC__
      ~after:[rpc_downer]
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

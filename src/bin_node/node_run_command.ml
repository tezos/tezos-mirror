(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs, <contact@nomadic-labs.com>          *)
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
        "The node is configured to listen on a public address (%a), while \
         only 'private' networks are authorised with `--sandbox`.\n\
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
    declare_3
      ~section
      ~name:"starting_rpc_server"
      ~msg:"starting RPC server on {host}:{port}"
      ~level:Notice
      ("host", Data_encoding.string)
      ("port", Data_encoding.uint16)
      ("tls", Data_encoding.bool)

  let starting_node =
    declare_1
      ~section
      ~name:"starting_node"
      ~msg:"starting the Tezos node"
      ~level:Notice
      ("chain", Distributed_db_version.Name.encoding)

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
      ("exit_code", Data_encoding.int31)
end

open Filename.Infix

let init_identity_file (config : Node_config_file.t) =
  let identity_file =
    config.data_dir // Node_data_version.default_identity_file_name
  in
  if Sys.file_exists identity_file then
    Node_identity_file.read identity_file
    >>=? fun identity ->
    Event.(emit read_identity) identity.peer_id >>= fun () -> return identity
  else
    Event.(emit generating_identity) ()
    >>= fun () ->
    Node_identity_file.generate identity_file config.p2p.expected_pow
    >>=? fun identity ->
    Event.(emit identity_generated) identity.peer_id
    >>= fun () -> return identity

let init_node ?sandbox ?checkpoint ~identity ~singleprocess
    (config : Node_config_file.t) =
  (* TODO "WARN" when pow is below our expectation. *)
  ( match config.disable_config_validation with
  | true ->
      Event.(emit disabled_config_validation) ()
  | false ->
      Lwt.return_unit )
  >>= fun () ->
  ( match config.p2p.discovery_addr with
  | None ->
      Event.(emit disabled_discovery_addr) () >>= fun () -> return (None, None)
  | Some addr -> (
      Node_config_file.resolve_discovery_addrs addr
      >>=? function
      | [] ->
          failwith "Cannot resolve P2P discovery address: %S" addr
      | (addr, port) :: _ ->
          return (Some addr, Some port) ) )
  >>=? fun (discovery_addr, discovery_port) ->
  ( match config.p2p.listen_addr with
  | None ->
      Event.(emit disabled_listen_addr) () >>= fun () -> return (None, None)
  | Some addr -> (
      Node_config_file.resolve_listening_addrs addr
      >>=? function
      | [] ->
          failwith "Cannot resolve P2P listening address: %S" addr
      | (addr, port) :: _ ->
          return (Some addr, Some port) ) )
  >>=? fun (listening_addr, listening_port) ->
  ( match (listening_addr, sandbox) with
  | (Some addr, Some _) when Ipaddr.V6.(compare addr unspecified) = 0 ->
      return_none
  | (Some addr, Some _) when not (Ipaddr.V6.is_private addr) ->
      fail (Non_private_sandbox addr)
  | (None, Some _) ->
      return_none
  | _ ->
      Node_config_file.resolve_bootstrap_addrs
        (Node_config_file.bootstrap_peers config)
      >>=? fun trusted_points ->
      let p2p_config : P2p.config =
        {
          listening_addr;
          listening_port;
          discovery_addr;
          discovery_port;
          trusted_points;
          peers_file =
            config.data_dir // Node_data_version.default_peers_file_name;
          private_mode = config.p2p.private_mode;
          reconnection_config = config.p2p.reconnection_config;
          identity;
          proof_of_work_target =
            Crypto_box.make_pow_target config.p2p.expected_pow;
          trust_discovered_peers = sandbox <> None;
        }
      in
      return_some (p2p_config, config.p2p.limits) )
  >>=? fun p2p_config ->
  ( match (config.blockchain_network.genesis_parameters, sandbox) with
  | (None, None) ->
      return_none
  | (Some parameters, None) ->
      return_some (parameters.context_key, parameters.values)
  | (_, Some filename) -> (
      Lwt_utils_unix.Json.read_file filename
      >>= function
      | Error _err ->
          fail (Invalid_sandbox_file filename)
      | Ok json ->
          return_some ("sandbox_parameter", json) ) )
  >>=? fun sandbox_param ->
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
      patch_context;
      data_dir = config.data_dir;
      store_root = Node_data_version.store_dir config.data_dir;
      context_root = Node_data_version.context_dir config.data_dir;
      protocol_root = Node_data_version.protocol_dir config.data_dir;
      p2p = p2p_config;
      checkpoint;
      enable_testchain = config.p2p.enable_testchain;
      disable_mempool = config.p2p.disable_mempool;
    }
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

let launch_rpc_server (config : Node_config_file.t) node (addr, port) =
  let rpc_config = config.rpc in
  let host = Ipaddr.V6.to_string addr in
  let dir = Node.build_rpc_directory node in
  let dir = Node_directory.build_node_directory config dir in
  let dir =
    RPC_directory.register_describe_directory_service
      dir
      RPC_service.description_service
  in
  let mode =
    match rpc_config.tls with
    | None ->
        `TCP (`Port port)
    | Some {cert; key} ->
        `TLS (`Crt_file_path cert, `Key_file_path key, `No_password, `Port port)
  in
  Event.(emit starting_rpc_server) (host, port, rpc_config.tls <> None)
  >>= fun () ->
  let cors_headers =
    sanitize_cors_headers ~default:["Content-Type"] rpc_config.cors_headers
  in
  Lwt.catch
    (fun () ->
      RPC_server.launch
        ~host
        mode
        dir
        ~media_types:Media_type.all_media_types
        ~cors:
          {
            allowed_origins = rpc_config.cors_origins;
            allowed_headers = cors_headers;
          }
      >>= return)
    (function
      | Unix.Unix_error (Unix.EADDRINUSE, "bind", "") ->
          fail (RPC_Port_already_in_use [(addr, port)])
      | exn ->
          Lwt.return (error_exn exn))

let init_rpc (config : Node_config_file.t) node =
  List.fold_right_es
    (fun addr acc ->
      Node_config_file.resolve_rpc_listening_addrs addr
      >>=? function
      | [] ->
          failwith "Cannot resolve listening address: %S" addr
      | addrs ->
          List.fold_right_es
            (fun x a ->
              launch_rpc_server config node x >>=? fun o -> return (o :: a))
            addrs
            acc)
    config.rpc.listen_addrs
    []

let run ?verbosity ?sandbox ?checkpoint ~singleprocess
    (config : Node_config_file.t) =
  Node_data_version.ensure_data_dir config.data_dir
  >>=? fun () ->
  Lwt_lock_file.create
    ~unlink_on_exit:true
    (Node_data_version.lock_file config.data_dir)
  >>=? fun () ->
  (* Main loop *)
  let log_cfg =
    match verbosity with
    | None ->
        config.log
    | Some default_level ->
        {config.log with default_level}
  in
  Internal_event_unix.init
    ~lwt_log_sink:log_cfg
    ~configuration:config.internal_events
    ()
  >>= fun () ->
  Node_config_validation.check config
  >>=? fun () ->
  init_identity_file config
  >>=? fun identity ->
  Updater.init (Node_data_version.protocol_dir config.data_dir) ;
  Event.(emit starting_node) config.blockchain_network.chain_name
  >>= fun () ->
  init_node ?sandbox ?checkpoint ~identity ~singleprocess config
  >>= (function
        | Ok node ->
            return node
        | Error
            (State.Incorrect_history_mode_switch {previous_mode; next_mode}
            :: _) ->
            failwith
              "@[Cannot switch from history mode '%a' to '%a'. In order to \
               change your history mode please refer to the Tezos node \
               documentation. @]"
              History_mode.Legacy.pp
              previous_mode
              History_mode.Legacy.pp
              next_mode
        | Error _ as err ->
            Lwt.return err)
  >>=? fun node ->
  let node_downer =
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
        Event.(emit shutting_down_node) () >>= fun () -> Node.shutdown node)
  in
  init_rpc config node
  >>=? fun rpc ->
  let rpc_downer =
    Lwt_exit.register_clean_up_callback
      ~loc:__LOC__
      ~after:[node_downer]
      (fun _ ->
        Event.(emit shutting_down_rpc_server) ()
        >>= fun () -> List.iter_p RPC_server.shutdown rpc)
  in
  Event.(emit node_is_ready) ()
  >>= fun () ->
  let _ =
    Lwt_exit.register_clean_up_callback
      ~loc:__LOC__
      ~after:[rpc_downer]
      (fun exit_status ->
        Event.(emit bye) exit_status >>= fun () -> Internal_event_unix.close ())
  in
  Lwt_utils.never_ending ()

let process sandbox verbosity checkpoint singleprocess args =
  let verbosity =
    let open Internal_event in
    match verbosity with [] -> None | [_] -> Some Info | _ -> Some Debug
  in
  let main_promise =
    Node_shared_arg.read_and_patch_config_file
      ~ignore_bootstrap_peers:
        (match sandbox with Some _ -> true | None -> false)
      args
    >>=? fun config ->
    ( match sandbox with
    | Some _ ->
        if config.data_dir = Node_config_file.default_data_dir then
          failwith "Cannot use default data directory while in sandbox mode"
        else return_unit
    | None ->
        return_unit )
    >>=? fun () ->
    ( match checkpoint with
    | None ->
        return_none
    | Some s -> (
      match Block_header.of_b58check s with
      | Some b ->
          return_some b
      | None ->
          failwith
            "Failed to parse the provided checkpoint (Base58Check-encoded)." )
    )
    >>=? fun checkpoint ->
    Lwt_lock_file.is_locked (Node_data_version.lock_file config.data_dir)
    >>=? function
    | false ->
        Lwt.catch
          (fun () -> run ?sandbox ?verbosity ?checkpoint ~singleprocess config)
          (function
            | Unix.Unix_error (Unix.EADDRINUSE, "bind", "") ->
                List.fold_right_es
                  (fun addr acc ->
                    Node_config_file.resolve_rpc_listening_addrs addr
                    >>=? fun x -> return (x @ acc))
                  config.rpc.listen_addrs
                  []
                >>=? fun addrlist -> fail (RPC_Port_already_in_use addrlist)
            | exn ->
                Lwt.return (error_exn exn))
    | true ->
        failwith "Data directory is locked by another process"
  in
  Lwt_main.run
    ( Lwt_exit.wrap_and_error main_promise
    >>= function
    | Ok (Ok _) ->
        Lwt_exit.exit_and_wait 0 >>= fun _ -> Lwt.return (`Ok ())
    | Ok (Error err) ->
        Lwt_exit.exit_and_wait 2
        >>= fun _ ->
        Lwt.return (`Error (false, Format.asprintf "%a" pp_print_error err))
    | Error exit_status ->
        Lwt.return (`Error (false, Format.asprintf "Exited %d" exit_status)) )

module Term = struct
  let verbosity =
    let open Cmdliner in
    let doc =
      "Increase log level. Using $(b,-v) is equivalent to using \
       $(b,TEZOS_LOG='* -> info'), and $(b,-vv) is equivalent to using \
       $(b,TEZOS_LOG='* -> debug')."
    in
    Arg.(
      value & flag_all
      & info ~docs:Node_shared_arg.Manpage.misc_section ~doc ["v"])

  let sandbox =
    let open Cmdliner in
    let doc =
      "Run the daemon in sandbox mode. P2P to non-localhost addresses are \
       disabled, and constants of the economic protocol can be altered with a \
       JSON file which overrides the $(b,genesis_parameters) field of the \
       network configuration (e.g. scripts/sandbox.json). $(b,IMPORTANT): \
       Using sandbox mode affects the node state and subsequent runs of Tezos \
       node must also use sandbox mode. In order to run the node in normal \
       mode afterwards, a full reset must be performed (by removing the \
       node's data directory)."
    in
    Arg.(
      value
      & opt (some non_dir_file) None
      & info
          ~docs:Node_shared_arg.Manpage.misc_section
          ~doc
          ~docv:"FILE.json"
          ["sandbox"])

  let checkpoint =
    let open Cmdliner in
    let doc =
      "When asked to take a block hash as a checkpoint, the daemon will only \
       accept the chains that contains that block and those that might reach \
       it."
    in
    Arg.(
      value
      & opt (some string) None
      & info
          ~docs:Node_shared_arg.Manpage.misc_section
          ~doc
          ~docv:"<level>,<block_hash>"
          ["checkpoint"])

  let singleprocess =
    let open Cmdliner in
    let doc =
      "When enabled, it deactivates block validation using an external \
       process. Thus, the validation procedure is done in the same process as \
       the node and might not be responding when doing extensive I/Os."
    in
    Arg.(
      value & flag
      & info ~docs:Node_shared_arg.Manpage.misc_section ~doc ["singleprocess"])

  let term =
    Cmdliner.Term.(
      ret
        ( const process $ sandbox $ verbosity $ checkpoint $ singleprocess
        $ Node_shared_arg.Term.args ))
end

module Manpage = struct
  let command_description =
    "The $(b,run) command is meant to run the Tezos node. Most of its command \
     line arguments corresponds to config file entries, and will have \
     priority over the latter if used."

  let description = [`S "DESCRIPTION"; `P command_description]

  let debug =
    let log_sections =
      String.concat
        " "
        (TzString.Set.elements (Internal_event.get_registered_sections ()))
    in
    [ `S "DEBUG";
      `P
        ( "The environment variable $(b,TEZOS_LOG) is used to fine-tune what \
           is going to be logged. The syntax is \
           $(b,TEZOS_LOG='<section> -> <level> [ ; ...]') where section is \
           one of $(i," ^ log_sections
        ^ ") and level is one of $(i,fatal), $(i,error), $(i,warn), \
           $(i,notice), $(i,info) or $(i,debug). A $(b,*) can be used as a \
           wildcard in sections, i.e. $(b, node* -> debug). The rules are \
           matched left to right, therefore the leftmost rule is highest \
           priority ." ) ]

  let examples =
    [ `S "EXAMPLES";
      `I
        ( "$(b,Run in sandbox mode listening to RPC commands at localhost \
           port 8732)",
          "$(mname) run \
           --sandbox=src/proto_alpha/parameters/sandbox-parameters.json \
           --data-dir /custom/data/dir --rpc-addr localhost:8732" );
      `I ("$(b,Run a node that accepts network connections)", "$(mname) run")
    ]

  let man =
    description @ Node_shared_arg.Manpage.args @ debug @ examples
    @ Node_shared_arg.Manpage.bugs

  let info = Cmdliner.Term.info ~doc:"Run the Tezos node" ~man "run"
end

let cmd = (Term.term, Manpage.info)

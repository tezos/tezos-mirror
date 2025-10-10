(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021-2024 Nomadic Labs <contact@nomadic-labs.com>           *)
(*                                                                           *)
(*****************************************************************************)

open Cli_arg

type history_mode = Archive | Full of int option | Rolling of int option

let default_full = Full None

let default_rolling = Rolling None

type media_type = Json | Binary | Any

let string_of_media_type = function
  | Any -> "any"
  | Binary -> "binary"
  | Json -> "json"

type tls_config = {certificate_path : string; key_path : string}

type argument =
  | Network of string
  | History_mode of history_mode
  | Expected_pow of int
  | Singleprocess
  | Bootstrap_threshold of int
  | Synchronisation_threshold of int
  | Sync_latency of int
  | Connections of int
  | Private_mode
  | Disable_p2p_maintenance
  | Disable_p2p_swap
  | Peer of string
  | No_bootstrap_peers
  | Media_type of media_type
  | Metadata_size_limit of int option
  | Metrics_addr of string
  | Cors_origin of string
  | Disable_mempool
  | Version
  | RPC_additional_addr of string
  | RPC_additional_addr_external of string
  | Max_active_rpc_connections of int
  | Enable_http_cache_headers
  | Disable_context_pruning
  | Storage_maintenance_delay of string
  | Force_history_mode_switch
  | Allow_yes_crypto

let make_argument = function
  | Network x -> ["--network"; x]
  | History_mode Archive -> ["--history-mode"; "archive"]
  | History_mode (Full None) -> ["--history-mode"; "full"]
  | History_mode (Full (Some i)) ->
      ["--history-mode"; "full:" ^ string_of_int i]
  | History_mode (Rolling None) -> ["--history-mode"; "rolling"]
  | History_mode (Rolling (Some i)) ->
      ["--history-mode"; "rolling:" ^ string_of_int i]
  | Expected_pow x -> ["--expected-pow"; string_of_int x]
  | Singleprocess -> ["--singleprocess"]
  | Bootstrap_threshold x -> ["--bootstrap-threshold"; string_of_int x]
  | Synchronisation_threshold x ->
      ["--synchronisation-threshold"; string_of_int x]
  | Sync_latency x -> ["--sync-latency"; string_of_int x]
  | Connections x -> ["--connections"; string_of_int x]
  | Private_mode -> ["--private-mode"]
  | Disable_p2p_maintenance -> ["--disable-p2p-maintenance"]
  | Disable_p2p_swap -> ["--disable-p2p-swap"]
  | Peer x -> ["--peer"; x]
  | No_bootstrap_peers -> ["--no-bootstrap-peers"]
  | Media_type media_type -> ["--media-type"; string_of_media_type media_type]
  | Metadata_size_limit None -> ["--metadata-size-limit"; "unlimited"]
  | Metadata_size_limit (Some i) -> ["--metadata-size-limit"; string_of_int i]
  | Metrics_addr metrics_addr -> ["--metrics-addr"; metrics_addr]
  | Cors_origin cors_origin -> ["--cors-origin"; cors_origin]
  | Disable_mempool -> ["--disable-mempool"]
  | Version -> ["--version"]
  | RPC_additional_addr addr -> ["--rpc-addr"; addr]
  | RPC_additional_addr_external addr -> ["--external-rpc-addr"; addr]
  | Max_active_rpc_connections n ->
      ["--max-active-rpc-connections"; string_of_int n]
  | Enable_http_cache_headers -> ["--enable-http-cache-headers"]
  | Disable_context_pruning -> ["--disable-context-pruning"]
  | Storage_maintenance_delay x -> ["--storage-maintenance-delay"; x]
  | Force_history_mode_switch -> ["--force-history-mode-switch"]
  | Allow_yes_crypto -> ["--allow-yes-crypto"]

let make_arguments arguments = List.flatten (List.map make_argument arguments)

(** [true] if the two given arguments are the same type
    and cannot be repeated. [false] otherwise.
 *)
let is_redundant = function
  | Network _, Network _
  | History_mode _, History_mode _
  | Expected_pow _, Expected_pow _
  | Singleprocess, Singleprocess
  | Bootstrap_threshold _, Bootstrap_threshold _
  | Synchronisation_threshold _, Synchronisation_threshold _
  | Sync_latency _, Sync_latency _
  | Connections _, Connections _
  | Private_mode, Private_mode
  | Disable_p2p_maintenance, Disable_p2p_maintenance
  | Disable_p2p_swap, Disable_p2p_swap
  | No_bootstrap_peers, No_bootstrap_peers
  | Media_type _, Media_type _
  | Metadata_size_limit _, Metadata_size_limit _
  | Version, Version
  | Max_active_rpc_connections _, Max_active_rpc_connections _
  | Enable_http_cache_headers, Enable_http_cache_headers
  | Disable_context_pruning, Disable_context_pruning
  | Storage_maintenance_delay _, Storage_maintenance_delay _
  | Force_history_mode_switch, Force_history_mode_switch
  | Allow_yes_crypto, Allow_yes_crypto ->
      true
  | Metrics_addr addr1, Metrics_addr addr2 -> addr1 = addr2
  | Peer peer1, Peer peer2 -> peer1 = peer2
  | Network _, _
  | History_mode _, _
  | Expected_pow _, _
  | Singleprocess, _
  | Bootstrap_threshold _, _
  | Synchronisation_threshold _, _
  | Sync_latency _, _
  | Connections _, _
  | Private_mode, _
  | Disable_p2p_maintenance, _
  | Disable_p2p_swap, _
  | No_bootstrap_peers, _
  | Media_type _, _
  | Metadata_size_limit _, _
  | Peer _, _
  | Metrics_addr _, _
  | Cors_origin _, _
  | Disable_mempool, _
  | RPC_additional_addr _, _
  | RPC_additional_addr_external _, _
  | Version, _
  | Max_active_rpc_connections _, _
  | Enable_http_cache_headers, _
  | Disable_context_pruning, _
  | Storage_maintenance_delay _, _
  | Force_history_mode_switch, _
  | Allow_yes_crypto, _ ->
      false

(* Some arguments should not be written in the config file by [Node.init]
   because [Node.run] overwrites them or it does not exist in the configuration
   file of the node. *)
let should_be_runlike_argument = function
  (* The single process argument does not exist in the configuration file of
     the node. It is only known as a command-line option. *)
  | Singleprocess -> true
  (* More details are given in definition of type [argument]. *)
  | RPC_additional_addr _ -> true
  | _ -> false

type 'a known = Unknown | Known of 'a

module Parameters = struct
  type persistent_state = {
    data_dir : string;
    net_addr : string option;
    mutable net_port : int;
    advertised_net_port : int option;
    metrics_addr : string option;
    metrics_port : int;
    rpc_external : bool;
    rpc_host : string;
    rpc_port : int;
    rpc_tls : tls_config option;
    allow_all_rpc : bool;
    max_active_rpc_connections : int;
    default_expected_pow : int;
    mutable default_arguments : argument list;
    mutable arguments : argument list;
    mutable pending_ready : unit option Lwt.u list;
    mutable pending_level : (int * int option Lwt.u) list;
    mutable pending_identity : string option Lwt.u list;
    runner : Runner.t option;
  }

  type session_state = {
    mutable ready : bool;
    mutable level : int known;
    mutable identity : string known;
  }

  let base_default_name = "node"

  let default_colors = Log.Color.[|FG.cyan; FG.magenta; FG.yellow; FG.green|]
end

open Parameters
include Daemon.Make (Parameters)

let check_error ?exit_code ?msg node =
  match node.status with
  | Not_running ->
      Test.fail "node %s is not running, it has no stderr" (name node)
  | Running {process; _} -> Process.check_error ?exit_code ?msg process

let wait node =
  match node.status with
  | Not_running ->
      Test.fail
        "node %s is not running, cannot wait for it to terminate"
        (name node)
  | Running {process; _} -> Process.wait process

let name node = node.name

let net_port node = node.persistent_state.net_port

let advertised_net_port node = node.persistent_state.advertised_net_port

let rpc_scheme node =
  match node.persistent_state.rpc_tls with Some _ -> "https" | None -> "http"

let rpc_external node = node.persistent_state.rpc_external

let rpc_host node = node.persistent_state.rpc_host

let rpc_port node = node.persistent_state.rpc_port

let rpc_endpoint ?(local = false) node =
  let host =
    if local then Constant.default_host
    else Runner.address node.persistent_state.runner
  in
  sf "%s://%s:%d" (rpc_scheme node) host (rpc_port node)

let metrics_port node = node.persistent_state.metrics_port

let data_dir node = node.persistent_state.data_dir

let identity_file node = Filename.concat (data_dir node) "identity.json"

let runner node = node.persistent_state.runner

let pid node =
  match node.status with
  | Running status -> Process.pid status.process |> Option.some
  | Not_running -> None

let path node = node.path

let spawn_command ?env node =
  Process.spawn
    ?env
    ?runner:node.persistent_state.runner
    ~name:node.name
    ~color:node.color
    node.path

let spawn_identity_generate ?expected_pow node =
  spawn_command
    node
    [
      "identity";
      "generate";
      "--data-dir";
      node.persistent_state.data_dir;
      string_of_int
        (Option.value
           expected_pow
           ~default:node.persistent_state.default_expected_pow);
    ]

let identity_generate ?expected_pow node =
  spawn_identity_generate ?expected_pow node |> Process.check

let show_history_mode = function
  | Archive -> "archive"
  | Full None -> "full"
  | Full (Some i) -> "full_" ^ string_of_int i
  | Rolling None -> "rolling"
  | Rolling (Some i) -> "rolling_" ^ string_of_int i

let add_missing_argument arguments argument =
  if List.exists (fun arg -> is_redundant (arg, argument)) arguments then
    arguments
  else argument :: arguments

let add_default_arguments arguments =
  let arguments =
    (* Give a default value of "sandbox" to --network. *)
    add_missing_argument arguments (Network "sandbox")
  in
  (* Give a default value of 0 to --expected-pow. *)
  add_missing_argument arguments (Expected_pow 0)

let spawn_config_command command node arguments =
  let arguments =
    List.fold_left
      add_missing_argument
      arguments
      node.persistent_state.arguments
  in
  (* Since arguments will be in the configuration file, we will not need them after this. *)
  node.persistent_state.arguments <- [] ;
  spawn_command
    node
    ("config" :: command :: "--data-dir" :: node.persistent_state.data_dir
   :: make_arguments arguments)

let spawn_config_init = spawn_config_command "init"

let spawn_config_update = spawn_config_command "update"

let spawn_config_reset node arguments =
  node.persistent_state.arguments <- node.persistent_state.default_arguments ;
  spawn_config_command "reset" node arguments

let config_init node arguments =
  spawn_config_init node arguments |> Process.check

let config_update node arguments =
  spawn_config_update node arguments |> Process.check

let config_reset node arguments =
  spawn_config_reset node arguments |> Process.check

let config_show node =
  let* output =
    spawn_command
      node
      ["config"; "show"; "--data-dir"; node.persistent_state.data_dir]
    |> Process.check_and_read_stdout
  in
  return (JSON.parse ~origin:"config" output)

module Config_file = struct
  let filename node = sf "%s/config.json" @@ data_dir node

  let read node =
    match node.persistent_state.runner with
    | None -> Lwt.return (JSON.parse_file (filename node))
    | Some runner ->
        let* content =
          Process.spawn ~runner "cat" [filename node]
          |> Process.check_and_read_stdout
        in
        JSON.parse ~origin:"Node.config_file.read" content |> Lwt.return

  let write node config =
    match node.persistent_state.runner with
    | None -> Lwt.return (JSON.encode_to_file (filename node) config)
    | Some runner ->
        Helpers.write_file
          ~runner
          ~contents:(JSON.encode config)
          (filename node)

  let update node update =
    let* config = read node in
    let config = update config in
    write node config

  let set_prevalidator ?(operations_request_timeout = 10.)
      ?(max_refused_operations = 1000) ?(operations_batch_size = 50) old_config
      =
    let prevalidator =
      `O
        [
          ("operations_request_timeout", `Float operations_request_timeout);
          ( "max_refused_operations",
            `Float (float_of_int max_refused_operations) );
          ("operations_batch_size", `Float (float_of_int operations_batch_size));
        ]
      |> JSON.annotate ~origin:"set_prevalidator"
    in
    JSON.update
      "shell"
      (fun config -> JSON.put ("prevalidator", prevalidator) config)
      old_config

  let set_peer_validator ?(new_head_request_timeout = 60.) old_config =
    let peer_validator =
      `O
        [
          ( "peer_validator",
            `O [("new_head_request_timeout", `Float new_head_request_timeout)]
          );
        ]
      |> JSON.annotate ~origin:"set_peer_validator"
    in
    JSON.put ("shell", peer_validator) old_config

  let mk_genesis ~timestamp ~block ~protocol =
    `O
      [
        ("timestamp", `String timestamp);
        ("block", `String block);
        ("protocol", `String protocol);
      ]

  let mk_genesis_parameters ~genesis_pubkey =
    `O [("values", `O [("genesis_pubkey", `String genesis_pubkey)])]

  let sandbox_network_config : JSON.u =
    `O
      [
        ( "genesis",
          mk_genesis
            ~timestamp:"2018-06-30T16:07:32Z"
            ~block:"BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2"
            ~protocol:Protocol.genesis_hash );
        ( "genesis_parameters",
          mk_genesis_parameters ~genesis_pubkey:Constant.activator.public_key );
        ("chain_name", `String "TEZOS");
        ("sandboxed_chain_name", `String "SANDBOXED_TEZOS");
      ]

  let ghostnet_sandbox_network_config : JSON.u =
    `O
      [
        ( "genesis",
          mk_genesis
            ~timestamp:"2022-01-25T15:00:00Z"
            ~block:"BLockGenesisGenesisGenesisGenesisGenesis1db77eJNeJ9"
            ~protocol:Protocol.genesis_hash );
        ( "genesis_parameters",
          mk_genesis_parameters ~genesis_pubkey:Constant.activator.public_key );
        ("chain_name", `String "TEZOS");
        ("sandboxed_chain_name", `String "SANDBOXED_TEZOS");
      ]

  (* Copied from Octez_node_config.Config_file *)
  let ghostnet_network_config : JSON.u =
    `O
      [
        ( "genesis",
          mk_genesis
            ~timestamp:"2022-01-25T15:00:00Z"
            ~block:"BLockGenesisGenesisGenesisGenesisGenesis1db77eJNeJ9"
            ~protocol:"Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P" );
        ( "genesis_parameters",
          mk_genesis_parameters
            ~genesis_pubkey:
              "edpkuYLienS3Xdt5c1vfRX1ibMxQuvfM67ByhJ9nmRYYKGAAoTq1UC" );
        ("chain_name", `String "TEZOS_ITHACANET_2022-01-25T15:00:00Z");
        ("sandboxed_chain_name", `String "SANDBOXED_TEZOS");
      ]

  (* Copied from Octez_node_config.Config_file *)
  let shadownet_network_config : JSON.u =
    `O
      [
        ( "genesis",
          mk_genesis
            ~timestamp:"2025-08-07T20:00:00Z"
            ~block:"BMJTFWBgqGUzsvW4JLS1XuCYnpH18pBsLb5UvPCv2eiNC5vUeps"
            ~protocol:"Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P" );
        ( "genesis_parameters",
          mk_genesis_parameters
            ~genesis_pubkey:
              "edpktosVHk2f3Yrz9Jb6rMrk6uVy4sTxVhP2iyF39AdgzvsTWgbaLy" );
        ("chain_name", `String "TEZOS_SHADOWNET_2025-08-07T20:00:00Z");
        ("sandboxed_chain_name", `String "SANDBOXED_TEZOS");
      ]

  let seoulnet_network_config : JSON.u =
    `O
      [
        ( "genesis",
          mk_genesis
            ~timestamp:"2025-07-11T08:00:00Z"
            ~block:"BLQGRyv3v92oE9iM4BGWwWpy6NxcDFPb7NLsNeuRVr3TYidU7MC"
            ~protocol:"Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P" );
        ( "genesis_parameters",
          mk_genesis_parameters
            ~genesis_pubkey:
              "edpktosVHk2f3Yrz9Jb6rMrk6uVy4sTxVhP2iyF39AdgzvsTWgbaLy" );
        ("chain_name", `String "TEZOS_SEOULNET_2025-07-11T08:00:00Z");
        ("sandboxed_chain_name", `String "SANDBOXED_TEZOS");
      ]

  let tallinnnet_network_config : JSON.u =
    `O
      [
        ( "genesis",
          mk_genesis
            ~timestamp:"2025-11-18T21:00:00Z"
            ~block:"BMQuJ7YgLaLUiyiSWmGma5LwkEFBAmksDFmocyEUe59yVav7wfC"
            ~protocol:"Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P" );
        ( "genesis_parameters",
          mk_genesis_parameters
            ~genesis_pubkey:
              "edpktosVHk2f3Yrz9Jb6rMrk6uVy4sTxVhP2iyF39AdgzvsTWgbaLy" );
        ("chain_name", `String "TEZOS_TALLINNNET_2025-11-18T21:00:00Z");
        ("sandboxed_chain_name", `String "SANDBOXED_TEZOS");
      ]

  (* Copied from Octez_node_config.Config_file *)
  let mainnet_network_config : JSON.u =
    `O
      [
        ( "genesis",
          mk_genesis
            ~timestamp:"2018-06-30T16:07:32Z"
            ~block:"BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2"
            ~protocol:"Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P" );
        ("chain_name", `String "TEZOS_MAINNET");
        ("sandboxed_chain_name", `String "SANDBOXED_TEZOS_MAINNET");
      ]

  let put_user_activated_upgrades upgrade_points =
    JSON.put
      ( "user_activated_upgrades",
        JSON.annotate ~origin:"user_activated_upgrades"
        @@ `A
             (List.map
                (fun (level, protocol) ->
                  `O
                    [
                      ("level", `Float (float level));
                      ("replacement_protocol", `String (Protocol.hash protocol));
                    ])
                upgrade_points) )

  let set_sandbox_network_with_user_activated_upgrades upgrade_points old_config
      =
    let network =
      sandbox_network_config
      |> JSON.annotate
           ~origin:"set_sandbox_network_config_with_user_activated_upgrades"
      |> put_user_activated_upgrades upgrade_points
    in
    JSON.put ("network", network) old_config

  let update_network_with_user_activated_upgrades upgrade_points old_config =
    JSON.update
      "network"
      (fun config -> (put_user_activated_upgrades upgrade_points) config)
      old_config

  let set_sandbox_network_with_user_activated_overrides overrides old_config =
    let network =
      sandbox_network_config
      |> JSON.annotate
           ~origin:"set_sandbox_network_with_user_activated_overrides"
      |> JSON.put
           ( "user_activated_protocol_overrides",
             JSON.annotate ~origin:"user_activated_overrides"
             @@ `A
                  (List.map
                     (fun (replaced_protocol, replacement_protocol) ->
                       `O
                         [
                           ("replaced_protocol", `String replaced_protocol);
                           ("replacement_protocol", `String replacement_protocol);
                         ])
                     overrides) )
    in
    JSON.put ("network", network) old_config

  let set_network_with_dal_config
      (dal_config : Tezos_crypto_dal.Cryptobox.Config.t) old_config =
    let dal_config_json =
      JSON.annotate
        ~origin:"dal_initialisation"
        (`O
           [
             ("activated", `Bool dal_config.activated);
             ( "bootstrap_peers",
               `A
                 (List.map
                    (fun peer -> `String peer)
                    dal_config.bootstrap_peers) );
           ])
    in
    let network =
      JSON.unannotate JSON.(old_config |-> "network")
      |> JSON.annotate ~origin:"set_network_with_dal_config"
      |> JSON.put ("dal_config", dal_config_json)
    in
    JSON.put ("network", network) old_config

  let set_sandbox_network old_config =
    JSON.put
      ( "network",
        JSON.annotate ~origin:"set_sandbox_network" sandbox_network_config )
      old_config

  let set_network ?user_activated_upgrades (origin, network_config) old_config =
    let may_patch_user_activated_upgrades =
      match user_activated_upgrades with
      | None -> Fun.id
      | Some upgrade_points -> put_user_activated_upgrades upgrade_points
    in
    JSON.put
      ( "network",
        JSON.annotate ~origin network_config
        |> may_patch_user_activated_upgrades )
      old_config

  let set_mainnet_network ?user_activated_upgrades () =
    set_network
      ?user_activated_upgrades
      ("set_mainnet_network", mainnet_network_config)

  let set_ghostnet_network ?user_activated_upgrades () =
    set_network
      ?user_activated_upgrades
      ("set_ghostnet_network", ghostnet_network_config)

  let set_ghostnet_sandbox_network ?user_activated_upgrades () =
    set_network
      ?user_activated_upgrades
      ("set_ghostnet_sandbox_network", ghostnet_sandbox_network_config)

  let set_seoulnet_network ?user_activated_upgrades () =
    set_network
      ?user_activated_upgrades
      ("set_seoulnet_network", seoulnet_network_config)

  let set_tallinnnet_network ?user_activated_upgrades () =
    set_network
      ?user_activated_upgrades
      ("set_tallinnnet_network", tallinnnet_network_config)

  let set_shadownet_network ?user_activated_upgrades () =
    set_network
      ?user_activated_upgrades
      ("set_shadownet_network", shadownet_network_config)
end

type snapshot_history_mode = Rolling_history | Full_history

type export_format = Tar | Raw

let spawn_snapshot_export ?(history_mode = Full_history) ?export_level
    ?(export_format = Tar) node file =
  spawn_command
    node
    (["snapshot"; "export"; "--data-dir"; node.persistent_state.data_dir]
    @ (match history_mode with
      | Full_history -> []
      | Rolling_history -> ["--rolling"])
    @ optional_arg "block" string_of_int export_level
    @ (["--export-format"]
      @ match export_format with Tar -> ["tar"] | Raw -> ["raw"])
    @ [file])

let snapshot_export ?history_mode ?export_level ?export_format node file =
  spawn_snapshot_export ?history_mode ?export_level ?export_format node file
  |> Process.check

let spawn_snapshot_info ?(json = false) node file =
  spawn_command
    node
    (["snapshot"; "info"] @ (if json then ["--json"] else []) @ [file])

let snapshot_info ?json node file =
  spawn_snapshot_info ?json node file |> Process.check_and_read_stdout

let spawn_snapshot_import ?env ?(force = false) ?(no_check = false)
    ?(reconstruct = false) node file =
  spawn_command
    ?env
    node
    (["snapshot"; "import"; "--data-dir"; node.persistent_state.data_dir]
    @ (if reconstruct then ["--reconstruct"] else [])
    @ (if no_check then ["--no-check"] else [])
    @ (if force then ["--force"] else [])
    @ [file])

let snapshot_import ?env ?force ?no_check ?reconstruct node file =
  spawn_snapshot_import ?env ?force ?no_check ?reconstruct node file
  |> Process.check

let spawn_reconstruct node =
  spawn_command
    node
    ["reconstruct"; "--data-dir"; node.persistent_state.data_dir]

let reconstruct node = spawn_reconstruct node |> Process.check

let trigger_ready node value =
  let pending = node.persistent_state.pending_ready in
  node.persistent_state.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let set_ready node =
  (match node.status with
  | Not_running -> ()
  | Running status -> status.session_state.ready <- true) ;
  trigger_ready node (Some ())

let update_level node current_level =
  (match node.status with
  | Not_running -> ()
  | Running status -> (
      match status.session_state.level with
      | Unknown -> status.session_state.level <- Known current_level
      | Known old_level ->
          status.session_state.level <- Known (max old_level current_level))) ;
  let pending = node.persistent_state.pending_level in
  node.persistent_state.pending_level <- [] ;
  List.iter
    (fun ((level, resolver) as pending) ->
      if current_level >= level then
        Lwt.wakeup_later resolver (Some current_level)
      else
        node.persistent_state.pending_level <-
          pending :: node.persistent_state.pending_level)
    pending

let update_identity node identity =
  match node.status with
  | Not_running -> ()
  | Running status ->
      (match status.session_state.identity with
      | Unknown -> status.session_state.identity <- Known identity
      | Known identity' ->
          if identity' <> identity then Test.fail "node identity changed") ;
      let pending = node.persistent_state.pending_identity in
      node.persistent_state.pending_identity <- [] ;
      List.iter
        (fun resolver -> Lwt.wakeup_later resolver (Some identity))
        pending

let handle_event node {name; value; timestamp = _} =
  match name with
  | "node_is_ready.v0" -> set_ready node
  | "head_increment.v0" | "branch_switch.v0" -> (
      if
        (* Consider [head_increment] and [branch_switch] events only
           with the local RPC server. *)
        not node.persistent_state.rpc_external
      then
        match JSON.(value |-> "level" |> as_int_opt) with
        | None ->
            (* Names [head_increment] and [branch_switch] correspond to
               multiple different events. Some of those events carry a
               [level], some do not. *)
            ()
        | Some level -> update_level node level)
  | "store_synchronized_on_head.v0" -> (
      if
        (* Consider [store_synchronized_on_head] event only with the
           external RPC server. *)
        node.persistent_state.rpc_external
      then
        match JSON.(value |-> "level" |> as_int_opt) with
        | None ->
            (* Names [head_increment] and [branch_switch] correspond to
               multiple different events. Some of those events carry a
               [level], some do not. *)
            ()
        | Some level -> update_level node level)
  | "read_identity.v0" -> update_identity node (JSON.as_string value)
  | "compilation_error.v0" -> (
      match JSON.as_string_opt value with
      | Some fname ->
          if Sys.file_exists fname then (
            let content = read_file fname in
            Log.error "Protocol compilation failed:" ;
            Log.error "%s" (String.trim content))
          else
            Log.error
              "Protocol compilation failed but log file %S was not found"
              fname
      | None ->
          Log.error "Protocol compilation failed but cannot read the payload")
  | "set_head.v0" -> (
      match JSON.(value |> geti 1 |> as_int_opt) with
      | None -> ()
      | Some level ->
          if not node.persistent_state.rpc_external then update_level node level
      )
  | _ -> ()

let check_event ?where node name promise =
  let* result = promise in
  match result with
  | None ->
      raise (Terminated_before_event {daemon = node.name; event = name; where})
  | Some x -> return x

let wait_for_synchronisation ~statuses node =
  let filter json =
    let status = JSON.as_string json in
    Log.info "%s: %s" (name node) status ;
    if List.exists (fun st -> st =~ rex status) statuses then Some () else None
  in
  wait_for node "synchronisation_status.v0" filter

let wait_for_ready node =
  match node.status with
  | Running {session_state = {ready = true; _}; _} -> unit
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      let promise, resolver = Lwt.task () in
      node.persistent_state.pending_ready <-
        resolver :: node.persistent_state.pending_ready ;
      check_event node "node_is_ready.v0" promise

let wait_for_level node level =
  match node.status with
  | Running {session_state = {level = Known current_level; _}; _}
    when current_level >= level ->
      return current_level
  | Not_running | Running _ ->
      let promise, resolver = Lwt.task () in
      node.persistent_state.pending_level <-
        (level, resolver) :: node.persistent_state.pending_level ;
      let event_name =
        if node.persistent_state.rpc_external then
          "store_synchronized_on_head.v0"
        else "head_increment.v0 / branch_switch.v0"
      in
      check_event
        node
        event_name
        ~where:("level >= " ^ string_of_int level)
        promise

let get_last_seen_level node =
  match node.status with
  | Running {session_state = {level = Known level; _}; _} -> level
  | Not_running | Running _ -> 0

let get_level node = wait_for_level node 0

let wait_for_identity node =
  match node.status with
  | Running {session_state = {identity = Known identity; _}; _} ->
      return identity
  | Not_running | Running _ ->
      let promise, resolver = Lwt.task () in
      node.persistent_state.pending_identity <-
        resolver :: node.persistent_state.pending_identity ;
      check_event node "read_identity.v0" promise

let wait_for_request ~request node =
  let event_name =
    match request with
    | `Inject -> "request_completed_info.v0"
    | `Flush -> "request_completed_info.v0"
    | `Notify | `Arrived -> "request_completed_debug.v0"
  in
  let request_str =
    match request with
    | `Flush -> "flush"
    | `Inject -> "inject"
    | `Notify -> "notify"
    | `Arrived -> "arrived"
  in
  let filter json =
    match JSON.(json |-> "view" |-> "request" |> as_string_opt) with
    | Some s when String.equal s request_str -> Some ()
    | Some _ | None -> None
  in
  wait_for node event_name filter

let wait_for_connections node connections =
  let counter = ref 0 in
  let waiter, resolver = Lwt.task () in
  on_event node (fun {name; _} ->
      match name with
      | "connection.v0" ->
          incr counter ;
          if !counter = connections then Lwt.wakeup resolver ()
      | _ -> ()) ;
  let* () = wait_for_ready node in
  waiter

let wait_for_disconnections node disconnections =
  let counter = ref 0 in
  let waiter, resolver = Lwt.task () in
  on_event node (fun {name; _} ->
      match name with
      | "disconnection.v0" ->
          incr counter ;
          if !counter = disconnections then Lwt.wakeup resolver ()
      | _ -> ()) ;
  let* () = wait_for_ready node in
  waiter

let wait_for_branch_switch ?level ?hash node =
  wait_for
    node
    "branch_switch.v0"
    JSON.(
      fun json ->
        let level' = json |-> "level" |> as_int in
        let hash' = json |-> "view" |-> "hash" |> as_string in
        if
          Option.fold ~none:true ~some:(Int.equal level') level
          && Option.fold ~none:true ~some:(String.equal hash') hash
        then Some (level', hash')
        else None)

let enable_external_rpc_process =
  match Sys.getenv_opt "TZ_SCHEDULE_KIND" with
  | Some "EXTENDED_RPC_TESTS" -> true
  | _ -> false

let enable_singleprocess =
  match Sys.getenv_opt "TZ_SCHEDULE_KIND" with
  | Some "EXTENDED_VALIDATION_TESTS" -> true
  | _ -> false

let create ?runner ?(path = Uses.path Constant.octez_node) ?name ?color
    ?data_dir ?event_pipe ?net_addr ?net_port ?advertised_net_port ?metrics_addr
    ?metrics_port ?(rpc_external = enable_external_rpc_process)
    ?(rpc_host = Constant.default_host) ?rpc_port ?rpc_tls
    ?(allow_all_rpc = true) ?(max_active_rpc_connections = 500) arguments =
  let name = match name with None -> fresh_name () | Some name -> name in
  let data_dir =
    match data_dir with None -> Temp.dir ?runner name | Some dir -> dir
  in
  let net_port =
    match net_port with None -> Port.fresh () | Some port -> port
  in
  let rpc_port =
    match rpc_port with None -> Port.fresh () | Some port -> port
  in
  let metrics_port =
    match metrics_port with None -> Port.fresh () | Some port -> port
  in
  let arguments = add_default_arguments arguments in
  let default_expected_pow =
    list_find_map (function Expected_pow x -> Some x | _ -> None) arguments
    |> Option.value ~default:0
  in
  let node =
    create
      ?runner
      ~path
      ~name
      ?color
      ?event_pipe
      {
        data_dir;
        net_addr;
        net_port;
        advertised_net_port;
        rpc_external;
        rpc_host;
        rpc_port;
        rpc_tls;
        metrics_addr;
        metrics_port;
        max_active_rpc_connections;
        allow_all_rpc;
        default_arguments = arguments;
        arguments;
        default_expected_pow;
        runner;
        pending_ready = [];
        pending_level = [];
        pending_identity = [];
      }
  in
  on_event node (handle_event node) ;
  node

let add_argument node argument =
  node.persistent_state.arguments <- argument :: node.persistent_state.arguments

let add_peer node peer =
  let address =
    Runner.address
      ?from:node.persistent_state.runner
      peer.persistent_state.runner
    ^ ":"
  in
  add_argument node (Peer (address ^ string_of_int (net_port peer)))

let point ?from node =
  let from =
    match from with None -> None | Some peer -> peer.persistent_state.runner
  in
  let address = Runner.address ?from node.persistent_state.runner in
  (address, net_port node)

let point_str ?from node =
  let addr, port = point ?from node in
  addr ^ ":" ^ Int.to_string port

let point_and_id ?from node =
  let point = point_str ?from node in
  let* id = wait_for_identity node in
  Lwt.return (point ^ "#" ^ id)

let add_peer_with_id node peer =
  let* peer = point_and_id ~from:node peer in
  add_argument node (Peer peer) ;
  Lwt.return_unit

let get_peers node =
  List.filter_map
    (fun arg -> match arg with Peer s -> Some s | _ -> None)
    node.persistent_state.arguments

let remove_peers_json_file node =
  let filename = sf "%s/peers.json" (data_dir node) in
  Log.info "Removing file %s" filename ;
  Sys.remove filename

(** [runlike_command_arguments node command arguments]
    evaluates in a list of strings containing all command
    line arguments needed to spawn a [command] like [run]
    or [replay] for the given [node] and extra [arguments]. *)
let runlike_command_arguments node command arguments =
  let net_addr, rpc_addr, metrics_addr =
    match node.persistent_state.runner with
    | None ->
        ( Option.value
            ~default:Constant.default_host
            node.persistent_state.net_addr
          ^ ":",
          node.persistent_state.rpc_host ^ ":",
          Option.value
            ~default:Constant.default_host
            node.persistent_state.metrics_addr
          ^ ":" )
    | Some _ ->
        let any_addr = Unix.(string_of_inet_addr inet_addr_any) ^ ":" in
        (* FIXME spawn an ssh tunnel in case of remote host *)
        (any_addr, any_addr, any_addr)
  in
  let arguments =
    List.fold_left
      add_missing_argument
      arguments
      node.persistent_state.arguments
  in
  let command_args = make_arguments arguments in
  let command_args =
    match node.persistent_state.advertised_net_port with
    | None -> command_args
    | Some port -> "--advertised-net-port" :: string_of_int port :: command_args
  in
  let command_args =
    if node.persistent_state.allow_all_rpc then
      "--allow-all-rpc"
      :: (rpc_addr ^ string_of_int node.persistent_state.rpc_port)
      :: command_args
    else command_args
  in
  let command_args =
    match node.persistent_state.rpc_tls with
    | None -> command_args
    | Some {certificate_path; key_path} ->
        "--rpc-tls" :: (certificate_path ^ "," ^ key_path) :: command_args
  in
  command :: "--data-dir" :: node.persistent_state.data_dir :: "--net-addr"
  :: (net_addr ^ string_of_int node.persistent_state.net_port)
  :: "--metrics-addr"
  :: (metrics_addr ^ string_of_int node.persistent_state.metrics_port)
  :: (if node.persistent_state.rpc_external then "--external-rpc-addr"
      else "--rpc-addr")
  :: (rpc_addr ^ string_of_int node.persistent_state.rpc_port)
  :: "--max-active-rpc-connections"
  :: string_of_int node.persistent_state.max_active_rpc_connections
  :: command_args

let do_runlike_command ?env ?(on_terminate = fun _ -> ()) ?event_level
    ?event_sections_levels node arguments =
  (match node.status with
  | Not_running -> ()
  | Running _ -> Test.fail "node %s is already running" node.name) ;
  let on_terminate status =
    on_terminate status ;
    (* Cancel all [Ready] event listeners. *)
    trigger_ready node None ;
    (* Cancel all [Level_at_least] event listeners. *)
    let pending = node.persistent_state.pending_level in
    node.persistent_state.pending_level <- [] ;
    List.iter (fun (_, pending) -> Lwt.wakeup_later pending None) pending ;
    (* Cancel all [Read_identity] event listeners. *)
    let pending = node.persistent_state.pending_identity in
    node.persistent_state.pending_identity <- [] ;
    List.iter (fun pending -> Lwt.wakeup_later pending None) pending ;
    unit
  in
  run
    ?env
    ?runner:node.persistent_state.runner
    ?event_level
    ?event_sections_levels
    node
    {ready = false; level = Unknown; identity = Unknown}
    arguments
    ~on_terminate

let run ?env ?patch_config ?on_terminate ?event_level ?event_sections_levels
    node arguments =
  let* () =
    match patch_config with
    | None -> Lwt.return_unit
    | Some patch -> Config_file.update node patch
  in
  (* It is necessary to check the [enable_singleprocess] flag to
     ensure that the EXTENDED_VALIDATION_TESTS pipeline will
     effectively start all nodes with the `--singleprocess`
     flag. Indeed, as this option is not set in the config file (it is
     just a command line argument), if one starts a node with
     `Node.init`, terminates it, an then calls `Node.run`, the
     [Singleprocess] will not be activated anymore -- see the
     [Node.init] code and documentation. Thanks to this, we always
     enable it if it is required.*)
  let arguments =
    let args =
      if enable_singleprocess then
        (* Avoids to repeat the option if already enabled *)
        if List.mem Singleprocess arguments then arguments
        else arguments @ [Singleprocess]
      else arguments
    in
    runlike_command_arguments node "run" args
  in
  do_runlike_command
    ?env
    ?on_terminate
    ?event_level
    ?event_sections_levels
    node
    arguments

let replay ?on_terminate ?event_level ?event_sections_levels ?(strict = false)
    ?(blocks = ["head"]) node =
  (* Select the appropriated arguments as the replay command does not
     support all the node default arguments. *)
  let strict = if strict then ["--strict"] else [] in
  let directory = ["--data-dir"; node.persistent_state.data_dir] in
  let arguments = ["replay"] @ directory @ strict @ blocks in
  do_runlike_command
    ?on_terminate
    ?event_level
    ?event_sections_levels
    node
    arguments

let init ?runner ?path ?name ?env ?color ?data_dir ?event_pipe ?net_addr
    ?net_port ?advertised_net_port ?metrics_addr ?metrics_port ?rpc_external
    ?rpc_host ?rpc_port ?rpc_tls ?event_level ?event_sections_levels
    ?patch_config ?snapshot arguments =
  let run_arguments, config_arguments =
    List.partition should_be_runlike_argument arguments
  in
  let node =
    create
      ?runner
      ?path
      ?name
      ?color
      ?data_dir
      ?event_pipe
      ?net_addr
      ?net_port
      ?advertised_net_port
      ?metrics_addr
      ?metrics_port
      ?rpc_external
      ?rpc_host
      ?rpc_port
      ?rpc_tls
      config_arguments
  in
  let* () = identity_generate node in
  let* () = config_init node [] in
  (* We leave RPC port arguments in because we want them to be used by [run] if
     the node is restarted. It does mean that [config_init] will write them
     too, which can be confusing since it has no effect. This is a compromise.

     We filter out [Singleprocess] to prevent an error if a config command is
     called on [node]. This argument will not be used if the node is restarted.
  *)
  let singleprocess_arg, run_arguments =
    List.partition (function Singleprocess -> true | _ -> false) run_arguments
  in
  node.persistent_state.arguments <- run_arguments ;
  let* () =
    match snapshot with
    | Some (file, reconstruct) -> snapshot_import ~reconstruct node file
    | None -> unit
  in
  let* () =
    run
      ?env
      ?patch_config
      ?event_level
      ?event_sections_levels
      node
      singleprocess_arg
  in
  let* () = wait_for_ready node in
  return node

let send_raw_data node ~data =
  (* Extracted from Lwt_utils_unix. *)
  let write_string ?(pos = 0) ?len descr buf =
    let len = match len with None -> String.length buf - pos | Some l -> l in
    let rec inner pos len =
      if len = 0 then Lwt.return_unit
      else
        Lwt.bind (Lwt_unix.write_string descr buf pos len) (function
          | 0 ->
              Lwt.fail End_of_file
              (* other endpoint cleanly closed its connection *)
          | nb_written -> inner (pos + nb_written) (len - nb_written))
    in
    inner pos len
  in
  Log.debug "Write raw data to node %s" node.name ;
  let socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  Lwt_unix.set_close_on_exec socket ;
  let uaddr = Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, net_port node) in
  let* () = Lwt_unix.connect socket uaddr in
  write_string socket data

let upgrade_storage node =
  spawn_command
    node
    ["upgrade"; "storage"; "--data-dir"; node.persistent_state.data_dir]
  |> Process.check

let get_version node =
  let version_flag = make_argument Version in
  let* output =
    spawn_command node version_flag |> Process.check_and_read_stdout
  in
  return @@ String.trim output

let as_rpc_endpoint ?(local = false) (t : t) =
  let state = t.persistent_state in
  let scheme = if Option.is_some state.rpc_tls then "https" else "http" in
  let host =
    if local || Option.is_none t.persistent_state.runner then state.rpc_host
    else Runner.address t.persistent_state.runner
  in
  Endpoint.make ~scheme ~host ~port:state.rpc_port ()

module RPC = struct
  module RPC_callers : RPC_core.CALLERS with type uri_provider := t = struct
    let call ?rpc_hooks ?log_request ?log_response_status ?log_response_body
        node rpc =
      RPC_core.call
        ?rpc_hooks
        ?log_request
        ?log_response_status
        ?log_response_body
        (as_rpc_endpoint node)
        rpc

    let call_raw ?rpc_hooks ?log_request ?log_response_status ?log_response_body
        ?extra_headers node rpc =
      RPC_core.call_raw
        ?rpc_hooks
        ?log_request
        ?log_response_status
        ?log_response_body
        ?extra_headers
        (as_rpc_endpoint node)
        rpc

    let call_json ?rpc_hooks ?log_request ?log_response_status
        ?log_response_body node rpc =
      RPC_core.call_json
        ?rpc_hooks
        ?log_request
        ?log_response_status
        ?log_response_body
        (as_rpc_endpoint node)
        rpc
  end

  include RPC_callers
  include RPC
end

let current_cycle node =
  let* level = RPC.call node @@ RPC.get_chain_block_helper_current_level () in
  return level.cycle

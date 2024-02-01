(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Cli_arg

type history_mode = Archive | Full of int option | Rolling of int option

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
  | RPC_additional_addr_local of string
  | Max_active_rpc_connections of int

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
  | RPC_additional_addr_local addr -> ["--local-rpc-addr"; addr]
  | Max_active_rpc_connections n ->
      ["--max-active-rpc-connections"; string_of_int n]

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
  | Max_active_rpc_connections _, Max_active_rpc_connections _ ->
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
  | RPC_additional_addr_local _, _
  | Version, _
  | Max_active_rpc_connections _, _ ->
      false

type 'a known = Unknown | Known of 'a

module Parameters = struct
  type persistent_state = {
    data_dir : string;
    net_addr : string option;
    mutable net_port : int;
    advertised_net_port : int option;
    metrics_addr : string option;
    metrics_port : int;
    rpc_local : bool;
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

let rpc_local node = node.persistent_state.rpc_local

let rpc_host node = node.persistent_state.rpc_host

let rpc_port node = node.persistent_state.rpc_port

let rpc_endpoint node =
  sf "%s://%s:%d" (rpc_scheme node) (rpc_host node) (rpc_port node)

let data_dir node = node.persistent_state.data_dir

let runner node = node.persistent_state.runner

let spawn_command node =
  Process.spawn
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

  let read node = JSON.parse_file (filename node)

  let write node config = JSON.encode_to_file (filename node) config

  let update node update = read node |> update |> write node

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

  let sandbox_network_config =
    `O
      [
        ( "genesis",
          `O
            [
              ("timestamp", `String "2018-06-30T16:07:32Z");
              ( "block",
                `String "BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2" );
              ("protocol", `String Protocol.genesis_hash);
            ] );
        ( "genesis_parameters",
          `O
            [
              ( "values",
                `O [("genesis_pubkey", `String Constant.activator.public_key)]
              );
            ] );
        ("chain_name", `String "TEZOS");
        ("sandboxed_chain_name", `String "SANDBOXED_TEZOS");
      ]

  let ghostnet_sandbox_network_config =
    `O
      [
        ( "genesis",
          `O
            [
              ("timestamp", `String "2022-01-25T15:00:00Z");
              ( "block",
                `String "BLockGenesisGenesisGenesisGenesisGenesis1db77eJNeJ9" );
              ("protocol", `String Protocol.genesis_hash);
            ] );
        ( "genesis_parameters",
          `O
            [
              ( "values",
                `O [("genesis_pubkey", `String Constant.activator.public_key)]
              );
            ] );
        ("chain_name", `String "TEZOS");
        ("sandboxed_chain_name", `String "SANDBOXED_TEZOS");
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

  let set_sandbox_network_with_dal_config
      (dal_config : Tezos_crypto_dal.Cryptobox.Config.t) old_config =
    let dal_config_json =
      let parameters =
        match dal_config.use_mock_srs_for_testing with
        | Some parameters ->
            `O
              [
                ("slot_size", `Float (float_of_int parameters.slot_size));
                ("page_size", `Float (float_of_int parameters.page_size));
                ( "redundancy_factor",
                  `Float (float_of_int parameters.redundancy_factor) );
                ( "number_of_shards",
                  `Float (float_of_int parameters.number_of_shards) );
              ]
        | None -> `Null
      in
      JSON.annotate
        ~origin:"dal_initialisation"
        (`O
          [
            ("activated", `Bool dal_config.activated);
            ("use_mock_srs_for_testing", parameters);
            ( "bootstrap_peers",
              `A
                (List.map (fun peer -> `String peer) dal_config.bootstrap_peers)
            );
          ])
    in
    let network =
      sandbox_network_config
      |> JSON.annotate ~origin:"set_sandbox_network_with_dal_config"
      |> JSON.put ("dal_config", dal_config_json)
    in
    JSON.put ("network", network) old_config

  let set_ghostnet_sandbox_network ?user_activated_upgrades () old_config =
    let may_patch_user_activated_upgrades =
      match user_activated_upgrades with
      | None -> Fun.id
      | Some upgrade_points -> put_user_activated_upgrades upgrade_points
    in
    JSON.put
      ( "network",
        JSON.annotate
          ~origin:"set_ghostnet_sandbox_network"
          ghostnet_sandbox_network_config
        |> may_patch_user_activated_upgrades )
      old_config
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
  spawn_snapshot_info ?json node file |> Process.check

let spawn_snapshot_import ?(no_check = false) ?(reconstruct = false) node file =
  spawn_command
    node
    (["snapshot"; "import"; "--data-dir"; node.persistent_state.data_dir]
    @ (if reconstruct then ["--reconstruct"] else [])
    @ (if no_check then ["--no-check"] else [])
    @ [file])

let snapshot_import ?no_check ?reconstruct node file =
  spawn_snapshot_import ?no_check ?reconstruct node file |> Process.check

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
      match JSON.(value |-> "level" |> as_int_opt) with
      | None ->
          (* There are several kinds of events and maybe
             this one is not the one with the level: ignore it. *)
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
      | Some level -> update_level node level)
  | _ -> ()

let check_event ?where node name promise =
  let* result = promise in
  match result with
  | None ->
      raise (Terminated_before_event {daemon = node.name; event = name; where})
  | Some x -> return x

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
      check_event
        node
        "head_increment.v0 / branch_switch.v0"
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

let create ?runner ?(path = Constant.octez_node) ?name ?color ?data_dir
    ?event_pipe ?net_addr ?net_port ?advertised_net_port ?metrics_addr
    ?metrics_port ?(rpc_local = false) ?(rpc_host = "localhost") ?rpc_port
    ?rpc_tls ?(allow_all_rpc = true) ?(max_active_rpc_connections = 500)
    arguments =
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
        rpc_local;
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
        ( Option.value ~default:"127.0.0.1" node.persistent_state.net_addr ^ ":",
          node.persistent_state.rpc_host ^ ":",
          Option.value ~default:"127.0.0.1" node.persistent_state.metrics_addr
          ^ ":" )
    | Some _ ->
        (* FIXME spawn an ssh tunnel in case of remote host *)
        ("0.0.0.0:", "0.0.0.0:", "0.0.0.0:")
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
  :: (if node.persistent_state.rpc_local then "--local-rpc-addr"
     else "--rpc-addr")
  :: (rpc_addr ^ string_of_int node.persistent_state.rpc_port)
  :: "--max-active-rpc-connections"
  :: string_of_int node.persistent_state.max_active_rpc_connections
  :: command_args

let do_runlike_command ?(on_terminate = fun _ -> ()) ?event_level
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
    ?runner:node.persistent_state.runner
    ?event_level
    ?event_sections_levels
    node
    {ready = false; level = Unknown; identity = Unknown}
    arguments
    ~on_terminate

let run ?patch_config ?on_terminate ?event_level ?event_sections_levels node
    arguments =
  let () =
    match patch_config with
    | None -> ()
    | Some patch -> Config_file.update node patch
  in
  let arguments = runlike_command_arguments node "run" arguments in
  do_runlike_command
    ?on_terminate
    ?event_level
    ?event_sections_levels
    node
    arguments

let replay ?on_terminate ?event_level ?event_sections_levels ?(strict = false)
    ?(blocks = ["head"]) node arguments =
  let strict = if strict then ["--strict"] else [] in
  let arguments =
    runlike_command_arguments node "replay" arguments @ strict @ blocks
  in
  do_runlike_command
    ?on_terminate
    ?event_level
    ?event_sections_levels
    node
    arguments

let init ?runner ?path ?name ?color ?data_dir ?event_pipe ?net_port
    ?advertised_net_port ?metrics_addr ?metrics_port ?rpc_local ?rpc_host
    ?rpc_port ?rpc_tls ?event_level ?event_sections_levels ?patch_config
    ?snapshot arguments =
  (* The single process argument does not exist in the configuration
     file of the node. It is only known as a command-line option. As a
     consequence, we filter Singleprocess from the list of arguments
     passed to create, and we readd it if necessary when calling
     run. *)
  let single_process = List.mem Singleprocess arguments in
  let node =
    create
      ?runner
      ?path
      ?name
      ?color
      ?data_dir
      ?event_pipe
      ?net_port
      ?advertised_net_port
      ?metrics_addr
      ?metrics_port
      ?rpc_local
      ?rpc_host
      ?rpc_port
      ?rpc_tls
      (List.filter (fun x -> x <> Singleprocess) arguments)
  in
  let* () = identity_generate node in
  let* () = config_init node [] in
  let* () =
    match snapshot with
    | Some (file, reconstruct) -> snapshot_import ~reconstruct node file
    | None -> unit
  in
  let argument = if single_process then [Singleprocess] else [] in
  let* () =
    run ?patch_config ?event_level ?event_sections_levels node argument
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

let as_rpc_endpoint (t : t) =
  let state = t.persistent_state in
  let scheme = if Option.is_some state.rpc_tls then "https" else "http" in
  Endpoint.{scheme; host = state.rpc_host; port = state.rpc_port}

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
        node rpc =
      RPC_core.call_raw
        ?rpc_hooks
        ?log_request
        ?log_response_status
        ?log_response_body
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

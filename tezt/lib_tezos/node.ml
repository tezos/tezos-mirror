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

type history_mode = Archive | Full of int option | Rolling of int option

type media_type = Json | Binary | Any

let string_of_media_type = function
  | Any -> "any"
  | Binary -> "binary"
  | Json -> "json"

type argument =
  | Network of string
  | History_mode of history_mode
  | Expected_pow of int
  | Singleprocess
  | Bootstrap_threshold of int
  | Synchronisation_threshold of int
  | Connections of int
  | Private_mode
  | Peer of string
  | No_bootstrap_peers
  | Disable_operations_precheck
  | Media_type of media_type
  | Metadata_size_limit of int option
  | Metrics_addr of string

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
  | Connections x -> ["--connections"; string_of_int x]
  | Private_mode -> ["--private-mode"]
  | Peer x -> ["--peer"; x]
  | No_bootstrap_peers -> ["--no-bootstrap-peers"]
  | Disable_operations_precheck -> ["--disable-mempool-precheck"]
  | Media_type media_type -> ["--media-type"; string_of_media_type media_type]
  | Metadata_size_limit None -> ["--metadata-size-limit"; "unlimited"]
  | Metadata_size_limit (Some i) -> ["--metadata-size-limit"; string_of_int i]
  | Metrics_addr metrics_addr -> ["--metrics-addr"; metrics_addr]

let make_arguments arguments = List.flatten (List.map make_argument arguments)

type 'a known = Unknown | Known of 'a

module Parameters = struct
  type persistent_state = {
    data_dir : string;
    mutable net_port : int;
    advertised_net_port : int option;
    rpc_host : string;
    rpc_port : int;
    default_expected_pow : int;
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

let rpc_host node = node.persistent_state.rpc_host

let rpc_port node = node.persistent_state.rpc_port

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

let spawn_config_init node arguments =
  let arguments = node.persistent_state.arguments @ arguments in
  (* Since arguments will be in the configuration file, we will not need them after this. *)
  node.persistent_state.arguments <- [] ;
  let arguments =
    (* Give a default value of "sandbox" to --network. *)
    if List.exists (function Network _ -> true | _ -> false) arguments then
      arguments
    else Network "sandbox" :: arguments
  in
  spawn_command
    node
    ("config"
     ::
     "init"
     ::
     "--data-dir" :: node.persistent_state.data_dir :: make_arguments arguments)

let config_init node arguments =
  spawn_config_init node arguments |> Process.check

module Config_file = struct
  let filename node = sf "%s/config.json" @@ data_dir node

  let read node = JSON.parse_file (filename node)

  let write node config =
    with_open_out (filename node) @@ fun chan ->
    output_string chan (JSON.encode config)

  let update node update = read node |> update |> write node

  let set_prevalidator ?(operations_request_timeout = 10.)
      ?(max_refused_operations = 1000) ?(operations_batch_size = 50)
      ?(disable_operations_precheck = false) old_config =
    let prevalidator =
      `O
        [
          ("operations_request_timeout", `Float operations_request_timeout);
          ( "max_refused_operations",
            `Float (float_of_int max_refused_operations) );
          ("operations_batch_size", `Float (float_of_int operations_batch_size));
          ("disable_precheck", `Bool disable_operations_precheck);
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

  let set_sandbox_network_with_user_activated_upgrades upgrade_points old_config
      =
    let network =
      sandbox_network_config
      |> JSON.annotate
           ~origin:"set_sandbox_network_with_user_activated_upgrades"
      |> JSON.put
           ( "user_activated_upgrades",
             JSON.annotate ~origin:"user_activated_upgrades"
             @@ `A
                  (List.map
                     (fun (level, protocol) ->
                       `O
                         [
                           ("level", `Float (float level));
                           ( "replacement_protocol",
                             `String (Protocol.hash protocol) );
                         ])
                     upgrade_points) )
    in
    JSON.put ("network", network) old_config

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
end

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

let handle_event node {name; value} =
  match name with
  | "node_is_ready.v0" -> set_ready node
  | "node_chain_validator.v0" -> (
      match JSON.as_list_opt value with
      | Some [_timestamp; details] -> (
          match
            JSON.(
              details |-> "event" |-> "processed_block" |-> "level"
              |> as_int_opt)
          with
          | None ->
              (* There are several kinds of [node_chain_validator.v0] events
                 and maybe this one is not the one with the level: ignore it. *)
              ()
          | Some level -> update_level node level)
      | _ ->
          (* Other kind of node_chain_validator event that we don't care about. *)
          ())
  | "read_identity.v0" -> update_identity node (JSON.as_string value)
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
      let (promise, resolver) = Lwt.task () in
      node.persistent_state.pending_ready <-
        resolver :: node.persistent_state.pending_ready ;
      check_event node "node_is_ready.v0" promise

let wait_for_level node level =
  match node.status with
  | Running {session_state = {level = Known current_level; _}; _}
    when current_level >= level ->
      return current_level
  | Not_running | Running _ ->
      let (promise, resolver) = Lwt.task () in
      node.persistent_state.pending_level <-
        (level, resolver) :: node.persistent_state.pending_level ;
      check_event
        node
        "node_chain_validator.v0"
        ~where:("level >= " ^ string_of_int level)
        promise

let get_level node =
  match node.status with
  | Running {session_state = {level = Known level; _}; _} -> level
  | Not_running | Running _ -> 0

let wait_for_identity node =
  match node.status with
  | Running {session_state = {identity = Known identity; _}; _} ->
      return identity
  | Not_running | Running _ ->
      let (promise, resolver) = Lwt.task () in
      node.persistent_state.pending_identity <-
        resolver :: node.persistent_state.pending_identity ;
      check_event node "read_identity.v0" promise

let wait_for_request ~request node =
  let event_name =
    match request with
    | `Flush | `Inject -> "request_completed_notice.v0"
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

let create ?runner ?(path = Constant.tezos_node) ?name ?color ?data_dir
    ?event_pipe ?net_port ?advertised_net_port ?(rpc_host = "localhost")
    ?rpc_port arguments =
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
  let arguments =
    (* Give a default value of 0 to --expected-pow. *)
    if List.exists (function Expected_pow _ -> true | _ -> false) arguments
    then arguments
    else Expected_pow 0 :: arguments
  in
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
        net_port;
        advertised_net_port;
        rpc_host;
        rpc_port;
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

let point_and_id ?from node =
  let from =
    match from with None -> None | Some peer -> peer.persistent_state.runner
  in
  let address = Runner.address ?from node.persistent_state.runner ^ ":" in
  let* id = wait_for_identity node in
  Lwt.return (address ^ string_of_int (net_port node) ^ "#" ^ id)

let add_peer_with_id node peer =
  let* peer = point_and_id ~from:node peer in
  add_argument node (Peer peer) ;
  Lwt.return_unit

let get_peers node =
  List.filter_map
    (fun arg -> match arg with Peer s -> Some s | _ -> None)
    node.persistent_state.arguments

(** [runlike_command_arguments node command arguments]
    evaluates in a list of strings containing all command
    line arguments needed to spawn a [command] like [run]
    or [replay] for the given [node] and extra [arguments]. *)
let runlike_command_arguments node command arguments =
  let (net_addr, rpc_addr) =
    match node.persistent_state.runner with
    | None -> ("127.0.0.1:", node.persistent_state.rpc_host ^ ":")
    | Some _ ->
        (* FIXME spawn an ssh tunnel in case of remote host *)
        ("0.0.0.0:", "0.0.0.0:")
  in
  let arguments = node.persistent_state.arguments @ arguments in

  let command_args = make_arguments arguments in
  let command_args =
    match node.persistent_state.advertised_net_port with
    | None -> command_args
    | Some port -> "--advertised-net-port" :: string_of_int port :: command_args
  in
  command
  ::
  "--data-dir"
  ::
  node.persistent_state.data_dir
  ::
  "--net-addr"
  ::
  (net_addr ^ string_of_int node.persistent_state.net_port)
  ::
  "--rpc-addr"
  :: (rpc_addr ^ string_of_int node.persistent_state.rpc_port) :: command_args

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

let run ?on_terminate ?event_level ?event_sections_levels node arguments =
  let arguments = runlike_command_arguments node "run" arguments in
  do_runlike_command
    ?on_terminate
    ?event_level
    ?event_sections_levels
    node
    arguments

let replay ?on_terminate ?event_level ?event_sections_levels
    ?(blocks = ["head"]) node arguments =
  let arguments = runlike_command_arguments node "replay" arguments @ blocks in
  do_runlike_command
    ?on_terminate
    ?event_level
    ?event_sections_levels
    node
    arguments

let init ?runner ?path ?name ?color ?data_dir ?event_pipe ?net_port
    ?advertised_net_port ?rpc_host ?rpc_port ?event_level ?event_sections_levels
    arguments =
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
      ?rpc_host
      ?rpc_port
      arguments
  in
  let* () = identity_generate node in
  let* () = config_init node [] in
  let* () = run ?event_level ?event_sections_levels node [] in
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

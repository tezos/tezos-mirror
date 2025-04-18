(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type liquidity_baking_vote = Off | On | Pass

let liquidity_baking_vote_to_string = function
  | Off -> "off"
  | On -> "on"
  | Pass -> "pass"

let liquidity_baking_vote_of_string_opt = function
  | "off" -> Some Off
  | "on" -> Some On
  | "pass" -> Some Pass
  | _ -> None

type protocol_status = Active | Frozen | Ignore

let protocol_status = function Protocol.Alpha -> Ignore | _ -> Active

(* This is hard-coded after the same value from [Daemon] module from
   [src/lib_agnostic_baker]. *)
let extra_levels_for_old_baker = 3

module Parameters = struct
  type persistent_state = {
    delegates : string list;
    runner : Runner.t option;
    base_dir : string;
    node_data_dir : string;
    node_rpc_endpoint : Endpoint.t;
    dal_node_rpc_endpoint : Endpoint.t option;
    dal_node_timeout_percentage : int option;
    mutable pending_ready : unit option Lwt.u list;
    votefile : string option;
    liquidity_baking_toggle_vote : liquidity_baking_vote option;
    force_apply_from_round : int option;
    remote_mode : bool;
    operations_pool : string option;
    state_recorder : bool;
    node_version_check_bypass : bool;
    node_version_allowed : string option;
    keep_alive : bool;
  }

  type session_state = {mutable ready : bool}

  let base_default_name = "agnostic-baker"

  let default_colors = Log.Color.[|FG.green|]
end

open Parameters
include Daemon.Make (Parameters)

let trigger_ready agnostic_baker value =
  let pending = agnostic_baker.persistent_state.pending_ready in
  agnostic_baker.persistent_state.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let set_ready agnostic_baker =
  (match agnostic_baker.status with
  | Not_running -> ()
  | Running status -> status.session_state.ready <- true) ;
  trigger_ready agnostic_baker (Some ())

let liquidity_baking_votefile ?path vote =
  let votefile =
    Option.value
      path
      ~default:(Temp.file "liquidity_baking_toggle_votefile.json")
  in
  JSON.encode_to_file_u
    votefile
    (`O
      [
        ( "liquidity_baking_toggle_vote",
          `String (liquidity_baking_vote_to_string vote) );
      ]) ;
  votefile

let enable_remote_mode =
  match Sys.getenv_opt "TZ_SCHEDULE_KIND" with
  | Some "EXTENDED_BAKER_REMOTE_MODE_TESTS" -> Some true
  | Some _ -> Some false
  | _ -> None

let create_from_uris ?runner ?(path = Uses.path Constant.octez_agnostic_baker)
    ?name ?color ?event_pipe ?(delegates = []) ?votefile
    ?(liquidity_baking_toggle_vote = Some Pass) ?force_apply_from_round
    ?remote_mode ?operations_pool ?dal_node_rpc_endpoint
    ?dal_node_timeout_percentage ?(state_recorder = false)
    ?(node_version_check_bypass = false) ?node_version_allowed ~base_dir
    ~node_data_dir ~node_rpc_endpoint ?(keep_alive = false) () =
  let remote_mode =
    Option.value
      remote_mode
      ~default:(Option.value enable_remote_mode ~default:false)
  in
  let agnostic_baker =
    create
      ~path
      ?name
      ?color
      ?event_pipe
      ?runner
      {
        delegates;
        runner;
        base_dir;
        node_data_dir;
        node_rpc_endpoint;
        pending_ready = [];
        votefile;
        liquidity_baking_toggle_vote;
        force_apply_from_round;
        remote_mode;
        operations_pool;
        dal_node_rpc_endpoint;
        dal_node_timeout_percentage;
        state_recorder;
        node_version_check_bypass;
        node_version_allowed;
        keep_alive;
      }
  in
  agnostic_baker

let handle_event node ({name; _} : event) =
  match name with "starting_daemon.v0" -> set_ready node | _ -> ()

let create ?runner ?path ?name ?color ?event_pipe ?(delegates = []) ?votefile
    ?(liquidity_baking_toggle_vote = Some Pass) ?force_apply_from_round
    ?remote_mode ?operations_pool ?dal_node_rpc_endpoint
    ?dal_node_timeout_percentage ?(state_recorder = false)
    ?(node_version_check_bypass = false) ?node_version_allowed ?keep_alive node
    client =
  let agnostic_baker =
    create_from_uris
      ?runner
      ?path
      ?name
      ?color
      ?event_pipe
      ~delegates
      ?votefile
      ~liquidity_baking_toggle_vote
      ?force_apply_from_round
      ?remote_mode
      ?operations_pool
      ?dal_node_rpc_endpoint
      ?dal_node_timeout_percentage
      ~state_recorder
      ~node_version_check_bypass
      ?node_version_allowed
      ~base_dir:(Client.base_dir client)
      ~node_data_dir:(Node.data_dir node)
      ~node_rpc_endpoint:(Node.as_rpc_endpoint node)
      ?keep_alive
      ()
  in
  on_event agnostic_baker (handle_event agnostic_baker) ;
  agnostic_baker

let run_args agnostic_baker =
  let delegates = agnostic_baker.persistent_state.delegates in
  let node_data_dir = agnostic_baker.persistent_state.node_data_dir in
  let base_dir = agnostic_baker.persistent_state.base_dir in
  let node_addr =
    Endpoint.as_string agnostic_baker.persistent_state.node_rpc_endpoint
  in
  let votefile =
    Cli_arg.optional_arg
      "votefile"
      Fun.id
      agnostic_baker.persistent_state.votefile
  in
  let liquidity_baking_toggle_vote =
    Cli_arg.optional_arg
      "liquidity-baking-toggle-vote"
      liquidity_baking_vote_to_string
      agnostic_baker.persistent_state.liquidity_baking_toggle_vote
  in
  let force_apply_from_round =
    (* From Protocol Q, the flag --force-apply has been replaced by
       --force-apply-from-round, the following maintains back-compatibility with
       ParisC tests. *)
    Cli_arg.optional_arg
      "force-apply-from-round"
      string_of_int
      agnostic_baker.persistent_state.force_apply_from_round
  in
  let operations_pool =
    Cli_arg.optional_arg
      "operations-pool"
      Fun.id
      agnostic_baker.persistent_state.operations_pool
  in
  let dal_node_endpoint =
    Cli_arg.optional_arg
      "dal-node"
      Endpoint.as_string
      agnostic_baker.persistent_state.dal_node_rpc_endpoint
  in
  let without_dal =
    Cli_arg.optional_switch
      "without-dal"
      (Option.is_none agnostic_baker.persistent_state.dal_node_rpc_endpoint)
  in
  let dal_node_timeout_percentage =
    Cli_arg.optional_arg
      "dal-node-timeout-percentage"
      string_of_int
      agnostic_baker.persistent_state.dal_node_timeout_percentage
  in
  let state_recorder =
    Cli_arg.optional_switch
      "record-state"
      agnostic_baker.persistent_state.state_recorder
  in
  let node_version_check_bypass =
    Cli_arg.optional_switch
      "node-version-check-bypass"
      agnostic_baker.persistent_state.node_version_check_bypass
  in
  let node_version_allowed =
    Cli_arg.optional_arg
      "node-version-allowed"
      Fun.id
      agnostic_baker.persistent_state.node_version_allowed
  in
  let keep_alive =
    Cli_arg.optional_switch
      "keep-alive"
      agnostic_baker.persistent_state.keep_alive
  in
  let run_args =
    if agnostic_baker.persistent_state.remote_mode then ["remotely"]
    else ["with"; "local"; "node"; node_data_dir]
  in
  ["--endpoint"; node_addr; "--base-dir"; base_dir; "run"]
  @ run_args @ delegates @ liquidity_baking_toggle_vote @ votefile
  @ force_apply_from_round @ operations_pool @ dal_node_endpoint @ without_dal
  @ dal_node_timeout_percentage @ state_recorder @ node_version_check_bypass
  @ node_version_allowed @ keep_alive

let run ?env ?event_level ?event_sections_levels ?(extra_arguments = [])
    (agnostic_baker : t) =
  (match agnostic_baker.status with
  | Not_running -> ()
  | Running _ ->
      Test.fail "agnostic_baker %s is already running" agnostic_baker.name) ;
  let on_terminate _ =
    (* Cancel all [Ready] event listeners. *)
    trigger_ready agnostic_baker None ;
    unit
  in
  run
    ?env
    ?event_level
    ?event_sections_levels
    agnostic_baker
    {ready = false}
    (run_args agnostic_baker @ extra_arguments)
    ~on_terminate
    ?runner:agnostic_baker.persistent_state.runner

let spawn_run ?env (agnostic_baker : t) =
  (match agnostic_baker.status with
  | Not_running -> ()
  | Running _ ->
      Test.fail "agnostic_baker %s is already running" agnostic_baker.name) ;
  Process.spawn
    ?env
    ?runner:agnostic_baker.persistent_state.runner
    agnostic_baker.path
    (run_args agnostic_baker)

let check_event ?where agnostic_baker name promise =
  let* result = promise in
  match result with
  | None ->
      raise
        (Terminated_before_event
           {daemon = agnostic_baker.name; event = name; where})
  | Some x -> return x

let wait_for_ready agnostic_baker =
  match agnostic_baker.status with
  | Running {session_state = {ready = true; _}; _} -> unit
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      let promise, resolver = Lwt.task () in
      agnostic_baker.persistent_state.pending_ready <-
        resolver :: agnostic_baker.persistent_state.pending_ready ;
      check_event agnostic_baker "agnostic baker started" promise

let init ?env ?runner ?(path = Uses.path Constant.octez_agnostic_baker) ?name
    ?color ?event_level ?event_pipe ?event_sections_levels ?(delegates = [])
    ?votefile ?liquidity_baking_toggle_vote ?force_apply_from_round ?remote_mode
    ?operations_pool ?dal_node_rpc_endpoint ?dal_node_timeout_percentage
    ?state_recorder ?node_version_check_bypass ?node_version_allowed ?keep_alive
    node client =
  let* () = Node.wait_for_ready node in
  let agnostic_baker =
    create
      ?runner
      ~path
      ?name
      ?color
      ?event_pipe
      ?votefile
      ?liquidity_baking_toggle_vote
      ?force_apply_from_round
      ?remote_mode
      ?operations_pool
      ?dal_node_rpc_endpoint
      ?dal_node_timeout_percentage
      ?state_recorder
      ?node_version_check_bypass
      ?node_version_allowed
      ~delegates
      ?keep_alive
      node
      client
  in
  let* () = run ?env ?event_level ?event_sections_levels agnostic_baker in
  let* () = wait_for_ready agnostic_baker in
  return agnostic_baker

(** Logging helpers for baker events. *)

let log_block_injection ?color baker =
  on_event baker (fun event ->
      if String.equal event.name "block_injected.v0" then
        let open JSON in
        let level = event.value |-> "level" |> as_int in
        let round = event.value |-> "round" |> as_int in
        let delegate =
          event.value |-> "delegate" |-> "alias" |> as_string_opt
        in
        let delegate =
          match delegate with
          | None ->
              (* The encoding was changed in Protocol 023 *)
              event.value |-> "delegate" |-> "consensus_key" |-> "alias"
              |> as_string
          | Some delegate -> delegate
        in
        Log.info
          ?color
          "[%s] Block injected at level %d round %d for %s."
          (name baker)
          level
          round
          delegate)

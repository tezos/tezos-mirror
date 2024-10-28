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

module Parameters = struct
  type persistent_state = {
    delegates : string list;
    runner : Runner.t option;
    base_dir : string;
    node_data_dir : string;
    node_rpc_endpoint : Endpoint.t;
    mutable pending_ready : unit option Lwt.u list;
    remote_mode : bool;
    liquidity_baking_toggle_vote : liquidity_baking_vote option;
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

let create_from_uris ?runner ?(path = Uses.path Constant.octez_agnostic_baker)
    ?name ?color ?event_pipe ?(delegates = []) ?(remote_mode = false)
    ?(liquidity_baking_toggle_vote = Some Pass) ~base_dir ~node_data_dir
    ~node_rpc_endpoint () =
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
        remote_mode;
        liquidity_baking_toggle_vote;
      }
  in
  agnostic_baker

let handle_event node ({name; _} : event) =
  match name with "starting_daemon.v0" -> set_ready node | _ -> ()

let create ?runner ?path ?name ?color ?event_pipe ?(delegates = [])
    ?(remote_mode = false) ?(liquidity_baking_toggle_vote = Some Pass) node
    client =
  let agnostic_baker =
    create_from_uris
      ?runner
      ?path
      ?name
      ?color
      ?event_pipe
      ~delegates
      ~remote_mode
      ~liquidity_baking_toggle_vote
      ~base_dir:(Client.base_dir client)
      ~node_data_dir:(Node.data_dir node)
      ~node_rpc_endpoint:(Node.as_rpc_endpoint node)
      ()
  in
  on_event agnostic_baker (handle_event agnostic_baker) ;
  agnostic_baker

let run ?event_level ?event_sections_levels (agnostic_baker : t) =
  (match agnostic_baker.status with
  | Not_running -> ()
  | Running _ ->
      Test.fail "agnostic_baker %s is already running" agnostic_baker.name) ;
  let delegates = agnostic_baker.persistent_state.delegates in
  let runner = agnostic_baker.persistent_state.runner in
  let node_data_dir = agnostic_baker.persistent_state.node_data_dir in
  let base_dir = agnostic_baker.persistent_state.base_dir in
  let node_addr =
    Endpoint.as_string agnostic_baker.persistent_state.node_rpc_endpoint
  in
  let run_args =
    if agnostic_baker.persistent_state.remote_mode then ["remotely"]
    else ["with"; "local"; "node"; node_data_dir]
  in
  let liquidity_baking_toggle_vote =
    Cli_arg.optional_arg
      "liquidity-baking-toggle-vote"
      liquidity_baking_vote_to_string
      agnostic_baker.persistent_state.liquidity_baking_toggle_vote
  in
  let arguments =
    ["--"; "--endpoint"; node_addr; "--base-dir"; base_dir; "run"]
    @ run_args @ delegates @ liquidity_baking_toggle_vote
  in

  let on_terminate _ =
    (* Cancel all [Ready] event listeners. *)
    trigger_ready agnostic_baker None ;
    unit
  in
  run
    ?event_level
    ?event_sections_levels
    agnostic_baker
    {ready = false}
    arguments
    ~on_terminate
    ?runner

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

let init ?runner ?(path = Uses.path Constant.octez_agnostic_baker) ?name ?color
    ?event_level ?event_pipe ?event_sections_levels ?(delegates = [])
    ?remote_mode ?liquidity_baking_toggle_vote node client =
  let* () = Node.wait_for_ready node in
  let agnostic_baker =
    create
      ?runner
      ~path
      ?name
      ?color
      ?event_pipe
      ?remote_mode
      ?liquidity_baking_toggle_vote
      ~delegates
      node
      client
  in
  let* () = run ?event_level ?event_sections_levels agnostic_baker in
  let* () = wait_for_ready agnostic_baker in
  return agnostic_baker

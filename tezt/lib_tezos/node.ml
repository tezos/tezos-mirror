(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type 'a known = Unknown | Known of 'a

module Parameters = struct
  type persistent_state = {
    data_dir : string;
    mutable net_port : int;
    mutable rpc_port : int;
    mutable pending_ready : unit option Lwt.u list;
    mutable pending_level : (int * int option Lwt.u) list;
    mutable pending_identity : string option Lwt.u list;
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

let name node = node.name

let net_port node = node.persistent_state.net_port

let rpc_port node = node.persistent_state.rpc_port

let data_dir node = node.persistent_state.data_dir

let next_port = ref 19732

let fresh_port () =
  let port = !next_port in
  incr next_port ; port

let () = Test.declare_reset_function @@ fun () -> next_port := 19732

let spawn_command node =
  Process.spawn ~name:node.name ~color:node.color node.path

let spawn_identity_generate ?(expected_pow = 0) node =
  spawn_command
    node
    [ "identity";
      "generate";
      "--data-dir";
      node.persistent_state.data_dir;
      string_of_int expected_pow ]

let identity_generate ?expected_pow node =
  spawn_identity_generate ?expected_pow node |> Process.check

type history_mode = Archive | Full | Rolling

let show_history_mode = function
  | Archive ->
      "archive"
  | Full ->
      "full"
  | Rolling ->
      "rolling"

let spawn_config_init ?(network = "sandbox") ?net_port ?rpc_port ?history_mode
    node =
  ( match net_port with
  | None ->
      ()
  | Some port ->
      node.persistent_state.net_port <- port ) ;
  ( match rpc_port with
  | None ->
      ()
  | Some port ->
      node.persistent_state.rpc_port <- port ) ;
  spawn_command
    node
    ( "config" :: "init" :: "--data-dir" :: node.persistent_state.data_dir
    :: "--network" :: network :: "--net-addr"
    :: ("127.0.0.1:" ^ string_of_int node.persistent_state.net_port)
    :: "--rpc-addr"
    :: ("localhost:" ^ string_of_int node.persistent_state.rpc_port)
    ::
    ( match history_mode with
    | None ->
        []
    | Some Full ->
        ["--history-mode"; "full"]
    | Some Archive ->
        ["--history-mode"; "archive"]
    | Some Rolling ->
        ["--history-mode"; "experimental-rolling"] ) )

let config_init ?network ?net_port ?rpc_port ?history_mode node =
  spawn_config_init ?network ?net_port ?rpc_port ?history_mode node
  |> Process.check

let trigger_ready node value =
  let pending = node.persistent_state.pending_ready in
  node.persistent_state.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let set_ready node =
  ( match node.status with
  | Not_running ->
      ()
  | Running status ->
      status.session_state.ready <- true ) ;
  trigger_ready node (Some ())

let update_level node current_level =
  ( match node.status with
  | Not_running ->
      ()
  | Running status -> (
    match status.session_state.level with
    | Unknown ->
        status.session_state.level <- Known current_level
    | Known old_level ->
        status.session_state.level <- Known (max old_level current_level) ) ) ;
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
  | Not_running ->
      ()
  | Running status ->
      ( match status.session_state.identity with
      | Unknown ->
          status.session_state.identity <- Known identity
      | Known identity' ->
          if identity' <> identity then Test.fail "node identity changed" ) ;
      let pending = node.persistent_state.pending_identity in
      node.persistent_state.pending_identity <- [] ;
      List.iter
        (fun resolver -> Lwt.wakeup_later resolver (Some identity))
        pending

let handle_event node {name; value} =
  match name with
  | "node_is_ready.v0" ->
      set_ready node
  | "node_chain_validator.v0" -> (
    match JSON.as_list_opt value with
    | Some [_timestamp; details] -> (
      match JSON.(details |-> "event" |-> "level" |> as_int_opt) with
      | None ->
          (* There are several kinds of [node_chain_validator.v0] events
             and maybe this one is not the one with the level: ignore it. *)
          ()
      | Some level ->
          update_level node level )
    | _ ->
        (* Other kind of node_chain_validator event that we don't care about. *)
        () )
  | "read_identity.v0" ->
      update_identity node (JSON.as_string value)
  | _ ->
      ()

let create ?(path = Constant.tezos_node) ?name ?color ?data_dir ?event_pipe
    ?net_port ?rpc_port () =
  let name = match name with None -> fresh_name () | Some name -> name in
  let data_dir =
    match data_dir with None -> Temp.dir name | Some dir -> dir
  in
  let net_port =
    match net_port with None -> fresh_port () | Some port -> port
  in
  let rpc_port =
    match rpc_port with None -> fresh_port () | Some port -> port
  in
  let node =
    create
      ~path
      ~name
      ?color
      ?event_pipe
      {
        data_dir;
        net_port;
        rpc_port;
        pending_ready = [];
        pending_level = [];
        pending_identity = [];
      }
  in
  on_event node (handle_event node) ;
  node

let run ?(expected_pow = 0) ?(single_process = false) ?bootstrap_threshold
    ?synchronisation_threshold ?connections ?(private_mode = false) node =
  ( match node.status with
  | Not_running ->
      ()
  | Running _ ->
      Test.fail "node %s is already running" node.name ) ;
  let arguments =
    "run" :: "--expected-pow" :: string_of_int expected_pow :: "--data-dir"
    :: node.persistent_state.data_dir
    ::
    ( match bootstrap_threshold with
    | None ->
        []
    | Some x ->
        ["--bootstrap-threshold"; string_of_int x] )
    @ ( match synchronisation_threshold with
      | None ->
          []
      | Some x ->
          ["--synchronisation-threshold"; string_of_int x] )
    @ ( match connections with
      | None ->
          []
      | Some x ->
          ["--connections"; string_of_int x] )
    @ (if single_process then ["--singleprocess"] else [])
    @ if private_mode then ["--private-mode"] else []
  in
  let on_terminate _ =
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
    node
    {ready = false; level = Unknown; identity = Unknown}
    arguments
    ~on_terminate

let check_event ?where node name promise =
  let* result = promise in
  match result with
  | None ->
      raise (Terminated_before_event {daemon = node.name; event = name; where})
  | Some x ->
      return x

let wait_for_ready node =
  match node.status with
  | Running {session_state = {ready = true; _}; _} ->
      unit
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

let wait_for_identity node =
  match node.status with
  | Running {session_state = {identity = Known identity; _}; _} ->
      return identity
  | Not_running | Running _ ->
      let (promise, resolver) = Lwt.task () in
      node.persistent_state.pending_identity <-
        resolver :: node.persistent_state.pending_identity ;
      check_event node "read_identity.v0" promise

let init ?path ?name ?color ?data_dir ?event_pipe ?expected_pow ?network
    ?net_port ?rpc_port ?history_mode ?single_process ?bootstrap_threshold
    ?synchronisation_threshold ?connections ?private_mode () =
  let node = create ?path ?name ?color ?data_dir ?event_pipe () in
  let* () = identity_generate ?expected_pow node in
  let* () = config_init ?network ?net_port ?rpc_port ?history_mode node in
  let* () =
    run
      ?expected_pow
      ?single_process
      ?bootstrap_threshold
      ?synchronisation_threshold
      ?connections
      ?private_mode
      node
  in
  let* () = wait_for_ready node in
  return node

let restart ?expected_pow ?single_process ?bootstrap_threshold
    ?synchronisation_threshold ?connections ?private_mode node =
  let* () = terminate node in
  let* () =
    run
      ?expected_pow
      ?single_process
      ?bootstrap_threshold
      ?synchronisation_threshold
      ?connections
      ?private_mode
      node
  in
  wait_for_ready node

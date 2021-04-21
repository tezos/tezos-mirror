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

type history_mode = Archive | Full | Rolling

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

let make_argument = function
  | Network x ->
      ["--network"; x]
  | History_mode Archive ->
      ["--history-mode"; "archive"]
  | History_mode Full ->
      ["--history-mode"; "full"]
  | History_mode Rolling ->
      ["--history-mode"; "experimental-rolling"]
  | Expected_pow x ->
      ["--expected-pow"; string_of_int x]
  | Singleprocess ->
      ["--singleprocess"]
  | Bootstrap_threshold x ->
      ["--bootstrap-threshold"; string_of_int x]
  | Synchronisation_threshold x ->
      ["--synchronisation-threshold"; string_of_int x]
  | Connections x ->
      ["--connections"; string_of_int x]
  | Private_mode ->
      ["--private-mode"]
  | Peer x ->
      ["--peer"; x]

let make_arguments arguments = List.flatten (List.map make_argument arguments)

type 'a known = Unknown | Known of 'a

module Parameters = struct
  type persistent_state = {
    data_dir : string;
    mutable net_port : int;
    mutable rpc_port : int;
    default_expected_pow : int;
    mutable arguments : argument list;
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

let spawn_identity_generate ?expected_pow node =
  spawn_command
    node
    [ "identity";
      "generate";
      "--data-dir";
      node.persistent_state.data_dir;
      string_of_int
        (Option.value
           expected_pow
           ~default:node.persistent_state.default_expected_pow) ]

let identity_generate ?expected_pow node =
  spawn_identity_generate ?expected_pow node |> Process.check

let show_history_mode = function
  | Archive ->
      "archive"
  | Full ->
      "full"
  | Rolling ->
      "rolling"

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
    ( "config" :: "init" :: "--data-dir" :: node.persistent_state.data_dir
    :: "--net-addr"
    :: ("127.0.0.1:" ^ string_of_int node.persistent_state.net_port)
    :: "--rpc-addr"
    :: ("localhost:" ^ string_of_int node.persistent_state.rpc_port)
    :: make_arguments arguments )

let config_init node arguments =
  spawn_config_init node arguments |> Process.check

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

let create ?(path = Constant.tezos_node) ?name ?color ?data_dir ?event_pipe
    ?net_port ?rpc_port arguments =
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
      ~path
      ~name
      ?color
      ?event_pipe
      {
        data_dir;
        net_port;
        rpc_port;
        arguments;
        default_expected_pow;
        pending_ready = [];
        pending_level = [];
        pending_identity = [];
      }
  in
  on_event node (handle_event node) ;
  node

let add_argument node argument =
  node.persistent_state.arguments <-
    argument :: node.persistent_state.arguments

let add_peer node peer =
  add_argument node (Peer ("127.0.0.1:" ^ string_of_int (net_port peer)))

let point_and_id node =
  let* id = wait_for_identity node in
  Lwt.return ("127.0.0.1:" ^ string_of_int (net_port node) ^ "#" ^ id)

let add_peer_with_id node peer =
  let* peer = point_and_id peer in
  add_argument node (Peer peer) ;
  Lwt.return_unit

let run ?(on_terminate = fun _ -> ()) node arguments =
  ( match node.status with
  | Not_running ->
      ()
  | Running _ ->
      Test.fail "node %s is already running" node.name ) ;
  let arguments = node.persistent_state.arguments @ arguments in
  let arguments =
    "run" :: "--data-dir" :: node.persistent_state.data_dir
    :: make_arguments arguments
  in
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
    node
    {ready = false; level = Unknown; identity = Unknown}
    arguments
    ~on_terminate

let init ?path ?name ?color ?data_dir ?event_pipe ?net_port ?rpc_port arguments
    =
  let node =
    create
      ?path
      ?name
      ?color
      ?data_dir
      ?event_pipe
      ?net_port
      ?rpc_port
      arguments
  in
  let* () = identity_generate node in
  let* () = config_init node [] in
  let* () = run node [] in
  let* () = wait_for_ready node in
  return node

let restart node arguments =
  let* () = terminate node in
  let* () = run node arguments in
  wait_for_ready node

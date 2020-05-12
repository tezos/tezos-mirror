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

open Base

type 'a known = Unknown | Known of 'a

type status =
  | Not_running
  | Running of {
      process : Process.t;
      mutable ready : bool;
      mutable level : int known;
      mutable identity : string known;
    }

module String_map = Map.Make (String)

type custom_event =
  | Custom_event : {
      filter : JSON.t -> 'a option;
      resolver : 'a option Lwt.u;
    }
      -> custom_event

type event = {name : string; value : JSON.t}

(* [raw_event_handlers] contains handlers for events registered with [on_event].

   [pending] fields contain resolvers for event promises
   registered with [wait_for] functions. *)
type t = {
  name : string;
  color : Log.Color.t;
  data_dir : string;
  mutable net_port : int;
  mutable rpc_port : int;
  event_pipe : string;
  mutable status : status;
  mutable raw_event_handlers : (event -> unit) list;
  mutable pending_ready : unit option Lwt.u list;
  mutable pending_level : (int * int option Lwt.u) list;
  mutable pending_identity : string option Lwt.u list;
  mutable pending_custom : custom_event list String_map.t;
}

let name node = node.name

let net_port node = node.net_port

let rpc_port node = node.rpc_port

let terminate node =
  match node.status with
  | Not_running ->
      unit
  | Running {process; _} ->
      Process.terminate process ;
      let* _ = Process.wait process in
      unit

let next_name = ref 1

let fresh_name () =
  let index = !next_name in
  incr next_name ;
  "node" ^ string_of_int index

let next_port = ref 19732

let fresh_port () =
  let port = !next_port in
  incr next_port ; port

(* Blue is reserved for the client.
   White is reserved for non-process test logs.
   Red is reserved for errors and warnings.
   Black is unreadable.
   This leaves the following colors. *)
let colors = Log.Color.[|FG.cyan; FG.magenta; FG.yellow; FG.green|]

let next_color = ref 0

let get_next_color () =
  let color = colors.(!next_color mod Array.length colors) in
  incr next_color ; color

let () =
  Test.declare_reset_function
  @@ fun () ->
  next_name := 1 ;
  next_port := 19732 ;
  next_color := 0

let create ?name ?color ?data_dir ?event_pipe ?net_port ?rpc_port () =
  let name = match name with None -> fresh_name () | Some name -> name in
  let color =
    match color with None -> get_next_color () | Some color -> color
  in
  let data_dir =
    match data_dir with None -> Temp.dir name | Some dir -> dir
  in
  let net_port =
    match net_port with None -> fresh_port () | Some port -> port
  in
  let rpc_port =
    match rpc_port with None -> fresh_port () | Some port -> port
  in
  let event_pipe =
    match event_pipe with
    | None ->
        Temp.file (name ^ "-event-pipe")
    | Some file ->
        file
  in
  {
    name;
    color;
    data_dir;
    net_port;
    rpc_port;
    event_pipe;
    status = Not_running;
    raw_event_handlers = [];
    pending_ready = [];
    pending_level = [];
    pending_identity = [];
    pending_custom = String_map.empty;
  }

let run_command node =
  Process.run ~name:node.name ~color:node.color Constant.tezos_node

let identity_generate ?(expected_pow = 0) node =
  run_command
    node
    [ "identity";
      "generate";
      "--data-dir";
      node.data_dir;
      string_of_int expected_pow ]

type history_mode = Archive | Full | Rolling

let show_history_mode = function
  | Archive ->
      "archive"
  | Full ->
      "full"
  | Rolling ->
      "rolling"

let config_init ?(network = "sandbox") ?net_port ?rpc_port ?history_mode node =
  (match net_port with None -> () | Some port -> node.net_port <- port) ;
  (match rpc_port with None -> () | Some port -> node.rpc_port <- port) ;
  run_command
    node
    ( "config" :: "init" :: "--data-dir" :: node.data_dir :: "--network"
    :: network :: "--net-addr"
    :: ("[::1]:" ^ string_of_int node.net_port)
    :: "--rpc-addr"
    :: ("localhost:" ^ string_of_int node.rpc_port)
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

let trigger_ready node value =
  let pending = node.pending_ready in
  node.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let set_ready node =
  ( match node.status with
  | Not_running ->
      ()
  | Running status ->
      status.ready <- true ) ;
  trigger_ready node (Some ())

let update_level node current_level =
  ( match node.status with
  | Not_running ->
      ()
  | Running status -> (
    match status.level with
    | Unknown ->
        status.level <- Known current_level
    | Known old_level ->
        status.level <- Known (max old_level current_level) ) ) ;
  let pending = node.pending_level in
  node.pending_level <- [] ;
  List.iter
    (fun ((level, resolver) as pending) ->
      if current_level >= level then
        Lwt.wakeup_later resolver (Some current_level)
      else node.pending_level <- pending :: node.pending_level)
    pending

let update_identity node identity =
  match node.status with
  | Not_running ->
      ()
  | Running status ->
      ( match status.identity with
      | Unknown ->
          status.identity <- Known identity
      | Known identity' ->
          if identity' <> identity then Test.fail "node identity changed" ) ;
      let pending = node.pending_identity in
      node.pending_identity <- [] ;
      List.iter
        (fun resolver -> Lwt.wakeup_later resolver (Some identity))
        pending

let trigger_custom_events node name value =
  match String_map.find_opt name node.pending_custom with
  | None ->
      ()
  | Some events ->
      (* Trigger matching events and accumulate others in [acc]. *)
      let rec loop acc = function
        | [] ->
            node.pending_custom <-
              String_map.add name (List.rev acc) node.pending_custom
        | (Custom_event {filter; resolver; _} as head) :: tail ->
            let acc =
              match filter value with
              | exception exn ->
                  (* We cannot have [async] promises raise exceptions other than
                     [Test.Failed], and events are handled with [async] so we
                     need to convert the exception. *)
                  Test.fail
                    "uncaught exception in filter for event %s of node %s: %s"
                    name
                    node.name
                    (Printexc.to_string exn)
              | None ->
                  head :: acc
              | Some value ->
                  Lwt.wakeup_later resolver (Some value) ;
                  acc
            in
            loop acc tail
      in
      loop [] events

let handle_raw_event node line =
  let open JSON in
  let json = parse ~origin:"node event" line in
  let event = json |-> "fd-sink-item.v0" |-> "event" in
  match as_object_opt event with
  | None | Some ([] | _ :: _ :: _) ->
      (* Some events are not one-field objects. Ignore them for now. *)
      ()
  | Some [(name, value)] -> (
      let raw_event = {name; value} in
      List.iter (fun handler -> handler raw_event) node.raw_event_handlers ;
      trigger_custom_events node name value ;
      match name with
      | "node_is_ready.v0" ->
          set_ready node
      | "node_chain_validator.v0" -> (
        match as_list_opt value with
        | Some [_timestamp; details] -> (
          (* TODO: should we also check that ["outcome" = "increment"]? *)
          match details |-> "event" |-> "level" |> as_int_opt with
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
          update_identity node (as_string value)
      | _ ->
          () )

let run ?(expected_pow = 0) ?(single_process = false) ?bootstrap_threshold
    ?connections node =
  let args =
    "run" :: "--expected-pow" :: string_of_int expected_pow :: "--data-dir"
    :: node.data_dir
    ::
    ( match bootstrap_threshold with
    | None ->
        []
    | Some x ->
        ["--bootstrap-threshold"; string_of_int x] )
    @ ( match connections with
      | None ->
          []
      | Some x ->
          ["--connections"; string_of_int x] )
    @ if single_process then ["--singleprocess"] else []
  in
  (* Create the named pipe where the node will send its internal events in JSON. *)
  if Sys.file_exists node.event_pipe then Sys.remove node.event_pipe ;
  Unix.mkfifo node.event_pipe 0o640 ;
  let process =
    Process.spawn
      ~name:node.name
      ~color:node.color
      ~env:
        [("TEZOS_EVENTS_CONFIG", "file-descriptor-path://" ^ node.event_pipe)]
      Constant.tezos_node
      args
  in
  node.status <-
    Running {process; ready = false; level = Unknown; identity = Unknown} ;
  (* Return control now, the rest (event handling) happens concurrently. *)
  async
  @@ let* input = Lwt_io.(open_file ~mode:input) node.event_pipe in
     let rec event_loop () =
       let* line = Lwt_io.read_line_opt input in
       match line with
       | Some line ->
           handle_raw_event node line ; event_loop ()
       | None -> (
         match node.status with
         | Not_running ->
             (* Process terminated, there will be no more events. *)
             Lwt_io.close input
         | Running _ ->
             (* It can take a little while before the pipe is opened by the node,
                and before that, reading from it yields end of file for some reason. *)
             let* () = Lwt_unix.sleep 0.01 in
             event_loop () )
     in
     let* () = event_loop ()
     and* () =
       let* _ = Process.wait process in
       (* Setting [node.status] to [Not_running] stops the event loop cleanly. *)
       node.status <- Not_running ;
       (* Cancel all [Ready] event listeners. *)
       trigger_ready node None ;
       (* Cancel all [Level_at_least] event listeners. *)
       let pending = node.pending_level in
       node.pending_level <- [] ;
       List.iter (fun (_, pending) -> Lwt.wakeup_later pending None) pending ;
       (* Cancel all [Read_identity] event listeners. *)
       let pending = node.pending_identity in
       node.pending_identity <- [] ;
       List.iter (fun pending -> Lwt.wakeup_later pending None) pending ;
       (* Cancel all [Any] event listeners. *)
       let pending = node.pending_custom in
       node.pending_custom <- String_map.empty ;
       String_map.iter
         (fun _ ->
           List.iter (fun (Custom_event {resolver; _}) ->
               Lwt.wakeup_later resolver None))
         pending ;
       unit
     in
     unit

exception
  Terminated_before_event of {
    node : string;
    event : string;
    where : string option;
  }

let () =
  Printexc.register_printer
  @@ function
  | Terminated_before_event {node; event; where = None} ->
      Some (sf "%s terminated before event occurred: %s" node event)
  | Terminated_before_event {node; event; where = Some where} ->
      Some
        (sf "%s terminated before event occurred: %s where %s" node event where)
  | _ ->
      None

let check_event ?where node name promise =
  let* result = promise in
  match result with
  | None ->
      raise (Terminated_before_event {node = node.name; event = name; where})
  | Some x ->
      return x

let wait_for_ready node =
  match node.status with
  | Running {ready = true; _} ->
      unit
  | Not_running | Running {ready = false; _} ->
      let (promise, resolver) = Lwt.task () in
      node.pending_ready <- resolver :: node.pending_ready ;
      check_event node "node_is_ready" promise

let wait_for_level node level =
  match node.status with
  | Running {level = Known current_level; _} when current_level >= level ->
      return current_level
  | Not_running | Running _ ->
      let (promise, resolver) = Lwt.task () in
      node.pending_level <- (level, resolver) :: node.pending_level ;
      check_event
        node
        "node_chain_validator"
        ~where:("level >= " ^ string_of_int level)
        promise

let wait_for_identity node =
  match node.status with
  | Running {identity = Known identity; _} ->
      return identity
  | Not_running | Running _ ->
      let (promise, resolver) = Lwt.task () in
      node.pending_identity <- resolver :: node.pending_identity ;
      check_event node "read_identity" promise

let wait_for ?where node name filter =
  let (promise, resolver) = Lwt.task () in
  let current_events =
    String_map.find_opt name node.pending_custom |> Option.value ~default:[]
  in
  node.pending_custom <-
    String_map.add
      name
      (Custom_event {filter; resolver} :: current_events)
      node.pending_custom ;
  check_event node name ?where promise

let on_event node handler =
  node.raw_event_handlers <- handler :: node.raw_event_handlers

let init ?name ?color ?data_dir ?event_pipe ?expected_pow ?network ?net_port
    ?rpc_port ?history_mode ?single_process ?bootstrap_threshold ?connections
    () =
  let node = create ?name ?color ?data_dir ?event_pipe () in
  let* () = identity_generate ?expected_pow node in
  let* () = config_init ?network ?net_port ?rpc_port ?history_mode node in
  run ?expected_pow ?single_process ?bootstrap_threshold ?connections node ;
  let* () = wait_for_ready node in
  return node

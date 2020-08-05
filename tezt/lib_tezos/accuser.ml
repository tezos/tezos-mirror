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

(* When an accuser is running, we store:
   - its process, so that we can terminate it for instance;
   - the event loop promise, which reads events and cleans them up when
     the node terminates;
   - some information about the state of the accuser so that users can
     query them. *)
type running_status = {
  process : Process.t;
  mutable ready : bool;
  mutable event_loop_promise : unit Lwt.t option;
}

type status = Not_running | Running of running_status

module String_map = Map.Make (String)

type custom_event =
  | Custom_event : {
      filter : JSON.t -> 'a option;
      resolver : 'a option Lwt.u;
    }
      -> custom_event

type t = {
  name : string;
  path : string;
  color : Tezt.Log.Color.t;
  mutable status : status;
  event_pipe : string;
  node_rpc_port : int;
  mutable pending_ready : unit option Lwt.u list;
  mutable pending_custom : custom_event list String_map.t;
}

let name accuser = accuser.name

let node_rpc_port accuser = accuser.node_rpc_port

let next_name = ref 1

let fresh_name () =
  let index = !next_name in
  incr next_name ;
  "accuser" ^ string_of_int index

let colors = Log.Color.[|FG.black; FG.white; FG.blue|]

let next_color = ref 0

let get_next_color () =
  let color = colors.(!next_color mod Array.length colors) in
  incr next_color ;
  Log.Color.(BG.yellow ++ color)

let create ?(path = Constant.alpha_accuser) ?name ?color ?event_pipe
    ~wait_for_node ~node =
  let* () =
    if wait_for_node then Node.wait_for_ready node else Lwt.return_unit
  in
  let name = match name with None -> fresh_name () | Some name -> name in
  let color =
    match color with None -> get_next_color () | Some color -> color
  in
  let event_pipe =
    match event_pipe with
    | None ->
        Temp.file (name ^ "-event-pipe")
    | Some file ->
        file
  in
  let node_rpc_port = Node.rpc_port node in
  Lwt.return
    {
      name;
      path;
      color;
      status = Not_running;
      event_pipe;
      node_rpc_port;
      pending_ready = [];
      pending_custom = String_map.empty;
    }

let trigger_ready accuser value =
  let pending = accuser.pending_ready in
  accuser.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let set_ready accuser =
  ( match accuser.status with
  | Not_running ->
      ()
  | Running status ->
      status.ready <- true ) ;
  trigger_ready accuser (Some ())

let handle_raw_stdout accuser line =
  match line with
  (* The accuser is ready when it communicates with a bootstrapped node. *)
  | "Accuser started." ->
      set_ready accuser
  | _ ->
      ()

let trigger_custom_events accuser name value =
  match String_map.find_opt name accuser.pending_custom with
  | None ->
      ()
  | Some events ->
      (* Trigger matching events and accumulate others in [acc]. *)
      let rec loop acc = function
        | [] ->
            accuser.pending_custom <-
              String_map.add name (List.rev acc) accuser.pending_custom
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
                    accuser.name
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

let handle_raw_event accuser line =
  let open JSON in
  let json = parse ~origin:"accuser event" line in
  let event = json |-> "fd-sink-item.v0" |-> "event" in
  (* let event = json in *)
  match as_object_opt event with
  | None | Some ([] | _ :: _ :: _) ->
      ()
  | Some [(name, value)] ->
      trigger_custom_events accuser name value ;
      ()

let run accuser =
  ( match accuser.status with
  | Not_running ->
      ()
  | Running _ ->
      Test.fail "accuser %s is already running" accuser.name ) ;
  let args =
    ["-E"; "http://localhost:" ^ string_of_int accuser.node_rpc_port; "run"]
  in
  (* Create the named pipe where the accuser will send its internal events in JSON. *)
  if Sys.file_exists accuser.event_pipe then Sys.remove accuser.event_pipe ;
  Unix.mkfifo accuser.event_pipe 0o640 ;
  (* Note: in the CI, it seems that if the accuser tries to open the
     FIFO for writing before we opened it for reading, the
     [Lwt.openfile] call (of the accuser, for writing) blocks
     forever. So we need to make sure that we open the file before we
     spawn the accuser. *)
  let* event_input = Lwt_io.(open_file ~mode:input) accuser.event_pipe in
  let process =
    Process.spawn
      ~name:accuser.name
      ~color:accuser.color
      ~env:
        [ ( "TEZOS_EVENTS_CONFIG",
            "file-descriptor-path://" ^ accuser.event_pipe ) ]
      accuser.path
      args
  in
  let running_status = {process; ready = false; event_loop_promise = None} in
  accuser.status <- Running running_status ;
  (* Make sure the accuser status is [Running], otherwise
     [event_loop_promise] would stop immediately thinking the accuser
     has been terminated. *)
  let event_loop_promise =
    let rec stdout_loop () =
      let* stdout_line = Lwt_io.read_line_opt (Process.stdout process) in
      match stdout_line with
      | Some line ->
          handle_raw_stdout accuser line ;
          stdout_loop ()
      | None -> (
        match accuser.status with
        | Not_running ->
            Lwt.return_unit
        | Running _ ->
            let* () = Lwt_unix.sleep 0.5 in
            stdout_loop () )
    in
    let rec event_loop () =
      let* line = Lwt_io.read_line_opt event_input in
      match line with
      | Some line ->
          handle_raw_event accuser line ;
          event_loop ()
      | None -> (
        match accuser.status with
        | Not_running ->
            Lwt_io.close event_input
        | Running _ ->
            let* () = Lwt_unix.sleep 0.5 in
            event_loop () )
    in
    let* () = event_loop ()
    and* () = stdout_loop ()
    and* () =
      let* _ = Process.wait process in
      (* Setting [node.status] to [Not_running] stops the event loop cleanly. *)
      accuser.status <- Not_running ;
      (* Cancel all [Ready] event listeners. *)
      trigger_ready accuser None ;
      let pending = accuser.pending_custom in
      accuser.pending_custom <- String_map.empty ;
      String_map.iter
        (fun _ ->
          List.iter (fun (Custom_event {resolver; _}) ->
              Lwt.wakeup_later resolver None))
        pending ;
      unit
    in
    unit
  in
  running_status.event_loop_promise <- Some event_loop_promise ;
  async event_loop_promise ;
  unit

exception
  Terminated_before_event of {
    accuser : string;
    event : string;
    where : string option;
  }

let () =
  Printexc.register_printer
  @@ function
  | Terminated_before_event {accuser; event; where = None} ->
      Some (sf "%s terminated before event occurred: %s" accuser event)
  | Terminated_before_event {accuser; event; where = Some where} ->
      Some
        (sf
           "%s terminated before event occurred: %s where %s"
           accuser
           event
           where)
  | _ ->
      None

let check_event ?where accuser name promise =
  let* result = promise in
  match result with
  | None ->
      raise
        (Terminated_before_event {accuser = accuser.name; event = name; where})
  | Some x ->
      return x

let wait_for_ready accuser =
  match accuser.status with
  | Running {ready = true; _} ->
      unit
  | Not_running | Running {ready = false; _} ->
      let (promise, resolver) = Lwt.task () in
      accuser.pending_ready <- resolver :: accuser.pending_ready ;
      check_event accuser "Accuser started." promise

let wait_for ?where accuser name filter =
  let (promise, resolver) = Lwt.task () in
  let current_events =
    String_map.find_opt name accuser.pending_custom |> Option.value ~default:[]
  in
  accuser.pending_custom <-
    String_map.add
      name
      (Custom_event {filter; resolver} :: current_events)
      accuser.pending_custom ;
  check_event accuser name ?where promise

let init ?path ?name ?color ?event_pipe ?(wait_for_node = true) ~node () =
  let* accuser = create ?path ?name ?color ?event_pipe ~wait_for_node ~node in
  let* () = run accuser in
  return accuser

let terminate accuser =
  match accuser.status with
  | Not_running ->
      unit
  | Running {event_loop_promise = None; _} ->
      invalid_arg
        "you cannot call Accuser.terminate before Accuser.run returns"
  | Running {process; _} ->
      Process.kill process ; unit

let restart accuser =
  let* () = terminate accuser in
  let* () = run accuser in
  wait_for_ready accuser

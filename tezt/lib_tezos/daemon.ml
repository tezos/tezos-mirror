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

(** Functor for the common parts of all Tezos daemons: node, baker,
    endorser and accuser. Handles event handling in particular. *)

module type PARAMETERS = sig
  (** Parameters of the [Daemon.Make] functor. *)

  (** Data to store whether a daemon is running or not. *)
  type persistent_state

  (** Data to store when a daemon is running. *)
  type session_state

  (** Basis for the default value for the [?name] argument of [create].

      Examples: ["node"] or ["accuser"]. *)
  val base_default_name : string

  (** Cycle of default values for the [?color] argument of [create]. *)
  val default_colors : Log.Color.t array
end

module Level = struct
  type default_level = [`Debug | `Info | `Notice]

  type level = [default_level | `Warning | `Error | `Fatal]

  let to_string = function
    | `Debug -> "debug"
    | `Info -> "info"
    | `Notice -> "notice"
    | `Warning -> "warning"
    | `Error -> "error"
    | `Fatal -> "fatal"
end

module Make (X : PARAMETERS) = struct
  exception
    Terminated_before_event of {
      daemon : string;
      event : string;
      where : string option;
    }

  let () =
    Printexc.register_printer @@ function
    | Terminated_before_event {daemon; event; where = None} ->
        Some (sf "%s terminated before event occurred: %s" daemon event)
    | Terminated_before_event {daemon; event; where = Some where} ->
        Some
          (sf
             "%s terminated before event occurred: %s where %s"
             daemon
             event
             where)
    | _ -> None

  (* When a daemon is running, we store:
     - its process, so that we can terminate it for instance;
     - the event loop promise, which reads events and cleans them up when
       the daemon terminates;
     - some information about the state of the daemon so that users can query them.

     The event loop promise is particularly important as when we terminate
     the daemon we must also wait for the event loop to finish cleaning up before
     we start the daemon again. The event loop is also responsible to set the status
     of the daemon to [Not_running], which is another reason to wait for it to
     finish before restarting a daemon. Otherwise we could have a [Not_running]
     daemon which would be actually running. *)
  type session_status = {
    process : Process.t;
    session_state : X.session_state;
    mutable event_loop_promise : unit Lwt.t option;
  }

  type status = Not_running | Running of session_status

  type event_handler =
    | Event_handler : {
        filter : JSON.t -> 'a option;
        resolver : 'a option Lwt.u;
      }
        -> event_handler

  type event = {name : string; value : JSON.t}

  type t = {
    name : string;
    color : Tezt.Log.Color.t;
    path : string;
    persistent_state : X.persistent_state;
    mutable status : status;
    event_pipe : string;
    mutable stdout_handlers : (string -> unit) list;
    mutable persistent_event_handlers : (event -> unit) list;
    mutable one_shot_event_handlers : event_handler list String_map.t;
  }

  let name daemon = daemon.name

  let terminate ?(kill = false) daemon =
    match daemon.status with
    | Not_running -> unit
    | Running {event_loop_promise = None; _} ->
        invalid_arg "you cannot call Daemon.terminate before Daemon.run returns"
    | Running {process; event_loop_promise = Some event_loop_promise; _} ->
        if kill then Process.kill process else Process.terminate process ;
        event_loop_promise

  let next_name = ref 1

  let fresh_name () =
    let index = !next_name in
    incr next_name ;
    X.base_default_name ^ string_of_int index

  let next_color = ref 0

  let get_next_color () =
    let color =
      X.default_colors.(!next_color mod Array.length X.default_colors)
    in
    incr next_color ;
    color

  let () =
    Test.declare_reset_function @@ fun () ->
    next_name := 1 ;
    next_color := 0

  let create ~path ?runner ?name ?color ?event_pipe persistent_state =
    let name = match name with None -> fresh_name () | Some name -> name in
    let color =
      match color with None -> get_next_color () | Some color -> color
    in
    let event_pipe =
      match event_pipe with
      | None -> Temp.file ?runner (name ^ "-event-pipe")
      | Some file -> file
    in
    {
      name;
      color;
      path;
      persistent_state;
      status = Not_running;
      event_pipe;
      stdout_handlers = [];
      persistent_event_handlers = [];
      one_shot_event_handlers = String_map.empty;
    }

  (** Takes the given JSON full event of the following form
      and evaluates in an event using [<name>] and
      [<value>]:

      {[{
        "fd-sink-item.v0": {
          [...]
          "event": { <name>:<value> }
        }
      }]}

      If the given JSON does not match the right structure,
      and in particular if the value of the field ["event"]
      is not a one-field object, the function evaluates in
      None. *)
  let get_event_from_full_event json =
    let event = JSON.(json |-> "fd-sink-item.v0" |-> "event") in
    match JSON.as_object_opt event with
    | None | Some ([] | _ :: _ :: _) -> None
    | Some [(name, value)] -> Some {name; value}

  let handle_raw_event daemon line =
    let json = JSON.parse ~origin:("event from " ^ daemon.name) line in
    match get_event_from_full_event json with
    | None -> ()
    | Some (raw_event : event) -> (
        let name = raw_event.name in
        List.iter
          (fun handler -> handler raw_event)
          daemon.persistent_event_handlers ;
        (* Trigger one-shot events. *)
        match String_map.find_opt name daemon.one_shot_event_handlers with
        | None -> ()
        | Some events ->
            (* Trigger matching events and accumulate others in [acc]. *)
            let rec loop acc = function
              | [] ->
                  daemon.one_shot_event_handlers <-
                    String_map.add
                      name
                      (List.rev acc)
                      daemon.one_shot_event_handlers
              | (Event_handler {filter; resolver} as head) :: tail ->
                  let acc =
                    match filter json with
                    | exception exn ->
                        Test.fail
                          "uncaught exception in filter for event %s of daemon \
                           %s: %s"
                          name
                          daemon.name
                          (Printexc.to_string exn)
                    | None -> head :: acc
                    | Some value ->
                        Lwt.wakeup_later resolver (Some value) ;
                        acc
                  in
                  loop acc tail
            in
            loop [] events)

  let run ?runner ?(on_terminate = fun _ -> unit) ?(event_level = `Info)
      ?(event_sections_levels = []) daemon session_state arguments =
    ignore (event_level : Level.default_level) ;
    (match daemon.status with
    | Not_running -> ()
    | Running _ -> Test.fail "daemon %s is already running" daemon.name) ;
    (* Create the named pipe where the daemon will send its internal events in JSON. *)
    if Runner.Sys.file_exists ?runner daemon.event_pipe then
      Runner.Sys.remove ?runner daemon.event_pipe ;
    Runner.Sys.mkfifo ?runner ~perms:0o640 daemon.event_pipe ;
    (* Note: in the CI, it seems that if the daemon tries to open the
       FIFO for writing before we opened it for reading, the
       [Lwt.openfile] call (of the daemon, for writing) blocks
       forever. So we need to make sure that we open the file before we
       spawn the daemon. *)
    let event_process =
      match runner with
      | None -> None
      | Some runner ->
          let cmd = "cat" in
          let arguments = [daemon.event_pipe] in
          let name = Filename.basename daemon.event_pipe in
          let process =
            Process.spawn ~name ~runner ~log_output:false cmd arguments
          in
          Some process
    in
    (* The input is either the local pipe or the remote pipe. *)
    let* event_input =
      match event_process with
      | None -> Lwt_io.(open_file ~mode:input) daemon.event_pipe
      | Some process -> Lwt.return @@ Process.stdout process
    in
    let env =
      let args =
        List.fold_right
          (fun (prefix, level) args ->
            sf "section-prefix=%s:%s" prefix (Level.to_string level) :: args)
          (("", (event_level :> Level.level)) :: event_sections_levels)
          []
      in
      let args_str = "?" ^ String.concat "&" (List.rev args) in
      String_map.singleton
        "TEZOS_EVENTS_CONFIG"
        ("file-descriptor-path://" ^ daemon.event_pipe ^ args_str)
    in
    let process =
      Process.spawn
        ?runner
        ~name:daemon.name
        ~color:daemon.color
        ~env
        daemon.path
        arguments
    in
    (* Make sure the daemon status is [Running], otherwise
       [event_loop_promise] would stop immediately thinking the daemon
       has been terminated. *)
    let running_status = {process; session_state; event_loop_promise = None} in
    daemon.status <- Running running_status ;
    let event_loop_promise =
      let rec event_loop () =
        let* line = Lwt_io.read_line_opt event_input in
        match line with
        | Some line ->
            handle_raw_event daemon line ;
            event_loop ()
        | None -> (
            match daemon.status with
            | Not_running -> (
                match event_process with
                | None -> Lwt_io.close event_input
                | Some process -> Lwt.return @@ Process.kill process)
            | Running _ ->
                (* It can take a little while before the pipe is opened by the daemon,
                   and before that, reading from it yields end of file for some reason. *)
                let* () = Lwt_unix.sleep 0.01 in
                event_loop ())
      in
      let rec stdout_loop () =
        let* stdout_line = Lwt_io.read_line_opt (Process.stdout process) in
        match stdout_line with
        | Some line ->
            List.iter (fun handler -> handler line) daemon.stdout_handlers ;
            stdout_loop ()
        | None -> (
            match daemon.status with
            | Not_running -> Lwt.return_unit
            | Running _ ->
                (* TODO: is the sleep necessary here? *)
                let* () = Lwt_unix.sleep 0.01 in
                stdout_loop ())
      in
      let* () = event_loop ()
      and*! () = stdout_loop ()
      and*! () =
        let* process_status = Process.wait process in
        (* Setting [daemon.status] to [Not_running] stops the event loop cleanly. *)
        daemon.status <- Not_running ;
        (* Cancel one-shot event handlers. *)
        let pending = daemon.one_shot_event_handlers in
        daemon.one_shot_event_handlers <- String_map.empty ;
        String_map.iter
          (fun _ ->
            List.iter (fun (Event_handler {resolver; _}) ->
                Lwt.wakeup_later resolver None))
          pending ;
        on_terminate process_status
      in
      unit
    in
    running_status.event_loop_promise <- Some event_loop_promise ;
    Background.register event_loop_promise ;
    unit

  let wait_for_full ?where daemon name filter =
    let (promise, resolver) = Lwt.task () in
    let current_events =
      String_map.find_opt name daemon.one_shot_event_handlers
      |> Option.value ~default:[]
    in
    daemon.one_shot_event_handlers <-
      String_map.add
        name
        (Event_handler {filter; resolver} :: current_events)
        daemon.one_shot_event_handlers ;
    let* result = promise in
    match result with
    | None ->
        raise
          (Terminated_before_event {daemon = daemon.name; event = name; where})
    | Some x -> return x

  let wait_for ?where daemon name filter =
    let filter json =
      let raw = get_event_from_full_event json in
      (* If [json] does not match the correct JSON structure, it
         will be filtered out, which will result in ignoring
         the current event.
         @see raw_event_from_event *)
      Option.bind raw (fun {value; _} -> filter value)
    in
    wait_for_full ?where daemon name filter

  let on_event daemon handler =
    daemon.persistent_event_handlers <-
      handler :: daemon.persistent_event_handlers

  let on_stdout daemon handler =
    daemon.stdout_handlers <- handler :: daemon.stdout_handlers

  let log_events daemon =
    on_event daemon @@ fun event ->
    Log.info "Received event: %s = %s" event.name (JSON.encode event.value)

  type observe_memory_consumption = Observe of (unit -> int option Lwt.t)

  let memory_consumption daemon =
    let from_command ~cmd ~args ~expect_failure r =
      let p = Process.spawn ~log_output:true cmd args in
      fun () ->
        let* output = Process.check_and_read_stdout ~expect_failure p in
        match output =~* rex r with
        | None -> Test.fail "Unable to find `%s' in process stdout" r
        | Some v -> return v
    in
    let cannot_observe = return @@ Observe (fun () -> return None) in
    match daemon.status with
    | Not_running -> cannot_observe
    | Running {process; _} -> (
        let* perf = Process.program_path "perf" in
        let* heaptrack_print = Process.program_path "heaptrack_print" in
        match (perf, heaptrack_print) with
        | (None, _) | (_, None) -> cannot_observe
        | (Some perf, Some heaptrack_print) -> (
            try
              let pid = Process.pid process |> string_of_int in
              let get_trace =
                from_command
                  ~cmd:perf
                  ~args:["stat"; "-r"; "5"; "heaptrack"; "-p"; pid]
                  ~expect_failure:true
                  ".* heaptrack --analyze \"(.*)\""
              in
              return
              @@ Observe
                   (fun () ->
                     let* dump = get_trace () in
                     let* peak =
                       from_command
                         ~cmd:heaptrack_print
                         ~args:[dump]
                         ~expect_failure:false
                         "peak heap memory consumption: (\\d+\\.?\\d*\\w)"
                         ()
                     in
                     match peak =~** rex "(\\d+\\.?\\d*)(\\w)" with
                     | None ->
                         Test.fail
                           "Invalid memory consumption format: %s\n"
                           peak
                     | Some (size, unit) ->
                         let factor_of_unit =
                           match unit with
                           | "K" -> 1024
                           | "M" -> 1024 * 1024
                           | "G" -> 1024 * 1024 * 1024
                           | _ -> 1
                         in
                         let size =
                           int_of_float
                           @@ float_of_string size
                              *. float_of_int factor_of_unit
                         in
                         return @@ Some size)
            with exn ->
              Test.fail
                "failed to set up memory consumption measurement: %s"
                (Printexc.to_string exn)))
end

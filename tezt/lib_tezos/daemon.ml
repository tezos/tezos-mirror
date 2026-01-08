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

(** Functor for the common parts of all Tezos daemons: node, baker and accuser.
    Handles event handling in particular. *)

module type PARAMETERS = sig
  type persistent_state

  type session_state

  val base_default_name : string

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

  type session_status = {
    process : Process.t;
    stdin : Lwt_io.output_channel;
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

  type event = {name : string; value : JSON.t; timestamp : float}

  type t = {
    name : string;
    color : Tezt.Log.Color.t;
    path : string;
    persistent_state : X.persistent_state;
    mutable status : status;
    event_pipe : string;
    mutable stdout_handlers : (string -> unit) list;
    mutable stderr_handlers : (string -> unit) list;
    mutable persistent_event_handlers : (event -> unit) list;
    mutable one_shot_event_handlers : event_handler list String_map.t;
  }

  let name daemon = daemon.name

  let pid daemon =
    match daemon.status with
    | Running status -> Process.pid status.process |> Option.some
    | Not_running -> None

  let path daemon = daemon.path

  (* Having to wait more that 3 seconds after hitting Ctrl+C is already unreasonable.
     We choose a timeout one order of magnitude larger to reduce flakiness in case
     the CPU happens to be slower etc. *)
  let terminate ?(timeout = 30.) daemon =
    match daemon.status with
    | Not_running -> unit
    | Running {event_loop_promise = None; _} ->
        invalid_arg "you cannot call Daemon.terminate before Daemon.run returns"
    | Running {process; event_loop_promise = Some event_loop_promise; _} ->
        Process.terminate ~timeout process ;
        event_loop_promise

  let stop daemon =
    match daemon.status with
    | Not_running -> unit
    | Running {event_loop_promise = None; _} ->
        invalid_arg "you cannot call Daemon.stop before Daemon.run returns"
    | Running {process; _} ->
        let pid = Process.pid process in
        Unix.kill pid Sys.sigstop ;
        unit

  let continue daemon =
    match daemon.status with
    | Not_running -> unit
    | Running {event_loop_promise = None; _} ->
        invalid_arg "you cannot call Daemon.continue before Daemon.run returns"
    | Running {process; _} ->
        let pid = Process.pid process in
        Unix.kill pid Sys.sigcont ;
        unit

  let kill daemon =
    match daemon.status with
    | Not_running -> unit
    | Running {event_loop_promise = None; _} ->
        invalid_arg "you cannot call Daemon.terminate before Daemon.run returns"
    | Running {process; event_loop_promise = Some event_loop_promise; _} ->
        Process.kill process ;
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
      stderr_handlers = [];
      persistent_event_handlers = [];
      one_shot_event_handlers = String_map.empty;
    }

  (** Takes the given JSON full event of the following form
      and evaluates in an event using [<name>] and
      [<value>]:

      {[{
        "fd-sink-item.v0": {
          "time_stamp":<timestamp>
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
    let timestamp =
      JSON.(json |-> "fd-sink-item.v0" |-> "time_stamp" |> as_float)
    in
    match JSON.as_object_opt event with
    | None | Some ([] | _ :: _ :: _) -> None
    | Some [(name, value)] -> Some {name; value; timestamp}

  let read_json_event daemon event_input =
    let max_event_size =
      1024 * 1024
      (* 1MB *)
    in
    let origin = "event from " ^ daemon.name in
    let buff = Buffer.create 256 in
    let rec loop () =
      let* line = Lwt_io.read_line_opt event_input in
      match line with
      | None -> return []
      | Some line -> (
          (* If a line alone is a valid JSON, then it is not stacked in the
              buffer and rather treated as an event.
              This is great to get rid of concurrency issue due to a small event
              being nested in a very big one, but it is not an ideal solution
              since it suffers several drawbacks:
              - It does not work if a several line event is nested into another one.
              - Many subterms of a valid json are valid json, hence if we are
                unlucky and cut a big json at unexpected position, we might parse
                a subterm and not stake it, preventing definitely the whole term
                to be seen as a valid json.
          *)
          match JSON.parse_opt ~origin line with
          | Some json ->
              if Buffer.length buff = 0 then return [json]
              else
                let* l = loop () in
                return (json :: l)
          | None -> (
              Buffer.add_string buff line ;
              match JSON.parse_opt ~origin (Buffer.contents buff) with
              | None when Buffer.length buff >= max_event_size ->
                  Format.ksprintf
                    failwith
                    "Could not parse daemon %s event after %d bytes.\n\
                     buffer: %s\n\
                     line:%s"
                    daemon.name
                    max_event_size
                    (Buffer.contents buff)
                    line
              | None -> loop ()
              | Some json -> return [json]))
    in
    loop ()

  let handle_raw_event daemon json =
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

  let run ?(env = String_map.empty) ?runner ?(on_terminate = fun _ -> unit)
      ?(event_level = `Info) ?(event_sections_levels = [])
      ?(capture_stderr = false) daemon session_state arguments =
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
          (* If the daemon shuts down, the command will fail. A proposition is
             to use "tail --retry --folow" instead.

             However, it seems that "tail" can be blocking.

             Probably a solution is to catch the error when the daemons shuts
             down and to rerun it once it is restarted.
          *)
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
      String_map.add
        "TEZOS_EVENTS_CONFIG"
        ("file-descriptor-path://" ^ daemon.event_pipe ^ args_str)
        env
    in
    let process, stdin =
      Process.spawn_with_stdin
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
    let running_status =
      {process; session_state; stdin; event_loop_promise = None}
    in
    daemon.status <- Running running_status ;
    let event_loop_promise =
      let rec event_loop () =
        let* json = read_json_event daemon event_input in
        match json with
        | _ :: _ as json_list ->
            List.iter (handle_raw_event daemon) json_list ;
            event_loop ()
        | [] -> (
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
      let rec channel_loop get_channel get_handlers () =
        let* channel_line = Lwt_io.read_line_opt (get_channel process) in
        match channel_line with
        | Some line ->
            List.iter (fun handler -> handler line) (get_handlers daemon) ;
            channel_loop get_channel get_handlers ()
        | None -> (
            match daemon.status with
            | Not_running -> Lwt.return_unit
            | Running _ ->
                (* TODO: is the sleep necessary here? *)
                let* () = Lwt_unix.sleep 0.01 in
                channel_loop get_channel get_handlers ())
      in
      let ( and*!! ) = lwt_both_fail_early in
      let* () = event_loop ()
      and*!! () =
        channel_loop Process.stdout (fun daemon -> daemon.stdout_handlers) ()
      and*!! () =
        if capture_stderr then
          channel_loop Process.stderr (fun daemon -> daemon.stderr_handlers) ()
        else unit
      and*!! () =
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
    let promise, resolver = Lwt.task () in
    let current_events =
      String_map.find_opt name daemon.one_shot_event_handlers
      |> Option.value ~default:[]
    in
    Log.debug
      "Waiting for event [%s]%s"
      name
      (match where with None -> "" | Some where -> " where " ^ where) ;
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

  let event_from_full_event_filter filter json =
    let raw = get_event_from_full_event json in
    (* If [json] does not match the correct JSON structure, it
       will be filtered out, which will result in ignoring
       the current event.
       @see raw_event_from_event *)
    Option.bind raw (fun {value; _} -> filter value)

  let wait_for ?where daemon name filter =
    wait_for_full ?where daemon name (event_from_full_event_filter filter)

  let on_event daemon handler =
    daemon.persistent_event_handlers <-
      handler :: daemon.persistent_event_handlers

  let on_stdout daemon handler =
    daemon.stdout_handlers <- handler :: daemon.stdout_handlers

  let on_stderr daemon handler =
    daemon.stderr_handlers <- handler :: daemon.stderr_handlers

  let log_events ?max_length daemon =
    let truncate s max_length =
      match max_length with
      | Some max_length when String.length s > max_length ->
          String.sub s 0 max_length ^ "[...]"
      | _ -> s
    in
    on_event daemon @@ fun event ->
    Log.info
      ~color:daemon.color
      "[%s] Received event: %s = %s"
      daemon.name
      event.name
      (truncate (JSON.encode event.value) max_length)

  type observe_memory_consumption = Observe of (unit -> int option Lwt.t)

  let memory_consumption daemon =
    let from_command ~cmd ~args ~expect_failure r =
      let p = Process.spawn ~log_output:true cmd args in
      fun () ->
        let* output = Process.check_and_read_stdout ~expect_failure p in
        return (output =~* rex r)
    in
    let cannot_observe = return @@ Observe (fun () -> return None) in
    match daemon.status with
    | Not_running -> cannot_observe
    | Running {process; _} -> (
        let* perf = Process.program_path "perf" in
        let* heaptrack_print = Process.program_path "heaptrack_print" in
        match (perf, heaptrack_print) with
        | None, _ | _, None -> cannot_observe
        | Some perf, Some heaptrack_print -> (
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
                     Process.kill process ;
                     let* dump = get_trace () in
                     match dump with
                     | None ->
                         (*
                            [perf] may fail if [kernel.perf_event_paranoid] is set to
                            a permissive enough value. In this case, we cannot observe
                            memory consumption. We do not consider this situation as an
                            error because that's a too strong requirement on CI workers.
                         *)
                         Log.warn
                           "kernel.perf_event_paranoid is not permissive \
                            enough. Aborting memory observation." ;
                         return None
                     | Some dump -> (
                         let* peak =
                           from_command
                             ~cmd:heaptrack_print
                             ~args:[dump]
                             ~expect_failure:false
                             "peak heap memory consumption: (\\d+\\.?\\d*\\w)"
                             ()
                         in
                         match
                           Option.get peak =~** rex "(\\d+\\.?\\d*)(\\w)"
                         with
                         | None ->
                             Test.fail
                               "Invalid memory consumption format: %s\n"
                               (match peak with
                               | None -> "(empty)"
                               | Some s -> s)
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
                             return @@ Some size))
            with exn ->
              Test.fail
                "failed to set up memory consumption measurement: %s"
                (Printexc.to_string exn)))
end

let n_events_rev n filter =
  if n <= 0 then invalid_arg "Base.n_events_rev: n must be > 0." ;
  let acc = ref [] in
  let size = ref 0 in
  let accumulation_threshold value =
    acc := value :: !acc ;
    incr size ;
    if !size >= n then Some !acc else None
  in
  let accumulating_filter json =
    Option.bind (filter json) accumulation_threshold
  in
  accumulating_filter

let n_events n filter =
  let accumulating_filter = n_events_rev n filter in
  let inverting_filter json = Option.map List.rev @@ accumulating_filter json in
  inverting_filter

let nth_event n filter =
  let accumulating_filter = n_events_rev n filter in
  let nth_filter json = Option.map List.hd @@ accumulating_filter json in
  nth_filter

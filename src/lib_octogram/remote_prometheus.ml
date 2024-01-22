(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Parameters = struct
  type persistent_state = {
    name : Agent_name.t;
    local_octogram_path : string;
    mutable pending_ready : unit option Lwt.u list;
    runner : Runner.t;
  }

  type session_state = {mutable ready : bool}

  let base_default_name = "octogram-prometheus"

  let default_colors = Log.Color.[|FG.magenta|]
end

open Parameters
include Daemon.Make (Parameters)

let wait agent =
  match agent.status with
  | Not_running ->
      Test.fail
        "%s is not running, cannot wait for it to terminate"
        (name agent)
  | Running {process; _} -> Process.wait process

let wait_for_promise ?timeout ?where agent name promise =
  let promise = Lwt.map Result.ok promise in
  let* result =
    match timeout with
    | None -> promise
    | Some timeout ->
        Lwt.pick
          [
            promise;
            (let* () = Lwt_unix.sleep timeout in
             Lwt.return_error ());
          ]
  in
  match result with
  | Ok (Some x) -> return x
  | Ok None ->
      raise (Terminated_before_event {daemon = agent.name; event = name; where})
  | Error () ->
      Format.ksprintf
        failwith
        "Timeout waiting for event %s of %s"
        name
        agent.name

let trigger_ready agent value =
  let pending = agent.persistent_state.pending_ready in
  agent.persistent_state.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let set_ready agent =
  (match agent.status with
  | Not_running -> ()
  | Running status -> status.session_state.ready <- true) ;
  trigger_ready agent (Some ())

let wait_for_ready agent =
  match agent.status with
  | Running {session_state = {ready = true; _}; _} -> unit
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      let promise, resolver = Lwt.task () in
      agent.persistent_state.pending_ready <-
        resolver :: agent.persistent_state.pending_ready ;
      wait_for_promise agent "Prometheus is ready" promise

let handle_raw_stdout agent line =
  if line =~ rex Prometheus_agent.greetings then set_ready agent

let on_stdout agent handler =
  agent.stdout_handlers <- handler :: agent.stdout_handlers

let create ~runner ~(name : Agent_name.t) ?path ?color ?event_pipe () =
  let local_octogram_path = Option.value ~default:"./octogram" path in
  let path = "/tmp" // sf "octogram-%s" (name :> string) in
  let agent =
    create
      ~path
      ~name:(name :> string)
      ~runner
      ?color
      ?event_pipe
      {pending_ready = []; local_octogram_path; runner; name}
  in
  on_stdout agent (handle_raw_stdout agent) ;
  agent

let run ?event_level ?event_sections_levels agent =
  if agent.status <> Not_running then Test.fail "Remote agent is not running" ;

  let on_terminate _ =
    trigger_ready agent None ;
    unit
  in

  let* () =
    Helpers.deploy
      ~r:true
      ~for_runner:agent.persistent_state.runner
      agent.persistent_state.local_octogram_path
      agent.path
  in

  (Background.register
  @@ let* () = wait_for_ready agent in
     Log.debug "Agent is ready, removing the binary" ;
     Helpers.exec ~runner:agent.persistent_state.runner "rm" [agent.path]) ;

  run
    ~runner:agent.persistent_state.runner
    ?event_level
    ?event_sections_levels
    agent
    {ready = false}
    ~on_terminate
    [
      Format.(
        sprintf "--%s" @@ Cli.get_string ~default:"info" "remote_verbosity");
      "prometheus";
      "-a";
      sf "agent_name=%s" (agent.persistent_state.name :> string);
    ]

let with_session_status state k =
  match state.status with
  | Running status -> k status
  | _ -> Test.fail "Remote prometheus is not running"

let record_metrics_source agent source =
  with_session_status agent @@ fun status ->
  let str =
    Helpers.to_json_string Prometheus_agent.input_encoding (Record source)
  in
  Log.debug "%s" str ;
  Lwt_io.write status.stdin Format.(sprintf "%s\n%!" str)

let quit agent =
  with_session_status agent @@ fun status ->
  let str = Helpers.to_json_string Prometheus_agent.input_encoding Quit in
  Log.debug "%s" str ;
  Lwt_io.write status.stdin Format.(sprintf "%s\n%!" str)

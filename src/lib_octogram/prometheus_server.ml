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

type metrics_source = {job_name : string; target : string}

let metrics_source_encoding =
  Data_encoding.(
    conv
      (fun {job_name; target} -> (job_name, target))
      (fun (job_name, target) -> {job_name; target})
      (obj2 (req "job_name" string) (req "target" string)))

let config_from_metrics_sources sources =
  Format.asprintf
    {|global:
  scrape_interval: 15s
scrape_configs:
%a|}
    Format.(
      pp_print_list (fun fmt {job_name; target} ->
          fprintf
            fmt
            {|  - job_name: '%s'
    scrape_interval: 7s
    metrics_path: '/metrics'
    static_configs:
      - targets: ['%s']|}
            job_name
            target))
    sources

module Parameters = struct
  type persistent_state = {
    mutable pending_ready : unit option Lwt.u list;
    web_listen_port : int;
  }

  type session_state = {
    mutable ready : bool;
    config_file : string;
    data_dir : string;
    mutable metrics_sources : metrics_source list;
  }

  let base_default_name = "prometheus"

  let default_colors = Log.Color.[|FG.red|]
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
      wait_for_promise agent "Orchestrator is ready" promise

let handle_raw_stderr agent line =
  if line =~ rex {|Server is ready to receive web requests.|} then
    set_ready agent

let create ?path ?color ?event_pipe ?web_listen_port () =
  let path = Option.value ~default:"prometheus" path in
  let web_listen_port = Option.value ~default:9090 web_listen_port in
  let agent =
    create ~path ?color ?event_pipe {pending_ready = []; web_listen_port}
  in
  on_stderr agent (handle_raw_stderr agent) ;
  agent

let run ?(kill_running_instances = false) ?(on_terminate = fun _ -> ())
    ?event_level ?event_sections_levels agent =
  if agent.status <> Not_running then
    Test.fail "Prometheus instance %s is already running" agent.name ;

  let on_terminate status =
    on_terminate status ;
    trigger_ready agent None ;
    unit
  in

  let* () =
    Helpers.when_
      kill_running_instances
      (Helpers.exec ~can_fail:true "pkill" ["-9"; "prometheus"])
  in

  let metrics_sources = [] in
  let config_file = Temp.file "prometheus.yml" in
  let data_dir = Temp.dir "data" in

  write_file config_file ~contents:(config_from_metrics_sources metrics_sources) ;

  run
    ?event_level
    ?event_sections_levels
    ~on_terminate
    ~capture_stderr:true
    agent
    {ready = false; metrics_sources; config_file; data_dir}
    [
      sf "--config.file=%s" config_file;
      "--web.enable-admin-api";
      "--web.enable-lifecycle";
      sf {|--storage.tsdb.path=%s|} data_dir;
      sf
        {|--web.listen-address=0.0.0.0:%d|}
        agent.persistent_state.web_listen_port;
    ]

let with_session_status agent k =
  match agent.status with
  | Running r -> k r
  | _ -> Test.fail "Prometheus instance %s is not running" agent.name

let post ?(body = `String "") agent route =
  Runnable.run
  @@ Curl.post_raw
       ~args:["-g"]
       (sf
          "http://localhost:%d/%s"
          agent.persistent_state.web_listen_port
          route)
  @@ JSON.annotate ~origin:"promotheus_request" body

let reload_config agent =
  let* _json = post agent "-/reload" in
  unit

let record_metrics_source agent source =
  with_session_status agent @@ fun status ->
  status.session_state.metrics_sources <-
    source :: status.session_state.metrics_sources ;
  write_file
    status.session_state.config_file
    ~contents:(config_from_metrics_sources status.session_state.metrics_sources) ;
  reload_config agent

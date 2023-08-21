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

type octogram_binary =
  | Push of {local_path : string}
  | Pull of {url : string}
  | Already_installed of {bin : string}

let octogram_binary_encoding =
  let c = Helpers.make_mk_case () in
  Data_encoding.(
    union
      [
        c.mk_case
          "push"
          (obj1 (req "push" string))
          (function Push {local_path} -> Some local_path | _ -> None)
          (fun local_path -> Push {local_path});
        c.mk_case
          "pull"
          (obj1 (req "pull" string))
          (function Pull {url} -> Some url | _ -> None)
          (fun url -> Pull {url});
        c.mk_case
          "installed"
          (obj1 (dft "installed" string "octogram"))
          (function Already_installed {bin} -> Some bin | _ -> None)
          (fun bin -> Already_installed {bin});
      ])

let ( // ) = Filename.concat

module IMap = Map.Make (Int)

type request = Pending of unit option Lwt.u list | Answered of string

type 'a request_handler = {
  proc_id : int;
  procedure : ('a, Uri.agent_uri) Remote_procedure.t;
}

module Parameters = struct
  type persistent_state = {
    name : Agent_name.t;
    mutable remote_home_dir : string;
    octogram_binary : octogram_binary;
    mutable pending_ready : unit option Lwt.u list;
    mutable pending_requests : request IMap.t;
    mutable services_cache : Services_cache.t;
    on_new_metrics_source :
      Agent_name.t -> string -> Services_cache.node_kind -> int -> unit;
    runner : Runner.t;
  }

  type session_state = {mutable ready : bool; mutable next_command_id : int}

  let base_default_name = "octogram-agent"

  let default_colors = Log.Color.[|FG.yellow; FG.magenta; FG.green; FG.gray|]
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

let name agent = agent.persistent_state.name

let color agent = agent.color

let runner agent = agent.persistent_state.runner

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

let trigger_request_completion agent i value =
  match (value, IMap.find_opt i agent.persistent_state.pending_requests) with
  | _, None -> Test.fail "Unknown command id %d" i
  | _, Some (Answered _) -> Test.fail "Already completed command %d" i
  | Some v, Some (Pending pending) ->
      agent.persistent_state.pending_requests <-
        IMap.add i (Answered v) agent.persistent_state.pending_requests ;
      List.iter (fun pending -> Lwt.wakeup_later pending (Some ())) pending
  | None, Some (Pending pending) ->
      agent.persistent_state.pending_requests <-
        IMap.remove i agent.persistent_state.pending_requests ;
      List.iter (fun pending -> Lwt.wakeup_later pending None) pending

let record_request_completion agent (i, v) =
  trigger_request_completion agent i (Some v)

let rec wait_for_request_raw agent i =
  let pending =
    match IMap.find_opt i agent.persistent_state.pending_requests with
    | None -> `Pending []
    | Some (Pending pending) -> `Pending pending
    | Some (Answered v) -> `Result v
  in
  match pending with
  | `Result v ->
      agent.persistent_state.pending_requests <-
        IMap.remove i agent.persistent_state.pending_requests ;
      return v
  | `Pending pending ->
      let promise, resolver = Lwt.task () in
      agent.persistent_state.pending_requests <-
        IMap.add
          i
          (Pending (resolver :: pending))
          agent.persistent_state.pending_requests ;
      let* () = wait_for_promise agent "Command completed" promise in
      wait_for_request_raw agent i

let start_request agent cmd =
  match agent.status with
  | Running {stdin; session_state = session; _} ->
      let i = session.next_command_id in
      session.next_command_id <- i + 1 ;
      let req = Agent.{proc_id = i; procedure = Packed cmd} in
      let req_str = Helpers.to_json_string Agent.request_encoding req in
      Log.debug "%s" req_str ;
      let* () = Lwt_io.write stdin Format.(sprintf "%s\n%!" req_str) in
      return {proc_id = i; procedure = cmd}
  | Not_running ->
      Test.fail "Cannot send a command: %s not started" (name agent :> string)

let update_services_cache agent name node_kind service_kind port =
  agent.persistent_state.services_cache <-
    Services_cache.add
      agent.persistent_state.services_cache
      name
      node_kind
      [(service_kind, port)]

let wait_for_request agent {proc_id; procedure = req} =
  let* str = wait_for_request_raw agent proc_id in
  let res =
    Helpers.of_json_string (Remote_procedure.response_encoding req) str
  in
  Remote_procedure.on_completion
    ~on_new_service:(update_services_cache agent)
    ~on_new_metrics_source:
      (agent.persistent_state.on_new_metrics_source agent.persistent_state.name)
    req
    res ;
  return res

let rex_log cmd =
  rex @@ Format.sprintf "^\\[\\d\\d:\\d\\d:\\d\\d.\\d\\d\\d\\] %s$" cmd

let handle_raw_stdout agent line =
  match line =~* rex_log {|Ready (.*)\.\.\.|} with
  | Some remote_home_dir ->
      agent.persistent_state.remote_home_dir <- remote_home_dir ;
      set_ready agent
  | None -> (
      match line =~** rex {|^<<\[(\d+)\]: (.*)$|} with
      | Some (id_str, json_str) ->
          let i = int_of_string id_str in
          record_request_completion agent (i, json_str)
      | None -> ())

let on_stdout agent handler =
  agent.stdout_handlers <- handler :: agent.stdout_handlers

let scope_in base_dir path =
  if Filename.is_relative path then base_dir // path else path

let create ~runner ~(name : Agent_name.t) ~on_new_metrics_source
    ~octogram_binary ?color ?event_pipe () =
  let path =
    match octogram_binary with
    | Already_installed {bin} -> bin
    | _ -> "/tmp" // sf "octogram-%s" (name :> string)
  in
  let agent =
    create
      ~path
      ~name:(name :> string)
      ~runner
      ?color
      ?event_pipe
      {
        name;
        pending_ready = [];
        pending_requests = IMap.empty;
        services_cache = Services_cache.empty;
        runner;
        remote_home_dir = "";
        octogram_binary;
        on_new_metrics_source;
      }
  in
  on_stdout agent (handle_raw_stdout agent) ;
  agent

let scope agent path = scope_in agent.persistent_state.remote_home_dir path

let run ?on_terminate ?event_level ?event_sections_levels agent =
  if agent.status <> Not_running then
    Test.fail "Smart contract rollup node %s is already running" agent.name ;

  let on_terminate status =
    trigger_ready agent None ;
    IMap.iter
      (fun i v ->
        match v with
        | Pending _ -> trigger_request_completion agent i None
        | _ -> ())
      agent.persistent_state.pending_requests ;
    match on_terminate with
    | Some on_terminate -> on_terminate status
    | None -> unit
  in

  let* () =
    match agent.persistent_state.octogram_binary with
    | Push {local_path} ->
        let* () =
          Helpers.deploy
            ~r:true
            ~for_runner:agent.persistent_state.runner
            local_path
            agent.path
        in
        Helpers.exec
          ~runner:agent.persistent_state.runner
          "chmod"
          ["+x"; agent.path]
    | Pull {url} ->
        let* _ =
          Helpers.download
            ~dir:(Filename.dirname agent.path)
            ~runner:agent.persistent_state.runner
            url
            (Filename.basename agent.path)
        in
        Helpers.exec
          ~runner:agent.persistent_state.runner
          "chmod"
          ["+x"; agent.path]
    | Already_installed _ -> unit
  in

  (match agent.persistent_state.octogram_binary with
  | Push _ | Pull _ ->
      Background.register
      @@ let* () = wait_for_ready agent in
         Helpers.exec
           ~can_fail:true
           ~runner:agent.persistent_state.runner
           "rm"
           [agent.path]
  | _ -> ()) ;

  run
    ~runner:agent.persistent_state.runner
    ?event_level
    ?event_sections_levels
    agent
    {ready = false; next_command_id = 0}
    ~on_terminate
    [
      Format.(
        sprintf "--%s" @@ Cli.get_string ~default:"info" "remote_verbosity");
      "agent";
      "-a";
      sf "agent_name=%s" (agent.persistent_state.name :> string);
    ]

let get_service_info node_kind service_kind agent node_name =
  Services_cache.get
    agent.persistent_state.services_cache
    node_name
    node_kind
    service_kind

let to_tvalue agent =
  let open Jingoo.Jg_types in
  Tobj [("name", Tstr (name agent :> string))]

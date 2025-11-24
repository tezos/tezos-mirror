(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Types

type service = {name : string; url : string}

type t = {
  process : unit Lwt.t;
  to_stop : unit Lwt.u;
  dir : string;
  monitoring : bool;
  prometheus : bool;
  opentelemetry : bool;
  mutable services : service list;
}

let pp_docker_image fmt = function
  | Agent.Configuration.Gcp {alias} -> Format.fprintf fmt "%s" alias
  | Octez_release {tag} -> Format.fprintf fmt "Octez %s release" tag

let domain agents =
  match Env.mode with
  | `Remote_orchestrator_local_agents | `Ssh_host _ ->
      Proxy.get_agent agents |> Agent.point |> Option.get |> fst
  | `Remote_orchestrator_remote_agents | `Local_orchestrator_local_agents
  | `Local_orchestrator_remote_agents ->
      "localhost"

let string_docker_command agent =
  match Agent.runner agent with
  | None -> "<no command, docker is host>"
  | Some runner ->
      let point = Agent.point agent |> Option.get in
      let ssh_id = runner.Runner.ssh_id in
      String.concat
        " "
        ([
           "ssh";
           Format.asprintf "root@%s" (fst point);
           "-p";
           string_of_int (snd point);
           "-i";
           ssh_id |> Option.get;
         ]
        @ runner.options)

let string_vm_command agent =
  match Agent.cmd_wrapper agent with
  | None -> "# Just run the command, the VM is the host machine"
  | Some cmd_wrapper ->
      String.concat " " (cmd_wrapper.Gcloud.cmd :: cmd_wrapper.args)

let monitored_binaries agent =
  match Agent.process_monitor agent with
  | None -> []
  | Some process_monitor ->
      let binaries = Process_monitor.get_binaries process_monitor in
      let binaries =
        List.sort (fun (g1, _) (g2, _) -> String.compare g1 g2) binaries
      in
      let binaries =
        let open Jingoo.Jg_types in
        List.fold_left
          (fun res (group, name) ->
            Tobj [("group", Tstr group); ("name", Tstr name)] :: res)
          []
          binaries
      in
      binaries

let agent_jingo_template agent =
  let open Jingoo.Jg_types in
  let Agent.Configuration.
        {
          vm =
            {
              machine_type;
              disk_type;
              disk_size_gb;
              docker_image;
              dockerbuild_args = _;
              max_run_duration;
              binaries_path;
              os;
            };
          name;
        } =
    Agent.configuration agent
  in
  Tobj
    ([
       ("name", Tstr name);
       ("machine_type", Tstr machine_type);
       ("docker_image", Tstr (Format.asprintf "%a" pp_docker_image docker_image));
       ( "max_run_duration",
         Tstr
           (Option.fold
              ~none:"no limit"
              ~some:(fun time -> string_of_int time ^ "s")
              max_run_duration) );
       ("binaries_path", Tstr binaries_path);
       ("os", Tstr (Os.to_string os));
       ("vm_command", Tstr (string_vm_command agent));
       ("docker_command", Tstr (string_docker_command agent));
       ("monitored_binaries", Tlist (monitored_binaries agent));
     ]
    @ (match disk_type with None -> [] | Some d -> [("disk_type", Tstr d)])
    @
    match disk_size_gb with None -> [] | Some s -> [("disk_size_gb", Tint s)])

let monitoring_jingo_template agents agent =
  let open Jingoo.Jg_types in
  let host =
    if Env.mode = `Local_orchestrator_local_agents then "localhost"
    else
      match Agent.point agent with
      | Some (host, _port) -> host
      | None -> domain agents
  in
  let vm_name = Option.value ~default:"None" (Agent.vm_name agent) in
  Tobj
    [
      ("name", Tstr (Agent.name agent));
      ("vm_name", Tstr vm_name);
      ("uri", Tstr (sf "http://%s:19999" host));
    ]

let service_jingo_template {name; url} =
  let open Jingoo.Jg_types in
  Tobj [("title", Tstr (String.capitalize_ascii name)); ("uri", Tstr url)]

let jingoo_template t agents =
  let open Jingoo.Jg_types in
  [
    ( "grafana",
      Tobj
        [
          ("activated", Tbool Env.grafana);
          ("uri", Tstr (Format.asprintf "http://%s:3000" (domain agents)));
        ] );
    ( "prometheus",
      Tobj
        [
          ("activated", Tbool Env.prometheus);
          ( "uri",
            Tstr
              (Format.asprintf
                 "http://%s:%d"
                 (domain agents)
                 Env.prometheus_port) );
        ] );
    ( "monitoring",
      Tlist
        (if Env.monitoring then
           List.map (monitoring_jingo_template agents) agents
         else []) );
    ( "opentelemetry",
      Tobj
        [
          ("activated", Tbool Env.open_telemetry);
          ("uri", Tstr (Format.asprintf "http://%s:16686" (domain agents)));
        ] );
    ("agents", Tlist (List.map agent_jingo_template agents));
    ("services", Tlist (List.map service_jingo_template t.services));
  ]

let index dir = dir // "index.html"

let write t ~agents =
  (* The content is formatted in markdown and will be rendered in html via
     pandoc. *)
  let content =
    Jingoo.Jg_template.from_file
      Path.website_index
      ~models:(jingoo_template t agents)
  in
  let dir = t.dir in
  let index = index dir in
  Base.with_open_out index (fun oc -> output_string oc content) ;
  let website_style = Path.website_style in
  Log.info "Website: write" ;
  Process.run "cp" [website_style; dir // "style.css"]

let add_service t ~agents service =
  t.services <- service :: t.services ;
  write t ~agents

let run () =
  (* We do not use the Temp.dir so that the base directory is predictable and
     can be mounted by the proxy VM if [--proxy] is used. *)
  let dir = Path.tmp_dir // "website" in
  let* () = Process.run "mkdir" ["-p"; dir] in
  let index = index dir in
  let port = Env.website_port in
  let prometheus = Env.prometheus in
  let monitoring = Env.monitoring in
  let opentelemetry = Env.open_telemetry in
  let stop, to_stop = Lwt.task () in
  let logger next_handler request =
    let meth = Dream.method_to_string (Dream.method_ request) in
    let target = Dream.target request in
    let req = Dream.client request in
    let fd_field =
      Dream.new_field ~name:"dream.fd" ~show_value:string_of_int ()
    in
    let fd_string =
      match Dream.field request fd_field with
      | None -> ""
      | Some fd -> " fd " ^ string_of_int fd
    in
    let user_agent = Dream.headers request "User-Agent" |> String.concat " " in
    Log.info
      ~color:Log.Color.FG.blue
      ~prefix:"dream"
      "%s %s %s%s %s"
      meth
      target
      req
      fd_string
      user_agent ;
    next_handler request
  in
  let process =
    Dream.serve ~stop ~port ~tls:false ~interface:"0.0.0.0"
    (* @@ Dream.logger *)
    @@ logger
    @@ Dream.router
         [
           Dream.get "/metrics" (fun _ ->
               let file = dir // "metrics.txt" in
               let content =
                 if Sys.file_exists file then read_file file else ""
               in
               Log.report "[WEB] GET /metrics" ;
               let response = Dream.response content in
               Dream.add_header
                 response
                 "Content-type"
                 "text/plain; version=0.0.4; charset=utf-8" ;
               Lwt.return response);
           Dream.get "/style.css" (fun _ ->
               let content = read_file (dir // "style.css") in
               Dream.respond content);
           Dream.get "/" (fun _ ->
               let content = read_file index in
               Dream.html content);
         ]
  in
  Lwt.return
    {
      process;
      to_stop;
      dir;
      monitoring;
      prometheus;
      opentelemetry;
      services = [];
    }

let start ~agents =
  let* t = run () in
  let* () = write t ~agents in
  Lwt.return t

let shutdown t =
  Log.info "Shutting down the website..." ;
  Lwt.wakeup t.to_stop () ;
  (* Do not fail if something happened during shutdown. *)
  let* () = t.process in
  Lwt.return_unit

let push_metric =
  let table = Hashtbl.create 11 in
  fun t ?help ?typ ?(labels = []) ~name value ->
    let i = Unix.gettimeofday () in
    let labels_str =
      let inner =
        labels
        |> List.map (fun (name, value) ->
               Format.asprintf "%s = \"%s\"" name value)
        |> String.concat ","
      in
      match labels with [] -> "" | _ -> Format.asprintf "{ %s }" inner
    in
    let help_str =
      match help with
      | Some help -> Format.asprintf "# HELP %s %s\n" name help
      | None -> ""
    in
    let typ_str =
      match typ with
      | Some `Counter -> Format.asprintf "# TYPE %s counter\n" name
      | Some `Gauge -> Format.asprintf "# TYPE %s gauge\n" name
      | None -> ""
    in
    let str =
      Format.asprintf
        "%s%s%s %s %f %d"
        help_str
        typ_str
        name
        labels_str
        value
        (int_of_float (i *. 1_000.))
    in
    Hashtbl.replace table (name, labels) str ;
    (* Extension '.txt' is important so that the http server can infer a
       content mime for this page. *)
    let filename = t.dir // "metrics.txt" in
    let str =
      Hashtbl.to_seq table
      |> Seq.map (fun (_, v) -> v)
      |> List.of_seq |> String.concat "\n"
    in
    with_open_out filename (fun oc -> output_string oc str)

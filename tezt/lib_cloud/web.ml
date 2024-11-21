(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type service = {name : string; url : string}

type t = {
  process : Process.t;
  dir : string;
  monitoring : bool;
  prometheus : bool;
  mutable services : service list;
}

let pp_docker_image fmt = function
  | Env.Gcp {alias} -> Format.fprintf fmt "%s" alias
  | Octez_release {tag} -> Format.fprintf fmt "Octez %s release" tag

let domain agents =
  match Env.mode with
  | `Orchestrator -> Proxy.get_agent agents |> Agent.point |> Option.get |> fst
  | `Host | `Localhost | `Cloud -> "localhost"

let string_docker_command agent =
  match Agent.runner agent with
  | None -> "<no command, docker is host>"
  | Some runner ->
      let point = Agent.point agent |> Option.get in
      let ssh_id = runner.Runner.ssh_id in
      String.concat
        " "
        [
          "ssh";
          Format.asprintf "root@%s" (fst point);
          "-p";
          string_of_int (snd point);
          "-o";
          "StrictHostKeyChecking=no";
          "-i";
          ssh_id |> Option.get;
        ]

let string_vm_command agent =
  match Agent.cmd_wrapper agent with
  | None -> "# Just run the command, the VM is the host machine"
  | Some cmd_wrapper ->
      String.concat " " (cmd_wrapper.Gcloud.cmd :: cmd_wrapper.args)

let agent_jingo_template agent =
  let open Jingoo.Jg_types in
  let Configuration.
        {machine_type; docker_image; max_run_duration; binaries_path; os} =
    Agent.configuration agent
  in
  Tobj
    [
      ("name", Tstr (Agent.name agent));
      ("machine_type", Tstr machine_type);
      ("docker_image", Tstr (Format.asprintf "%a" pp_docker_image docker_image));
      ( "max_run_duration",
        Tstr
          (Option.fold
             ~none:"no limit"
             ~some:(fun time -> string_of_int time ^ "s")
             max_run_duration) );
      ("binaries_path", Tstr binaries_path);
      ("os", Tstr os);
      ("vm_command", Tstr (string_vm_command agent));
      ("docker_command", Tstr (string_docker_command agent));
    ]

let service_jingo_template {name; url} =
  let open Jingoo.Jg_types in
  Tobj [("title", Tstr (String.capitalize_ascii name)); ("uri", Tstr url)]

let jingo_template t agents =
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
          ("uri", Tstr (Format.asprintf "http://%s:9090" (domain agents)));
        ] );
    ( "monitoring",
      Tobj
        [
          ("activated", Tbool Env.monitoring);
          ("uri", Tstr (Format.asprintf "http://%s:19999" (domain agents)));
        ] );
    ("agents", Tlist (List.map agent_jingo_template agents));
    ("services", Tlist (List.map service_jingo_template t.services));
  ]

let index dir = dir // "index.md"

let write t ~agents =
  (* The content is formatted in markdown and will be rendered in html via
     pandoc. *)
  let content =
    Jingoo.Jg_template.from_file
      Path.website_index
      ~models:(jingo_template t agents)
  in
  let dir = t.dir in
  let index = index dir in
  Base.with_open_out index (fun oc -> output_string oc content) ;
  Process.run
    "docker"
    [
      "run";
      "--rm";
      "--volume";
      Format.asprintf "%s:/data" dir;
      "pandoc/core";
      "index.md";
      "-o";
      "index.html";
      "-s";
    ]

let add_service t ~agents service =
  t.services <- service :: t.services ;
  write t ~agents

let run () =
  let* () =
    Process.run "mkdir" ["-p"; Filename.get_temp_dir_name () // "website"]
  in
  let dir = Filename.get_temp_dir_name () // "website" in
  let index = index dir in
  let port = Env.website_port in
  let prometheus = Env.prometheus in
  let monitoring = Env.monitoring in
  let process =
    Process.spawn
      "python3"
      [
        "-m";
        "http.server";
        string_of_int port;
        "--directory";
        Filename.dirname index;
      ]
  in
  Lwt.return {process; dir; monitoring; prometheus; services = []}

let start ~agents =
  let* t = run () in
  let* () = write t ~agents in
  Lwt.return t

let shutdown t =
  Log.info "Shutting down the website..." ;
  Process.terminate t.process ;
  (* Do not fail if something happened during shutdown. *)
  let* _ = Process.wait t.process in
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

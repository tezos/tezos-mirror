(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  process : Process.t;
  dir : string;
  monitoring : bool;
  prometheus : bool;
}

let pp_docker_image fmt = function
  | Env.Gcp {alias} -> Format.fprintf fmt "%s" alias
  | Octez_latest_release -> Format.fprintf fmt "Octez latest release"

let configuration ~agents =
  let str =
    agents
    |> List.map (fun agent ->
           let Configuration.
                 {
                   machine_type;
                   docker_image;
                   max_run_duration;
                   binaries_path;
                   os;
                 } =
             Agent.configuration agent
           in
           Format.asprintf
             {|
## %s

- **Machine type**: %s
- **Docker_image**: %a
- **Max_run_duration**: %s
- **Binaries_path**: %s
- **OS**: %s

           |}
             (Agent.name agent)
             machine_type
             pp_docker_image
             docker_image
             (Option.fold
                ~none:"no limit"
                ~some:(fun time -> string_of_int time ^ "s")
                max_run_duration)
             binaries_path
             os)
    |> String.concat "\n"
  in
  Format.asprintf "# Configurations@.%s\n" str

let string_docker_command agent =
  let point = Agent.point agent in
  let ssh_id = (Agent.runner agent).Runner.ssh_id in
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

let debugging ~agents =
  let str =
    agents
    |> List.map (fun agent ->
           let host_run_command =
             Printf.sprintf
               {|
```bash
%s
```            
            |}
               (string_vm_command agent)
           in
           let docker_command =
             Printf.sprintf {|
```bash
%s
```
|} (string_docker_command agent)
           in
           Printf.sprintf
             {|
## %s 
Connect on the VM:
%s 

Connect on the Docker:
%s
|}
             (Agent.name agent)
             host_run_command
             docker_command)
    |> String.concat "\n"
  in
  Format.asprintf "# Debugging@.%s\n" str

let monitoring ~agents =
  if Env.monitoring then
    let str =
      agents
      |> List.map (fun agent ->
             let address =
               Agent.runner agent |> fun runner -> Runner.address (Some runner)
             in
             Format.asprintf
               "- [%s](http://%s:19999)"
               (Agent.name agent)
               address)
      |> String.concat "\n"
    in
    Format.asprintf "# Monitoring@.%s\n" str
  else
    "# Monitoring\n Monitoring disabled. Use `--monitoring` to activate it.\n"

let prometheus ~agents =
  let domain =
    match Env.mode with
    | `Orchestrator -> Proxy.get_agent agents |> Agent.point |> fst
    | `Host | `Localhost | `Cloud -> "localhost"
  in
  if Env.prometheus then
    Format.asprintf
      "# Prometheus\n [Prometheus dashboard](http://%s:%d)\n"
      domain
      Env.prometheus_port
  else "Prometheus disabled. Use `--prometheus` to activate it.\n"

let grafana ~agents =
  let domain =
    match Env.mode with
    | `Orchestrator -> Proxy.get_agent agents |> Agent.point |> fst
    | `Host | `Localhost | `Cloud -> "localhost"
  in
  if Env.grafana then
    Format.asprintf "# Grafana\n [Grafana dashboard](http://%s:3000)\n" domain
  else "Grafana disabled. Use `--grafana` to activate it.\n"

let markdown_content ~agents =
  [
    configuration ~agents;
    grafana ~agents;
    prometheus ~agents;
    monitoring ~agents;
    debugging ~agents;
  ]
  |> String.concat "\n"

let index dir = dir // "index.md"

let write t ~agents =
  (* The content is formatted in markdown and will be rendered in html via
     pandoc. *)
  let content = markdown_content ~agents in
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
  Lwt.return {process; dir; monitoring; prometheus}

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
  fun t ?(labels = []) ~name value ->
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
    let str =
      Format.asprintf
        "%s %s %f %d"
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

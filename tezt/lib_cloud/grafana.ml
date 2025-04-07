(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

include Tezt_tezos_tezt_performance_regression.Grafana

type t = {
  provisioning_directory : string;
  dashboard_directory : string;
  password : string;
}

let generate_password () = "saucisse"

let generate_admin_api_key password =
  let cmd = "curl" in
  let args =
    [
      "-X";
      "POST";
      "-H";
      "Content-Type: application/json";
      "-d";
      "\n\
      \  {\n\
      \        \"name\": \"admin_api_key\",\n\
      \        \"role\": \"Admin\"\n\
      \      }";
      "-u";
      Format.asprintf "admin:%s" password;
      "http://localhost:3000/api/auth/keys";
    ]
  in
  let* output = Process.run_and_read_stdout cmd args in
  let json = JSON.parse ~origin:"Grafana.generate_admin_api_key" output in
  let key = JSON.(json |-> "key" |> as_string) in
  Lwt.return key

let configuration admin_api_key : config =
  {
    url = Uri.of_string "http://localhost:3000";
    api_token = Some admin_api_key;
    data_source = "Prometheus";
    timeout = 2.0;
  }

let default_source =
  sf
    {|
- name: Prometheus
  type: prometheus
  access: proxy
  url: http://localhost:%d
  isDefault: true
|}
    Env.prometheus_port

let provisioning_directory sources =
  let provisioning_directory =
    Filename.get_temp_dir_name () // "grafana" // "provisioning"
  in
  let provisioning_file =
    provisioning_directory // "datasources" // "datasource.yml"
  in
  let* () = Process.run "mkdir" ["-p"; provisioning_file |> Filename.dirname] in
  let content =
    "apiVersion: 1\n\ndatasources:\n" ^ String.concat "\n" sources
  in
  with_open_out provisioning_file (fun oc ->
      Stdlib.seek_out oc 0 ;
      output_string oc content) ;
  Lwt.return provisioning_directory

let shutdown _t = Process.run "docker" ["kill"; "grafana"]

let dashboards_filepaths () =
  let path =
    if Env.mode = `Orchestrator then
      Filename.get_temp_dir_name () // "grafana" // "dashboards"
    else Path.grafana_dashboards
  in
  let* output = Process.run_and_read_stdout "ls" [path] in
  let basenames =
    String.trim output |> String.split_on_char '\n'
    |> List.filter (fun file -> String.ends_with ~suffix:".json" file)
  in
  List.map (fun basename -> path // basename) basenames |> Lwt.return

let run ?(sources = [default_source]) () =
  let cmd = "docker" in
  let* () =
    Process.run "mkdir" ["-p"; Filename.get_temp_dir_name () // "grafana"]
  in
  let dashboard_directory =
    Filename.get_temp_dir_name () // "grafana" // "dashboards"
  in
  let* () =
    Process.run "mkdir" ["-p"; dashboard_directory |> Filename.dirname]
  in
  let* provisioning_directory = provisioning_directory sources in
  (* We generate a password to use admin features. This is not completely
     secured but this should prevent easy attacks if the grafana port is opened. *)
  let password = generate_password () in
  Log.info "Grafana admin password: %s" password ;
  (* This is the last version supporting api keys *)
  let grafana_docker_tag = "grafana/grafana:11.2.3" in
  let args =
    [
      "run";
      "-d";
      "--rm";
      "--name";
      "grafana";
      "--network";
      "host";
      "-e";
      "GF_AUTH_ANONYMOUS_ENABLED=true";
      "-e";
      "GF_AUTH_ANONYMOUS_ORG_ROLE=Viewer";
      "-e";
      Format.asprintf "GF_SECURITY_ADMIN_PASSWORD=%s" password;
      "-v";
      Format.asprintf "%s:/etc/grafana/provisioning" provisioning_directory;
      "-v";
      Format.asprintf "%s:/var/lib/grafana/dashboards" dashboard_directory;
      grafana_docker_tag;
    ]
  in
  let* status = Process.spawn cmd args |> Process.wait in
  let* () =
    match status with
    | WEXITED 0 -> Lwt.return_unit
    | _ ->
        (* For some reason the container is already alive, we restart it. *)
        let* () = shutdown () in
        Process.run cmd args
  in
  let is_ready output = String.trim output = "200" in
  let run () =
    Process.spawn
      "curl"
      [
        "-s";
        "-o";
        "/dev/null";
        "-w";
        "%{http_code}";
        "http://localhost:3000/api/health";
      ]
  in
  let* _ = Env.wait_process ~is_ready ~run () in
  let* admin_api_key = generate_admin_api_key password in
  let configuration = configuration admin_api_key in
  let* dashboards_filepaths = dashboards_filepaths () in
  let dashboard dashboard_filepath =
    read_file dashboard_filepath
    |> Format.asprintf "{\"dashboard\": %s, \"overwrite\": true}"
  in
  let rec loop = function
    | [] -> Lwt.return_unit
    | name :: l ->
        let* () =
          update_dashboard_from_json
            configuration
            ~json:(dashboard name)
            ~uid:name
        in
        loop l
  in
  let* () = loop dashboards_filepaths in
  Lwt.return {provisioning_directory; dashboard_directory; password}

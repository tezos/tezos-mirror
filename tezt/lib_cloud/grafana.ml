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
  port : int;
}

let generate_password () = "saucisse"

let generate_admin_api_key ~port password =
  (* Step 1: Create a service account *)
  let* sa_output =
    Process.run_and_read_stdout
      "curl"
      [
        "-s";
        "-X";
        "POST";
        "-H";
        "Content-Type: application/json";
        "-d";
        {|{"name":"tezt-cloud","role":"Admin"}|};
        "-u";
        Format.asprintf "admin:%s" password;
        sf "http://localhost:%d/api/serviceaccounts" port;
      ]
  in
  let sa_json =
    JSON.parse
      ~origin:"Grafana.generate_admin_api_key:service_account"
      sa_output
  in
  let sa_id = JSON.(sa_json |-> "id" |> as_int) in
  (* Step 2: Create a token for the service account *)
  let* token_output =
    Process.run_and_read_stdout
      "curl"
      [
        "-s";
        "-X";
        "POST";
        "-H";
        "Content-Type: application/json";
        "-d";
        {|{"name":"admin_token"}|};
        "-u";
        Format.asprintf "admin:%s" password;
        sf "http://localhost:%d/api/serviceaccounts/%d/tokens" port sa_id;
      ]
  in
  let token_json =
    JSON.parse ~origin:"Grafana.generate_admin_api_key:token" token_output
  in
  let key = JSON.(token_json |-> "key" |> as_string) in
  Lwt.return key

let configuration ~port admin_api_key : config =
  {
    url = Uri.of_string (sf "http://localhost:%d" port);
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
  let provisioning_directory = Path.tmp_dir // "grafana" // "provisioning" in
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
    if Env.mode = `Remote_orchestrator_local_agents then
      Path.tmp_dir // "grafana" // "dashboards"
    else Path.grafana_dashboards
  in
  let* output = Process.run_and_read_stdout "ls" [path] in
  let basenames =
    String.trim output |> String.split_on_char '\n'
    |> List.filter (fun file -> String.ends_with ~suffix:".json" file)
  in
  List.map (fun basename -> path // basename) basenames |> Lwt.return

let run ?(port = 3000) ?(interface = "0.0.0.0") ?(sources = [default_source])
    ?auth_info () =
  let cmd = "docker" in
  let* () = Process.run "mkdir" ["-p"; Path.tmp_dir // "grafana"] in
  let dashboard_directory = Path.tmp_dir // "grafana" // "dashboards" in
  let* () =
    Process.run "mkdir" ["-p"; dashboard_directory |> Filename.dirname]
  in
  let* provisioning_directory = provisioning_directory sources in
  let password, anonymous_enabled =
    match auth_info with
    | Some (auth : Env.auth_infos) -> (auth.password, "false")
    | None ->
        (* We generate a password to use admin features. This is not completely
           secured but this should prevent easy attacks if the grafana port is
           opened. *)
        (generate_password (), "true")
  in
  Log.info "Grafana admin password: %s" password ;
  let grafana_docker_tag = "grafana/grafana:latest" in
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
      sf "GF_AUTH_ANONYMOUS_ENABLED=%s" anonymous_enabled;
      "-e";
      "GF_AUTH_ANONYMOUS_ORG_ROLE=Viewer";
      "-e";
      Format.asprintf "GF_SECURITY_ADMIN_PASSWORD=%s" password;
      "-e";
      sf "GF_SERVER_HTTP_ADDR=%s" interface;
      "-e";
      sf "GF_SERVER_HTTP_PORT=%d" port;
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
        sf "http://localhost:%d/api/health" port;
      ]
  in
  let* _ = Env.wait_process ~is_ready ~run () in
  let* admin_api_key = generate_admin_api_key ~port password in
  let configuration = configuration ~port admin_api_key in
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
  Lwt.return {provisioning_directory; dashboard_directory; password; port}

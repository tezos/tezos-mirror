(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

include Tezt_tezos_tezt_performance_regression.Grafana

type t = {provisioning_file : string; dashboard_directory : string}

let provisioning_file () =
  let provisioning_file =
    Filename.get_temp_dir_name ()
    // "grafana" // "provisioning" // "provisioning.yml"
  in
  let* () = Process.run "mkdir" ["-p"; provisioning_file |> Filename.dirname] in
  let content =
    {|
apiVersion: 1

datasources:
  - name: Prometheus
    type: prometheus
    access: proxy
    url: http://prometheus:9090
    isDefault: true

providers:
  - name: 'default'
    orgId: 1
    folder: ''
    type: file
    disableDeletion: true
    options:
      path: /var/lib/grafana/dashboards
|}
  in
  with_open_out provisioning_file (fun oc ->
      Stdlib.seek_out oc 0 ;
      output_string oc content) ;
  Lwt.return provisioning_file

let shutdown _t = Process.run "docker" ["kill"; "grafana"]

let run () =
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
  let* provisioning_file = provisioning_file () in
  let args =
    [
      "run";
      "-d";
      "--rm";
      "--name";
      "grafana";
      "--network";
      "host";
      "-p";
      "3000:3000";
      "-e";
      "GF_AUTH_ANONYMOUS_ENABLED=true";
      "-e";
      "GF_AUTH_ANONYMOUS_ORG_ROLE=Viewer";
      "-v";
      Format.asprintf
        "%s:/etc/grafana/provisioning"
        (Filename.dirname provisioning_file);
      "-v";
      Format.asprintf "%s:/var/lib/grafana/dashboards" dashboard_directory;
      "grafana/grafana";
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
  Lwt.return {provisioning_file; dashboard_directory}

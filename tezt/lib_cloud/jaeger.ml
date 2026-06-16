(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t = {port : int}

let run ?(port = 16686) ?(interface = "0.0.0.0") () =
  let* () =
    Process.run
      "docker"
      [
        "run";
        "-d";
        "--network";
        "host";
        "--rm";
        "--name";
        "jaeger";
        "-e";
        sf "QUERY_HTTP_SERVER_HOST_PORT=%s:%d" interface port;
        "-e";
        (* Turned off to avoid port collision with the OTel collector on port
           4317. The OTel collector already collects OpenTelemetry traces, so
           there is no need to launch a second receiver. *)
        "COLLECTOR_OTLP_ENABLED=false";
        "jaegertracing/all-in-one:latest";
      ]
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
        sf "http://localhost:%d/" port;
      ]
  in
  let* _ = Env.wait_process ~is_ready ~run () in
  Lwt.return {port}

let shutdown _t =
  let* () = Docker.kill "jaeger" |> Process.check in
  Lwt.return_unit

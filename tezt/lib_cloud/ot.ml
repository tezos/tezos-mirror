(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t = unit

let configuration =
  {|
receivers:
  otlp:
    protocols:
      grpc:  # Listening for OTLP data over gRPC (default port: 4317)
      http:  # Listening for OTLP data over HTTP (default port: 55681)

exporters:

processors:
  batch:  # Batch processor to optimize telemetry processing
    timeout: 5s

service:
  pipelines:
    traces:  # Pipeline to process trace data
      receivers: [otlp]
      processors: [batch]
      exporters: [otlp/jaeger]

  telemetry:
    metrics:
      address: "0.0.0.0:8888"  # Optional: Expose metrics for the collector itself (Prometheus scrapeable)

  extensions:
    - health_check

extensions:
  health_check:
    endpoint: "localhost:13133"
|}

let run () =
  let configuration_file =
    Filename.get_temp_dir_name () // "otel-config.yaml"
  in
  write_file configuration_file ~contents:configuration ;
  let* () =
    Process.run
      "docker"
      [
        "run";
        "--rm";
        "-d";
        "--network";
        "host";
        "--name";
        "otel-collector";
        "-p";
        "4317:4317";
        "-p";
        "13133:13133";
        "-p";
        "55680-55681:55680-55681";
        "-v";
        Format.asprintf "%s:/etc/otel/config.yaml" configuration_file;
        "otel/opentelemetry-collector:latest";
        "--config";
        "/etc/otel/config.yaml";
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
        "http://localhost:13133/healthz";
      ]
  in
  let* _ = Env.wait_process ~is_ready ~run () in
  Lwt.return ()

let shutdown () =
  let* () = Docker.kill "otel-collector" |> Process.check in
  Lwt.return_unit

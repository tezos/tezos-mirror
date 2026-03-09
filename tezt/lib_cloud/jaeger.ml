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
        "-p";
        sf "%d:%d" port port;
        "-p";
        "14250:14250";
        "-e";
        sf "QUERY_HTTP_SERVER_HOST_PORT=%s:%d" interface port;
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

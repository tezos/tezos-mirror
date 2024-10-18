(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t = unit

let run () =
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
        "16686:16686";
        "-p";
        "14250:14250";
        "jaegertracing/all-in-one:latest";
      ]
  in
  let is_ready output = String.trim output = "200" in
  let run () =
    Process.spawn
      "curl"
      ["-s"; "-o"; "/dev/null"; "-w"; "%{http_code}"; "http://localhost:16686/"]
  in
  let* _ = Env.wait_process ~is_ready ~run () in
  Lwt.return ()

let shutdown () =
  let* () = Docker.kill "jaeger" |> Process.check in
  Lwt.return_unit

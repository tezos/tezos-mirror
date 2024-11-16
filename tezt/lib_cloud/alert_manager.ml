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
        "--rm";
        "--network";
        "host";
        "--name";
        "alert-manager";
        "-p";
        "9093-9093";
        "prom/alertmanager:latest";
      ]
  in
  Lwt.return ()

let shutdown () =
  let* () = Docker.kill "alert-manager" |> Process.check in
  Lwt.return_unit

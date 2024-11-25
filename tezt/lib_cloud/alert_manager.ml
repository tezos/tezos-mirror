(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type alert = {
  name : string;
  expr : string;
  for_ : string option;
  description : string option;
  summary : string option;
  severity : [`Critical | `Warning | `Info | `None];
}

let alert ~expr ~severity ?for_ ?description ?summary name =
  {name; expr; for_; description; summary; severity}

let name_of_alert {name; _} = name

let severity_to_string = function
  | `Critical -> "critical"
  | `Warning -> "warning"
  | `Info -> "info"
  | `None -> "none"

let alert_template alert =
  let open Jingoo.Jg_types in
  let payload =
    [
      ("name", Tstr alert.name);
      ("expr", Tstr alert.expr);
      ("severity", Tstr (severity_to_string alert.severity));
    ]
    @ Option.fold
        ~none:[]
        ~some:(fun v -> [("description", Tstr v)])
        alert.description
    @ Option.fold ~none:[] ~some:(fun v -> [("summary", Tstr v)]) alert.summary
  in
  Tobj payload

type t = unit

let run ~configuration_files:_ =
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

(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Declaration of tests using tezt cloud. *)
let () =
  let command =
    Clap.subcommand
      [
        ( Clap.case "DAL" @@ fun () ->
          let module M = Scenarios_cli.Dal () in
          `dal (module M : Scenarios_cli.Dal) );
        ( Clap.case "LAYER1" @@ fun () ->
          let module M = Scenarios_cli.Layer1 () in
          `layer1 (module M : Scenarios_cli.Layer1) );
        (Clap.case "BASIC" @@ fun () -> `basic);
      ]
  in
  Tezt_cloud.register ~tags:[Tag.cloud] ;
  (match command with
  | `basic -> Basic.register ()
  | `dal m -> Dal.register m
  | `layer1 m -> Layer1.register m) ;
  Test.run ()

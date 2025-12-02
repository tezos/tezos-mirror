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
        ( Clap.case "TEZLINK" @@ fun () ->
          let module M = Scenarios_cli.Tezlink () in
          `tezlink (module M : Scenarios_cli.Tezlink) );
        (Clap.case "BASIC" @@ fun () -> `basic);
        ( Clap.case "ETHERLINK" @@ fun () ->
          let module M = Scenarios_cli.Etherlink () in
          `etherlink (module M : Scenarios_cli.Etherlink) );
        ( Clap.case
            ~description:
              "Perform tasks unrelated to any particular scenario such as \
               `terraform destroy` or `prometheus import` jobs."
            "CLOUD"
        @@ fun () -> `cloud );
      ]
  in
  (match command with
  | `basic -> Basic.register ()
  | `dal m -> Dal.register m
  | `layer1 m -> Layer1.register m
  | `tezlink m -> Tezlink.register m
  | `etherlink m -> Etherlink.register m
  | `cloud -> Tezt_cloud.register ~tags:[]) ;
  Test.run ()

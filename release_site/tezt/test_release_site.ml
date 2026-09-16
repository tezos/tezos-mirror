(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Release Page / release_site
    Invocation:   dune exec release_site/tezt/main.exe -- --file test_release_site.ml
    Subject:      Tests for the release_site rendering tool
*)

let release_site =
  project_root // "_build/default/release_site/src/release_site.exe"

let test_only_unknown_component =
  Test.register
    ~__FILE__
    ~title:"release_site rejects an unknown --only component"
    ~tags:["release_site"; "only"]
  @@ fun () ->
  (* An unknown [--only] component is rejected from the catalog before any
     remote access, so this test runs offline. *)
  let* stderr =
    Process.spawn
      ~name:"release_site"
      release_site
      [
        "--bucket";
        "dummy-bucket";
        "--output-dir";
        Temp.dir "site";
        "--only";
        "does-not-exist";
      ]
    |> Process.check_and_read_stderr ~expect_failure:true
  in
  Check.(
    (stderr =~ rex "not in the release site catalog")
      ~error_msg:"Expected the catalog rejection message, got: %L") ;
  unit

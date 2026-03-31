(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Release Page / generate-release-page.sh
    Invocation:   dune exec ci/bin_release_page/tezt/main.exe --file test_generate_release_page.ml
    Subject:      Integration test for the generate-release-page.sh script
*)

let version_manager =
  project_root // "_build/default/ci/bin_release_page/src/version_manager.exe"

let generate_release_page_sh =
  project_root // "scripts/releases/generate-release-page.sh"

let test_generate_release_page =
  Test.register
    ~__FILE__
    ~title:
      "generate-release-page.sh produces index.html, older_releases.html and \
       feed.xml"
    ~tags:["generate_release_page"]
  @@ fun () ->
  let dir = Temp.dir "release_page" in
  let file = dir // "versions.json" in
  write_file file ~contents:"[]" ;
  let* () =
    Process.run
      ~name:"version_manager"
      version_manager
      ["--file"; file; "add"; "--major"; "122"; "--minor"; "0"]
  in
  let* () =
    Process.run
      ~name:"version_manager"
      version_manager
      ["--file"; file; "add"; "--major"; "123"; "--minor"; "4"]
  in
  let* () =
    Process.run
      ~name:"generate_release_page"
      generate_release_page_sh
      ["--version-path"; dir]
  in
  let index = read_file (project_root // "index.html") in
  let older = read_file (project_root // "older_releases.html") in
  let feed = read_file (project_root // "feed.xml") in
  Check.(
    (index ^ older =~ rex "122\\.0")
      ~error_msg:"Expected v122.0 in generated HTML") ;
  Check.(
    (index ^ older =~ rex "123\\.4")
      ~error_msg:"Expected v123.4 in generated HTML") ;
  Check.((feed =~ rex "122\\.0") ~error_msg:"Expected v122.0 in feed.xml") ;
  Check.((feed =~ rex "123\\.4") ~error_msg:"Expected v123.4 in feed.xml") ;
  unit

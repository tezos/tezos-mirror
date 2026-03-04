(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Release Page / version_manager
    Invocation:   dune exec ci/bin_release_page/tezt/main.exe --file test_version_manager.exe
    Subject:      Tests for the version_manager tool
*)

let version_manager =
  project_root // "_build/default/ci/bin_release_page/version_manager.exe"

(** [fresh_versions_file ()] creates a temporary file containing an empty
    JSON array, suitable for use as a [versions.json] input. *)
let fresh_versions_file () =
  let file = Temp.file "versions.json" in
  write_file file ~contents:"[]" ;
  file

let run ?(expect_failure = false) ~file args =
  Process.run
    ~name:"version_manager"
    ~expect_failure
    version_manager
    (["--file"; file] @ args)

let run_stdout ~file args =
  Process.run_and_read_stdout
    ~name:"version_manager"
    version_manager
    (["--file"; file] @ args)

let list file = run_stdout ~file ["list"]

let test_add =
  Test.register
    ~__FILE__
    ~title:"add stable version"
    ~tags:["version_manager"; "add"]
  @@ fun () ->
  let file = fresh_versions_file () in
  let* () = run ~file ["add"; "--major"; "123"; "--minor"; "4"] in
  let* output = list file in
  Check.((output =~ rex "v123\\.4") ~error_msg:"Expected v123.4 in list output") ;
  unit

let test_add_rc =
  Test.register
    ~__FILE__
    ~title:"add release candidate"
    ~tags:["version_manager"; "add"; "rc"]
  @@ fun () ->
  let file = fresh_versions_file () in
  let* () = run ~file ["add"; "--major"; "123"; "--minor"; "4"; "--rc"; "1"] in
  let* output = list file in
  Check.(
    (output =~ rex "v123\\.4-rc1")
      ~error_msg:"Expected v123.4-rc1 in list output") ;
  unit

let test_add_duplicate =
  Test.register
    ~__FILE__
    ~title:"adding a duplicate version fails"
    ~tags:["version_manager"; "add"; "duplicate"]
  @@ fun () ->
  let file = fresh_versions_file () in
  let* () = run ~file ["add"; "--major"; "123"; "--minor"; "4"] in
  let* () =
    run ~expect_failure:true ~file ["add"; "--major"; "123"; "--minor"; "4"]
  in
  unit

(* Test setting a version as latest *)
let test_set_latest =
  Test.register
    ~__FILE__
    ~title:"set-latest marks one version and clears others"
    ~tags:["version_manager"; "set_latest"]
  @@ fun () ->
  let file = fresh_versions_file () in
  let* () = run ~file ["add"; "--major"; "122"; "--minor"; "4"] in
  let* () = run ~file ["add"; "--major"; "123"; "--minor"; "4"] in
  let* () = run ~file ["set-latest"; "--major"; "123"; "--minor"; "4"] in
  let* output = list file in
  Check.(
    (output =~ rex "v123\\.4 \\(latest\\)")
      ~error_msg:"Expected v123.4 to be marked (latest)") ;
  Check.(
    (output =~! rex "v122\\.4 \\(latest\\)")
      ~error_msg:"Expected v122.4 not to be marked (latest)") ;
  unit

let test_set_active =
  Test.register
    ~__FILE__
    ~title:"set-active and set-inactive"
    ~tags:["version_manager"; "set_active"; "set_inactive"]
  @@ fun () ->
  let file = fresh_versions_file () in
  let* () = run ~file ["add"; "--major"; "123"; "--minor"; "4"] in
  (* set-active prints all currently active versions after the operation. *)
  let* active_output =
    run_stdout ~file ["set-active"; "--major"; "123"; "--minor"; "4"]
  in
  Check.(
    (active_output =~ rex "v123\\.4")
      ~error_msg:"Expected v123.4 in set-active output") ;
  (* set-inactive prints which versions were deactivated and which remain active. *)
  let* inactive_output =
    run_stdout ~file ["set-inactive"; "--major"; "123"; "--minor"; "4"]
  in
  Check.(
    (inactive_output =~ rex "v123\\.4")
      ~error_msg:"Expected v123.4 in set-inactive output") ;
  unit

let test_update_build_number =
  Test.register
    ~__FILE__
    ~title:"update-build-number sets build mark"
    ~tags:["version_manager"; "update_build_number"]
  @@ fun () ->
  let file = fresh_versions_file () in
  let* () = run ~file ["add"; "--major"; "123"; "--minor"; "4"] in
  let* () =
    run
      ~file
      [
        "update-build-number";
        "--major";
        "123";
        "--minor";
        "4";
        "--build-number";
        "5";
      ]
  in
  let* output = list file in
  Check.(
    (output =~ rex "v123\\.4 \\[build 5\\]")
      ~error_msg:"Expected v123.4 [build 5] in list output") ;
  unit

let test_generate_rss =
  Test.register
    ~__FILE__
    ~title:"generate-rss"
    ~tags:["version_manager"; "generate_rss"]
  @@ fun () ->
  let file = fresh_versions_file () in
  let output = Filename.temp_file "feed" ".xml" in
  (* Add two versions, with build numbers on the second. *)
  let* () = run ~file ["add"; "--major"; "122"; "--minor"; "4"] in
  let* () = run ~file ["add"; "--major"; "123"; "--minor"; "4"] in
  let* () =
    run
      ~file
      [
        "update-build-number";
        "--major";
        "123";
        "--minor";
        "4";
        "--build-number";
        "1";
      ]
  in
  let* () =
    run
      ~file
      [
        "update-build-number";
        "--major";
        "123";
        "--minor";
        "4";
        "--build-number";
        "2";
      ]
  in
  let* () =
    run
      ~file
      ["generate-rss"; "--path"; "octez.tezos.com/releases"; "--output"; output]
  in
  Check.is_true
    (Sys.file_exists output)
    ~error_msg:"Expected RSS feed file to be created" ;
  let content = read_file output in
  (* Each version generates a base RSS item. *)
  Check.(
    (content =~ rex "Octez v122\\.4") ~error_msg:"Expected RSS item for v122.4") ;
  Check.(
    (content =~ rex "Octez v123\\.4") ~error_msg:"Expected RSS item for v123.4") ;
  (* Each build number generates an additional RSS item. *)
  Check.(
    (content =~ rex "packaging revision 1")
      ~error_msg:"Expected RSS item for packaging revision 1") ;
  Check.(
    (content =~ rex "packaging revision 2")
      ~error_msg:"Expected RSS item for packaging revision 2") ;
  unit

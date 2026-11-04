(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Release Page / version_manager
    Invocation:   dune exec release_site/tezt/main.exe --file test_version_manager.exe
    Subject:      Tests for the version_manager tool
*)

let version_manager =
  project_root // "_build/default/release_site/src/version_manager.exe"

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

let test_add_dated =
  Test.register
    ~__FILE__
    ~title:"add dated version"
    ~tags:["version_manager"; "add"; "dated"]
  @@ fun () ->
  let file = fresh_versions_file () in
  let* () = run ~file ["add"; "--date"; "20260724"] in
  let* output = list file in
  Check.(
    (output =~ rex "v20260724\n") ~error_msg:"Expected v20260724 in list output") ;
  Check.(
    (output =~! rex "v20260724\\.")
      ~error_msg:"Expected no counter after v20260724") ;
  let raw = read_file file in
  Check.((raw =~ rex "\"date\": 20260724") ~error_msg:"Expected a date key") ;
  Check.(
    (raw =~! rex "major|minor") ~error_msg:"Expected no major or minor key") ;
  unit

let test_add_dated_with_counter =
  Test.register
    ~__FILE__
    ~title:"dated versions with and without a counter are distinct"
    ~tags:["version_manager"; "add"; "dated"]
  @@ fun () ->
  let file = fresh_versions_file () in
  let* () = run ~file ["add"; "--date"; "20260724"] in
  let* () = run ~file ["add"; "--date"; "20260724"; "--counter"; "1"] in
  let* output = list file in
  Check.(
    (output =~ rex "v20260724\n") ~error_msg:"Expected v20260724 in list output") ;
  Check.(
    (output =~ rex "v20260724\\.1")
      ~error_msg:"Expected v20260724.1 in list output") ;
  unit

let test_add_rejects_mixed_schemes =
  Test.register
    ~__FILE__
    ~title:"add rejects a missing minor and mixed version schemes"
    ~tags:["version_manager"; "add"; "dated"]
  @@ fun () ->
  let file = fresh_versions_file () in
  let* () = run ~expect_failure:true ~file ["add"; "--major"; "25"] in
  let* () =
    run
      ~expect_failure:true
      ~file
      ["add"; "--major"; "25"; "--minor"; "0"; "--date"; "20260724"]
  in
  let* () = run ~expect_failure:true ~file ["add"; "--counter"; "1"] in
  unit

let test_read_entry_without_minor_fails =
  Test.register
    ~__FILE__
    ~title:"an entry with a major and no minor does not decode"
    ~tags:["version_manager"; "compat"]
  @@ fun () ->
  let file = Temp.file "versions.json" in
  write_file file ~contents:{|[{"major":25,"latest":true,"pubDate":0}]|} ;
  let* () = run ~expect_failure:true ~file ["list"] in
  unit

let test_read_legacy_minor =
  Test.register
    ~__FILE__
    ~title:"a version written with a minor still decodes"
    ~tags:["version_manager"; "add"; "compat"]
  @@ fun () ->
  let file = Temp.file "versions.json" in
  (* The Octez and Etherlink pages always write a minor. *)
  write_file
    file
    ~contents:
      {|[{"major":25,"minor":0,"latest":true,"active":false,"pubDate":0}]|} ;
  let* output = list file in
  Check.((output =~ rex "v25\\.0") ~error_msg:"Expected v25.0 in list output") ;
  (* Re-encoding must keep the minor field. *)
  let* () = run ~file ["set-latest"; "--major"; "25"; "--minor"; "0"] in
  Check.(
    (read_file file =~ rex "\"minor\": 0")
      ~error_msg:"Expected re-encoded JSON to keep the minor field") ;
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

let test_add_beta =
  Test.register
    ~__FILE__
    ~title:"add beta version"
    ~tags:["version_manager"; "add"; "beta"]
  @@ fun () ->
  let file = fresh_versions_file () in
  let* () =
    run ~file ["add"; "--major"; "123"; "--minor"; "4"; "--beta"; "1"]
  in
  let* output = list file in
  Check.(
    (output =~ rex "v123\\.4-beta1")
      ~error_msg:"Expected v123.4-beta1 in list output") ;
  unit

let test_add_beta_and_rc_fails =
  Test.register
    ~__FILE__
    ~title:"add with both --rc and --beta fails"
    ~tags:["version_manager"; "add"; "rc"; "beta"]
  @@ fun () ->
  let file = fresh_versions_file () in
  let* () =
    run
      ~expect_failure:true
      ~file
      ["add"; "--major"; "123"; "--minor"; "4"; "--rc"; "1"; "--beta"; "1"]
  in
  unit

let test_add_beta_distinct_from_stable =
  Test.register
    ~__FILE__
    ~title:"beta and stable with same major.minor are distinct"
    ~tags:["version_manager"; "add"; "beta"; "duplicate"]
  @@ fun () ->
  let file = fresh_versions_file () in
  let* () =
    run ~file ["add"; "--major"; "123"; "--minor"; "4"; "--beta"; "1"]
  in
  (* Adding the stable version should succeed, not be treated as a duplicate. *)
  let* () = run ~file ["add"; "--major"; "123"; "--minor"; "4"] in
  let* output = list file in
  Check.(
    (output =~ rex "v123\\.4-beta1")
      ~error_msg:"Expected v123.4-beta1 in list output") ;
  Check.(
    (output =~ rex "v123\\.4\n") ~error_msg:"Expected v123.4 in list output") ;
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

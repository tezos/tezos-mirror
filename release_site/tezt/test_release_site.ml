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

let version_manager =
  project_root // "_build/default/release_site/src/version_manager.exe"

(* [write_versions dir versions] writes to [dir/versions.json] a versions.json
   containing the given [(major, minor)] [versions]. *)
let write_versions dir versions =
  let file = dir // "versions.json" in
  write_file file ~contents:"[]" ;
  Lwt_list.iter_s
    (fun (major, minor) ->
      Process.run
        ~name:"version_manager"
        version_manager
        [
          "--file";
          file;
          "add";
          "--major";
          string_of_int major;
          "--minor";
          string_of_int minor;
        ])
    versions

(* [add_dated_version dir date] adds to [dir/versions.json] a dated version
   without a counter, as used by the smart rollup node. *)
let add_dated_version dir date =
  Process.run
    ~name:"version_manager"
    version_manager
    ["--file"; dir // "versions.json"; "add"; "--date"; string_of_int date]

(* The components of the site catalog other than octez, which is at the root. *)
let subcomponents = ["grafazos"; "teztale"; "octez-smart-rollup-node"]

(* [render_locally ?only ~versions_dir ~output_dir ()] runs [release_site] on
   the versions.json files of [versions_dir], without any storage access. *)
let render_locally ?only ~versions_dir ~output_dir () =
  Process.run
    ~name:"release_site"
    release_site
    ([
       "--bucket";
       "dummy-bucket";
       "--url";
       "example.com";
       "--path";
       "/releases";
       "--versions-dir";
       versions_dir;
       "--no-remote-assets";
       "--output-dir";
       output_dir;
     ]
    @ match only with None -> [] | Some c -> ["--only"; c])

let test_render_site_locally =
  Test.register
    ~__FILE__
    ~title:"release_site renders the whole site from local versions.json"
    ~tags:["release_site"; "render"]
  @@ fun () ->
  let versions_dir = Temp.dir "versions" in
  let* () = write_versions versions_dir [(122, 0); (123, 4)] in
  (* A dated version at the root exercises the feed, which only octez emits. *)
  let* () = add_dated_version versions_dir 20260724 in
  let* () =
    Lwt_list.iter_s
      (fun c ->
        let dir = versions_dir // c in
        Sys.mkdir dir 0o755 ;
        write_versions dir [(1, 2)])
      subcomponents
  in
  let* () =
    add_dated_version (versions_dir // "octez-smart-rollup-node") 20260724
  in
  let output_dir = Temp.dir "site" in
  let* () = render_locally ~versions_dir ~output_dir () in
  let index = read_file (output_dir // "index.html") in
  let older = read_file (output_dir // "older_releases.html") in
  Check.(
    (index ^ older =~ rex "Octez 122\\.0")
      ~error_msg:"Expected v122.0 in the octez pages") ;
  Check.(
    (index ^ older =~ rex "Octez 123\\.4")
      ~error_msg:"Expected v123.4 in the octez pages") ;
  Check.(
    (index ^ older =~ rex "20260724")
      ~error_msg:"Expected v20260724 in the octez pages") ;
  Check.(
    (index ^ older =~! rex "20260724\\.0")
      ~error_msg:"Expected no counter after v20260724 in the octez pages") ;
  let feed = read_file (output_dir // "feed.xml") in
  Check.(
    (feed =~ rex "Octez v122\\.0") ~error_msg:"Expected v122.0 in feed.xml") ;
  Check.(
    (feed =~ rex "Octez v123\\.4") ~error_msg:"Expected v123.4 in feed.xml") ;
  Check.((feed =~ rex "20260724") ~error_msg:"Expected v20260724 in feed.xml") ;
  Check.(
    (feed =~! rex "20260724\\.0")
      ~error_msg:"Expected no counter after v20260724 in feed.xml") ;
  Check.(
    (feed =~ rex "<link>\\s*https://example\\.com/releases\\s*</link>")
      ~error_msg:"Expected the feed link to be derived from --url and --path") ;
  List.iter
    (fun c ->
      let index = read_file (output_dir // c // "index.html") in
      Check.(
        (index =~ rex "1\\.2") ~error_msg:(sf "Expected v1.2 in the %s tab" c)) ;
      Check.is_false
        (Sys.file_exists (output_dir // c // "feed.xml"))
        ~error_msg:(sf "Expected no feed.xml for %s" c))
    subcomponents ;
  let rollup_node =
    read_file (output_dir // "octez-smart-rollup-node" // "index.html")
  in
  Check.(
    (rollup_node =~ rex "20260724")
      ~error_msg:"Expected v20260724 in the rollup node tab") ;
  Check.(
    (rollup_node =~! rex "20260724\\.0")
      ~error_msg:"Expected no counter after v20260724 in the rollup node tab") ;
  unit

let test_render_only_locally =
  Test.register
    ~__FILE__
    ~title:"release_site --only renders a single tab"
    ~tags:["release_site"; "render"; "only"]
  @@ fun () ->
  (* Only the selected component's versions.json is provided: the other ones
     must not be read. *)
  let versions_dir = Temp.dir "versions" in
  Sys.mkdir (versions_dir // "teztale") 0o755 ;
  let* () = write_versions (versions_dir // "teztale") [(1, 2)] in
  let output_dir = Temp.dir "site" in
  let* () = render_locally ~only:"teztale" ~versions_dir ~output_dir () in
  let index = read_file (output_dir // "teztale" // "index.html") in
  Check.((index =~ rex "Teztale 1\\.2") ~error_msg:"Expected v1.2 in the tab") ;
  Check.is_false
    (Sys.file_exists (output_dir // "index.html"))
    ~error_msg:"Expected the octez tab not to be rendered" ;
  unit

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

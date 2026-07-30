(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Manifest
open Externals

include Product (struct
  let name = "release_site"

  let source = ["release_site/"]
end)

let _release_page_base_lib =
  private_lib
    "base_lib"
    ~opam:""
    ~path:"release_site/src"
    ~release_status:Unreleased
    ~modules:["base"]
    ~profile:"release-tools-deps"
    ~deps:[unix; clap; tezt_json_lib]

let _release_page =
  private_exe
    "release_page"
    ~opam:""
    ~path:"release_site/src"
    ~release_status:Unreleased
    ~modules:["release_page"]
    ~profile:"release-tools-deps"
    ~deps:[unix; clap; tezt_json_lib; _release_page_base_lib |> open_]

let _version_manager =
  private_exe
    "version_manager"
    ~opam:""
    ~path:"release_site/src"
    ~release_status:Unreleased
    ~modules:["version_manager"]
    ~profile:"release-tools-deps"
    ~deps:[unix; clap; tezt_json_lib; rss; _release_page_base_lib |> open_]

let _release_page_tests =
  private_exe
    "main"
    ~path:"release_site/tezt"
    ~opam:""
    ~synopsis:"Tests for the release page tools"
    ~release_status:Unreleased
    ~modules:["main"; "test_version_manager"; "test_generate_release_page"]
    ~profile:"release-tools-deps"
    ~deps:
      [tezt_lib |> open_ |> open_ ~m:"Base"; _release_page_base_lib |> open_]

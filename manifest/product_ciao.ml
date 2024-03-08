(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021-2023 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022-2023 Trili Tech <contact@trili.tech>                   *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

open Manifest
open Externals

let ci_lib_gitlab_ci_main =
  public_lib
    "gitlab_ci"
    ~synopsis:"OCaml library for generating GitLab CI YAML configuration files"
    ~path:"ci/lib_gitlab_ci"
    ~bisect_ppx:No
    ~deps:[yaml]
    ~inline_tests:ppx_expect
    ~release_status:Unreleased

let _ci_bin_main =
  private_exe
    "main"
    ~opam:""
    ~path:"ci/bin"
    ~bisect_ppx:No
    ~deps:[ci_lib_gitlab_ci_main |> open_ ~m:"Base"; yaml; unix]
    ~release_status:Unreleased

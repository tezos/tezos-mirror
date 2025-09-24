(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021-2024 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022-2023 Trili Tech <contact@trili.tech>                   *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Manifest
open Externals

include Product (struct
  let name = "CIAO"

  let source = ["ci"]
end)

let ci_lib_gitlab_ci_main =
  public_lib
    "gitlab_ci"
    ~synopsis:"OCaml library for generating GitLab CI YAML configuration files"
    ~path:"ci/lib_gitlab_ci"
    ~bisect_ppx:No
    ~deps:[yaml]
    ~inline_tests:ppx_expect
    ~release_status:Unreleased

let ci_lib_tezos_ci =
  private_lib
    "tezos_ci"
    ~opam:""
    ~path:"ci/lib_tezos_ci"
    ~bisect_ppx:No
    ~deps:[ci_lib_gitlab_ci_main |> open_ ~m:"Base"]
    ~release_status:Unreleased

let ci_lib_cacio =
  private_lib
    "cacio"
    ~opam:""
    ~path:"ci/lib_cacio"
    ~bisect_ppx:No
    ~deps:[ci_lib_gitlab_ci_main; ci_lib_tezos_ci]
    ~release_status:Unreleased

let ci_sdk_bindings =
  private_lib
    "sdk_bindings_ci"
    ~opam:""
    ~path:"contrib/sdk-bindings/ci"
    ~bisect_ppx:No
    ~modules:["sdk_bindings_ci"]
    ~deps:[ci_lib_gitlab_ci_main |> open_ ~m:"Base"; ci_lib_tezos_ci]
    ~release_status:Unreleased

let ci_lib_tezos_ci_jobs =
  private_lib
    "tezos_ci_jobs"
    ~opam:""
    ~path:"ci/lib_tezos_ci_jobs"
    ~bisect_ppx:No
    ~deps:
      [
        ci_lib_gitlab_ci_main |> open_ ~m:"Base";
        ci_lib_tezos_ci;
        ci_lib_cacio;
        ci_sdk_bindings;
        tezt_core_lib;
      ]
    ~release_status:Unreleased

let _release_page =
  private_exe
    "release_page"
    ~opam:""
    ~path:"ci/bin_release_page"
    ~release_status:Unreleased
    ~deps:[unix; clap; tezt_json_lib]

let ci_grafazos =
  private_lib
    "grafazos"
    ~opam:""
    ~path:"grafazos/ci"
    ~bisect_ppx:No
    ~deps:[ci_lib_gitlab_ci_main; ci_lib_tezos_ci; ci_lib_cacio]
    ~release_status:Unreleased

let ci_teztale =
  private_lib
    "teztale"
    ~opam:""
    ~path:"teztale/ci"
    ~bisect_ppx:No
    ~deps:
      [ci_lib_gitlab_ci_main |> open_ ~m:"Base"; ci_lib_tezos_ci; ci_lib_cacio]
    ~release_status:Unreleased

let ci_rollup_node =
  private_lib
    "rollup_node"
    ~opam:""
    ~path:"rollup_node/ci"
    ~bisect_ppx:No
    ~deps:
      [ci_lib_gitlab_ci_main |> open_ ~m:"Base"; ci_lib_tezos_ci; ci_lib_cacio]
    ~release_status:Unreleased

let ci_etherlink =
  private_lib
    "etherlink_ci"
    ~opam:""
    ~path:"etherlink/ci"
    ~bisect_ppx:No
    ~deps:
      [
        ci_lib_gitlab_ci_main |> open_ ~m:"Base";
        ci_lib_tezos_ci;
        ci_lib_cacio;
        ci_lib_tezos_ci_jobs;
      ]
    ~release_status:Unreleased

let _ci_bin_main =
  private_exe
    "main"
    ~opam:""
    ~path:"ci/bin"
    ~bisect_ppx:No
    ~deps:
      [
        ci_lib_gitlab_ci_main |> open_ ~m:"Base";
        ci_lib_tezos_ci;
        ci_lib_tezos_ci_jobs |> open_;
        ci_grafazos;
        ci_teztale;
        ci_rollup_node;
        ci_sdk_bindings;
        ci_etherlink;
      ]
    ~release_status:Unreleased

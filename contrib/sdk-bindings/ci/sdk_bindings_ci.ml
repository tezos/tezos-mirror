(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_ci
open Tezos_ci.Cache

(** Set of SDK-bindings related files *)
let changeset = Changeset.make ["sdk/rust/**/*"; "contrib/sdk-bindings"]

let job_test ?dependencies ?rules () =
  job
    ~__POS__
    ~name:"test_sdk_bindings"
    ~description:"Tests bindings of the Rust SDK"
    ~image:Images.rust_sdk_bindings
    ~stage:Stages.test
    ?dependencies
    ~before_script:[". $HOME/.venv/bin/activate"]
    ?rules
    ["make -C contrib/sdk-bindings check"; "make -C contrib/sdk-bindings test"]
  |> enable_cargo_cache |> enable_sccache

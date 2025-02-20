(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Gitlab_ci.Util
open Tezos_ci
module String_set = Set.Make (String)

(** Set of Teztale files *)
let changeset = Changeset.(make ["teztale/**/*"])

(** Job that builds the Teztale executables *)
let job_build ?rules ?(expire_in = Gitlab_ci.Types.(Duration (Days 1))) ~arch ()
    =
  let arch_string = arch_to_string arch in
  job
    ~__POS__
    ~arch
    ~name:("teztale.build:static-" ^ arch_string)
    ~image:Images.CI.build
    ~stage:Stages.build
    ?rules
    ~artifacts:
      (artifacts
         ~name:"teztale-binaries"
         ~expire_in
         ~when_:On_success
         ["octez-teztale-*"])
    ~before_script:
      [
        "./scripts/ci/take_ownership.sh";
        ". ./scripts/version.sh";
        "eval $(opam env)";
      ]
    ["make teztale"]

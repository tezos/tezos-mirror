(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the CI pipeline for building Tezos X artifacts:
   Octez binaries (octez-node, octez-client, octez-smart-rollup-node)
   and kernel artifacts (evm_kernel.wasm, smart-rollup-installer, etc.).

   The pipeline is triggered with TZ_SCHEDULE_KIND=tezos_x.build. *)

module CI = Cacio.Make (struct
  let name = "tezos_x"

  let paths = []
end)

let job_build_released =
  let executables_file = "script-inputs/tezos-x-executables" in
  let executables = read_lines_from_file executables_file in
  CI.job
    "build_released"
    ~__POS__
    ~description:"Build the Tezos X executables for amd64."
    ~stage:Build
    ~cpu:Very_high
    ~image:Tezos_ci.Images.CI.build
    ~variables:[("EXECUTABLE_FILES", "script-inputs/tezos-x-executables")]
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~name:"build-x86_64-$CI_COMMIT_REF_SLUG"
         ~when_:On_success
         ~expire_in:(Duration (Days 1))
         executables)
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ~policy:Pull_push ())
    ~script:
      [
        "./scripts/ci/take_ownership.sh";
        ". ./scripts/version.sh";
        "eval $(opam env)";
        "./scripts/ci/build_full_unreleased.sh";
      ]

let register () =
  CI.register_scheduled_pipeline
    "build"
    ~description:
      "Builds Octez binaries and kernel artifacts for Tezos X.\n\n\
       This pipeline is triggered with TZ_SCHEDULE_KIND=tezos_x.build. It \
       builds octez-node, octez-client, octez-evm-node, \
       octez-smart-rollup-node and kernel artifacts (evm_kernel.wasm, \
       smart-rollup-installer)."
    [(Auto, Kernels.job_build_kernels); (Auto, job_build_released)]

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
  Common.Build.job_build_dynamic_binaries
    ~name:"tezos_x.build_released"
    ~__POS__
    ~arch:Amd64
    ~cpu:Very_high
    ~dependencies:(Dependent [])
    "script-inputs/tezos-x-executables"

let register () =
  CI.register_scheduled_pipeline
    "build"
    ~description:
      "Builds Octez binaries and kernel artifacts for Tezos X.\n\n\
       This pipeline is triggered with TZ_SCHEDULE_KIND=tezos_x.build. It \
       builds octez-node, octez-client, octez-smart-rollup-node and kernel \
       artifacts (evm_kernel.wasm, smart-rollup-installer)."
    ~legacy_jobs:[job_build_released]
    [(Auto, Kernels.job_build_kernels)]

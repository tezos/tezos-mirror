(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This file defines jobs that were migrated to Cacio,
   but which are not yet part of a component.

   As such, the jobs are defined in the [Shared] component,
   and are added to the [scheduled_extended_test] pipeline. *)

module CI = Cacio.Shared

(* Requires Python.
   Can be changed to a python image, but using the build docker image
   to keep in sync with the python version used for the tests. *)
let job_script_b58_prefix =
  CI.job
    "oc.script:b58_prefix"
    ~__POS__
    ~stage:Test
    ~description:""
    ~image:Tezos_ci.Images.CI.test
    ~only_if_changed:
      [
        "scripts/b58_prefix/b58_prefix.py";
        "scripts/b58_prefix/test_b58_prefix.py";
      ]
    [
      ". ./scripts/version.sh";
      ". $HOME/.venv/bin/activate";
      "poetry run pylint scripts/b58_prefix/b58_prefix.py \
       --disable=missing-docstring --disable=invalid-name";
      "poetry run pytest scripts/b58_prefix/test_b58_prefix.py";
    ]

let job_test_liquidity_baking_scripts =
  CI.job
    "oc.test-liquidity-baking-scripts"
    ~__POS__
    ~stage:Test
    ~description:"Test the liquidity baking scripts."
    ~image:Tezos_ci.Images.CI.build
    ~needs_legacy:
      [
        (Artifacts, Code_verification.job_build_x86_64_release Before_merging);
        (Artifacts, Code_verification.job_build_x86_64_extra_exp Before_merging);
        (Artifacts, Code_verification.job_build_x86_64_extra_dev Before_merging);
      ]
    ~only_if_changed:
      [
        "src/**/*";
        "scripts/ci/test_liquidity_baking_scripts.sh";
        "scripts/check-liquidity-baking-scripts.sh";
      ]
    [
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "./scripts/ci/test_liquidity_baking_scripts.sh";
    ]

let job_test_release_versions =
  CI.job
    "oc:scripts:release_script_values"
    ~__POS__
    ~description:
      "Test the values defined in scripts/ci/octez-packages-version.sh."
    ~image:Tezos_ci.Images.CI.prebuild
    ~stage:Test
    ~only_if_changed:
      [
        "scripts/releases/octez-release.sh";
        "scripts/ci/octez-packages-version.sh";
        "scripts/ci/test_release_values.sh";
      ]
    ["scripts/ci/test_release_values.sh"]

let register () =
  CI.register_before_merging_jobs
    [
      (Auto, job_script_b58_prefix);
      (Auto, job_test_liquidity_baking_scripts);
      (Auto, job_test_release_versions);
    ] ;
  CI.register_schedule_extended_test_jobs
    [
      (Auto, job_script_b58_prefix);
      (Auto, job_test_liquidity_baking_scripts);
      (Auto, job_test_release_versions);
    ] ;
  ()

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

let job_script_snapshot_alpha_and_link =
  CI.job
    "oc.script:snapshot_alpha_and_link"
    ~__POS__
    ~stage:Test
    ~description:
      "Test that Alpha can be snapshotted using snapshot_alpha_and_link.sh."
    ~image:Tezos_ci.Images.CI.build
    ~cpu:Very_high
    ~only_if_changed:
      [
        "src/proto_alpha/**/*";
        "scripts/snapshot_alpha_and_link.sh";
        "scripts/snapshot_alpha.sh";
        "scripts/user_activated_upgrade.sh";
      ]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    ~dune_cache:
      (Cacio.dune_cache
         ~key:
           ("dune-build-cache-"
           ^ Gitlab_ci.Predefined_vars.(show ci_pipeline_id))
         ~policy:Pull
         ())
    [
      "./scripts/ci/take_ownership.sh";
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "./scripts/ci/script:snapshot_alpha_and_link.sh";
    ]

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

let job_oc_script_test_release_versions =
  CI.job
    "oc.script:test_octez_release_versions"
    ~__POS__
    ~stage:Test
    ~description:"Test how src/lib_version parses Git tags."
    ~image:Tezos_ci.Images.CI.build
    ~only_if_changed:
      ["scripts/test_octez_release_version.sh"; "src/lib_version/**/*"]
    [
      "./scripts/ci/take_ownership.sh";
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "./scripts/test_octez_release_version.sh";
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
      (Auto, job_script_snapshot_alpha_and_link);
      (Auto, job_script_b58_prefix);
      (Auto, job_test_liquidity_baking_scripts);
      (Auto, job_oc_script_test_release_versions);
      (Auto, job_test_release_versions);
    ] ;
  CI.register_schedule_extended_test_jobs
    [
      (Auto, job_script_snapshot_alpha_and_link);
      (Auto, job_script_b58_prefix);
      (Auto, job_test_liquidity_baking_scripts);
      (Auto, job_oc_script_test_release_versions);
      (Auto, job_test_release_versions);
    ] ;
  ()

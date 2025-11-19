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
  CI.register_before_merging_jobs [(Auto, job_test_release_versions)] ;
  CI.register_schedule_extended_test_jobs [(Auto, job_test_release_versions)] ;
  ()

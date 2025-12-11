(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This file defines sanity jobs that are not component-specific.
   As such, the jobs are defined in the [Shared] component.

   Sanity jobs are fast jobs that trigger without the need
   to click on the manual [trigger] job.
   Their purpose is to quickly detect some common mistakes
   before adding jobs to the merge train.
   Most of them also run in [schedule_extended_test]. *)

module CI = Cacio.Shared

let job_sanity_ci =
  CI.job
    "sanity_ci"
    ~__POS__
    ~description:
      "Check that generated dune, .opam and .yml files are up-to-date."
    ~image:Tezos_ci.Images.CI.build_master
    ~stage:Test
    ~only_if_changed:
      [
        "**/manifest/**/*";
        "**/dune";
        "opam/**/*";
        "**/ci/**/*";
        ".gitlab-ci.yml";
        ".gitlab/ci/pipelines/*.yml";
      ]
    [
      "./scripts/ci/take_ownership.sh";
      "eval $(opam env)";
      "make --silent -C manifest check";
      "make --silent -C ci check";
    ]

let register () =
  CI.register_before_merging_jobs [(Immediate, job_sanity_ci)] ;
  CI.register_schedule_extended_test_jobs [(Immediate, job_sanity_ci)] ;
  ()

(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* Global, non-component-specific, Tezt jobs.
   They all run tests from [tezt/tests/main.exe].
   Eventually most tests should be moved to component-specific Tezt jobs instead. *)

(* We will define Tezt jobs for tests that have not been split into components.
   As such, those jobs are shared between multiple components.
   So we define them in the [Shared] component. *)
module CI = Cacio.Shared

(** Tezt tag selector string.

    It returns a TSL expression that:
    - always deselects tags with [ci_disabled];
    - selects, respectively deselects, the tests with the tags
      [time_sensitive], [slow], [extra] or [cloud],
      depending on the value of the corresponding function argument.
      These arguments all default to false.

    See [src/lib_test/tag.mli] for a description of the above tags.

    The list of TSL expressions [and_] are appended to the final
    selector, allowing to modify the selection further. *)
let tests_tag_selector ?(time_sensitive = false) ?(slow = false)
    ?(extra = false) ?(cloud = false) (and_ : Tezt_core.TSL_AST.t list) :
    Tezt_core.TSL_AST.t =
  let tags =
    [
      (false, "ci_disabled");
      (time_sensitive, "time_sensitive");
      (slow, "slow");
      (extra, "extra");
      (cloud, "cloud");
    ]
  in
  let positive, negative = List.partition fst tags in
  let positive = List.map snd positive in
  let negative = List.map snd negative in
  Tezt_core.(
    TSL.conjunction
    @@ List.map (fun tag -> TSL_AST.Has_tag tag) positive
    @ List.map (fun tag -> TSL_AST.Not (Has_tag tag)) negative
    @ and_)

let common_needs =
  [
    (Cacio.Artifacts, Code_verification.job_build_x86_64_release Before_merging);
    (Artifacts, Code_verification.job_build_x86_64_extra_exp Before_merging);
    (Artifacts, Code_verification.job_build_x86_64_extra_dev Before_merging);
    (Artifacts, Code_verification.job_build_kernels Before_merging);
  ]

(* Note: before the migration to Cacio, some jobs had a job timeout of 60 minutes.
   But they still had a global Tezt timeout of 30 minutes,
   and they were still wrapped under a [timeout] call of 31 minutes,
   essentially rendering the exception to the job timeout useless.
   So for now we keep the default timeout.
   If you want to change it, use [~global_timeout: (Minutes 60)].
   The [tezt_job] function will compute the rest. *)

(* Specialization of Cacio's [tezt_job] with defaults that are specific
   to Tezt jobs defined in this module / the [Shared] component. *)
let tezt_job ?(retry_tests = 1) ?(needs_legacy = common_needs) =
  CI.tezt_job
    ~only_if_changed:(Tezos_ci.Changeset.encode Changesets.changeset_octez)
    ~needs_legacy
    ~retry_tests

let job_tezt =
  Cacio.parameterize @@ fun pipeline ->
  tezt_job
    ""
    ~__POS__
    ~pipeline
    ~description:"Run normal Tezt tests."
    ~test_coverage:true
    ~test_selection:(tests_tag_selector [Not (Has_tag "flaky")])
    ~parallel_jobs:34
    ~parallel_tests:6
    ~retry_jobs:2

let job_tezt_time_sensitive =
  Cacio.parameterize @@ fun pipeline ->
  tezt_job
    "time-sensitive"
    ~__POS__
    ~pipeline
    ~description:"Run Tezt tests tagged as time_sensitive."
    ~test_coverage:true
    ~test_selection:(tests_tag_selector ~time_sensitive:true [])
    ~parallel_jobs:2
    ~retry_jobs:2

let job_tezt_riscv_slow_sequential =
  Cacio.parameterize @@ fun pipeline ->
  tezt_job
    "riscv-slow-sequential"
    ~__POS__
    ~pipeline
    ~description:"Run Tezt tests tagged as riscv_slow_sequential."
    ~test_selection:(Tezt_core.TSL_AST.Has_tag "riscv_slow_sequential")
    ~test_timeout:No_timeout
    ~retry_jobs:2

let job_tezt_slow =
  Cacio.parameterize @@ fun pipeline ->
  tezt_job
    "slow"
    ~__POS__
    ~pipeline
    ~description:"Run Tezt tests tagged as slow."
    ~test_selection:(tests_tag_selector ~slow:true [])
    ~test_timeout:No_timeout
    ~parallel_jobs:12
    ~parallel_tests:3
    ~retry_jobs:2

let job_tezt_extra =
  Cacio.parameterize @@ fun pipeline ->
  tezt_job
    "extra"
    ~__POS__
    ~pipeline
    ~description:"Run Tezt tests tagged as extra and not flaky."
    ~test_selection:(tests_tag_selector ~extra:true [Not (Has_tag "flaky")])
    ~parallel_jobs:1
    ~parallel_tests:6
    ~retry_jobs:2

let job_tezt_flaky =
  Cacio.parameterize @@ fun pipeline ->
  tezt_job
    "flaky"
    ~__POS__
    ~pipeline
    ~description:"Run Tezt tests tagged as flaky."
    ~test_coverage:true
    ~allow_failure:Yes
    ~test_selection:(tests_tag_selector [Has_tag "flaky"])
    ~parallel_jobs:2
    ~retry_jobs:2
    ~retry_tests:3

let job_tezt_static_binaries =
  Cacio.parameterize @@ fun pipeline ->
  tezt_job
    "static-binaries"
    ~__POS__
    ~pipeline
    ~description:
      "Run Tezt tests tagged as cli and not flaky, using static executables."
    ~cpu:Normal
    ~needs_legacy:
      [
        (* The static job should actually be taken from Code_verification,
           but it is only defined at toplevel in Master_branch.
           Since we only need the name, this is fine.
           We did the same in the docs/ci. *)
        (Artifacts, Master_branch.job_static_x86_64);
        (Artifacts, Code_verification.job_build_x86_64_extra_exp Before_merging);
        (Artifacts, Code_verification.job_build_x86_64_extra_dev Before_merging);
        (* No need for kernels for this job. *)
      ]
    ~test_selection:(tests_tag_selector [Has_tag "cli"; Not (Has_tag "flaky")])
    ~parallel_tests:3
    ~before_script:["mv octez-binaries/x86_64/octez-* ."]

let register () =
  CI.register_before_merging_jobs
    [
      (Auto, job_tezt `merge_request);
      (Auto, job_tezt_time_sensitive `merge_request);
      (Manual, job_tezt_riscv_slow_sequential `merge_request);
      (Manual, job_tezt_slow `merge_request);
      (Manual, job_tezt_extra `merge_request);
      (Manual, job_tezt_flaky `merge_request);
      (Auto, job_tezt_static_binaries `merge_request);
    ] ;
  CI.register_schedule_extended_test_jobs
    [
      (Auto, job_tezt `scheduled);
      (Auto, job_tezt_time_sensitive `scheduled);
      (Auto, job_tezt_riscv_slow_sequential `scheduled);
      (Auto, job_tezt_slow `scheduled);
      (Auto, job_tezt_extra `scheduled);
      (Auto, job_tezt_flaky `scheduled);
      (Auto, job_tezt_static_binaries `scheduled);
    ] ;
  CI.register_custom_extended_test_jobs
    [
      (Auto, job_tezt `scheduled);
      (Auto, job_tezt_time_sensitive `scheduled);
      (Auto, job_tezt_slow `scheduled);
      (Auto, job_tezt_extra `scheduled);
      (Auto, job_tezt_flaky `scheduled);
    ] ;
  ()

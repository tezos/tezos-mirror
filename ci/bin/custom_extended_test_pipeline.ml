(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs of the [custom_extended_test_pipeline]
   pipeline.

   This generic pipeline aims to be used to run all tests of the code
   base with some particular options.  The goal of this pipeline is to
   test behaviours that are not enabled by default on the node, and
   thus, not tested through the tezt jobs. To do so, a particular
   configuration is set for each node that is run in the tezt jobs so
   that the feature to test is enabled.

   Currently, it is used for:
   - schedule_extended_rpc_tests: test the external RPC server
   - schedule_extended_validation_tests: test the singleprocess
     validation
*)

open Gitlab_ci.Types
open Tezos_ci
open Common

(* The build_x86_64 jobs are split in two to keep the artifact size
   under the 1GB hard limit set by GitLab. *)
(* [job_build_x86_64_release] builds the released executables. *)
let job_build_x86_64_release =
  job_build_dynamic_binaries ~__POS__ ~arch:Amd64 ~release:true ()

let job_build_x86_64_exp_dev_extra =
  job_build_dynamic_binaries ~__POS__ ~arch:Amd64 ~release:false ()

let jobs =
  (* Tezt jobs.

     This is intended to execute the same jobs as those performed in
     the [code_verification.ml] file -- see the [jobs_tezt] function
     comment to get more details.

     Note that this function must be in sync with the
     [Code_verification.jobs_tezt] one, so that the same set of test
     is executed.
  *)
  let job_tezt_fetch_records = Tezt.job_tezt_fetch_records () in
  let job_build_kernels = job_build_kernels () in
  let dependencies =
    Dependent
      [
        Artifacts job_build_x86_64_release;
        Artifacts job_build_x86_64_exp_dev_extra;
        Artifacts job_build_kernels;
        Artifacts job_tezt_fetch_records;
      ]
  in
  let tezt : tezos_job =
    Tezt.job
      ~__POS__
      ~name:"tezt"
        (* Exclude all tests with tags in [tezt_tags_always_disable] or
           [tezt_tags_exclusive_tags]. *)
      ~tezt_tests:(Tezt.tests_tag_selector [Not (Has_tag "flaky")])
      ~tezt_parallel:6
      ~parallel:(Vector 50)
      ~timeout:(Minutes 40)
      ~dependencies
      ()
  in
  let tezt_time_sensitive : tezos_job =
    (* the following tests are executed with [~tezt_parallel:1] to ensure
       that other tests do not affect their executions. However, these
       tests are not particularly cpu/memory-intensive hence they do not
       need to run on a particular machine contrary to performance
       regression tests. *)
    Tezt.job
      ~__POS__
      ~name:"tezt-time-sensitive"
      ~tezt_tests:(Tezt.tests_tag_selector ~time_sensitive:true [])
      ~tezt_variant:"-time_sensitive"
      ~dependencies
      ()
  in
  let tezt_slow : tezos_job =
    Tezt.job
      ~__POS__
      ~name:"tezt-slow"
      ~tezt_tests:
        (Tezt.tests_tag_selector
           ~slow:true
           (* TODO: https://gitlab.com/tezos/tezos/-/issues/7063
              The deselection of Paris [test_adaptive_issuance_launch.ml]
              should be removed once the fixes to its slowness has been
              snapshotted from Alpha. *)
           [
             Not
               (String_predicate
                  ( File,
                    Is
                      "src/proto_019_PtParisA/lib_protocol/test/integration/test_adaptive_issuance_launch.ml"
                  ));
           ])
      ~tezt_variant:"-slow"
      ~retry:2
      ~tezt_parallel:3
      ~parallel:(Vector 20)
      ~dependencies
      ()
  in
  let tezt_flaky : tezos_job =
    (* Runs tests tagged "flaky" [Tag.flaky].

       These tests only run on scheduled pipelines. They run with
       higher retries (both GitLab CI job retries, and tezt
       retries). They also run with [~parallel:1] to increase
       stability. *)
    Tezt.job
      ~__POS__
      ~name:"tezt-flaky"
      ~tezt_tests:(Tezt.tests_tag_selector [Has_tag "flaky"])
      ~tezt_variant:"-flaky"
        (* To handle flakiness, consider tweaking [~tezt_parallel] (passed to
           Tezt's '--job-count'), and [~tezt_retry] (passed to Tezt's
           '--retry') *)
      ~retry:2
      ~tezt_retry:3
      ~tezt_parallel:1
      ~dependencies
      ~allow_failure:Yes
      ()
  in
  [
    tezt;
    tezt_time_sensitive;
    tezt_slow;
    tezt_flaky;
    job_build_x86_64_release;
    job_build_x86_64_exp_dev_extra;
    job_build_kernels;
    job_tezt_fetch_records;
    job_datadog_pipeline_trace;
  ]

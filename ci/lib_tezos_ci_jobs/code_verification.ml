(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2024-2025 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs of the [code_verification] pipeline.

   This pipeline comes in two variants:

   - The [before_merging] pipeline runs on merge requests. Jobs in
   this pipeline are conditional on the set of [~changes] in the merge
   request. The goal is to only run those jobs whose outcome is
   affected by the merge request.

   - The [schedule_extended_test] pipeline runs daily on the [master]
   branch. It contains a set of jobs that are too slow to run in merge
   request pipelines and a large subset of the [before_merging]
   pipeline in addition. This subset excludes jobs that are irrelevant
   in a non-merge request context, like the commit title check. Jobs
   in this pipeline should run [Always] -- unless they depend on
   artifacts of another job in which case they should run
   [On_success]. The goal of this pipeline is to catch any breaking
   changes that might've slipped through the [before_merging]
   pipeline.

   When adding new jobs to the [code_verification] pipeline, make sure
   that it appears in both variants as applicable, with the
   appropriate rules. *)

open Gitlab_ci.Types
open Gitlab_ci.Util

(** Variants of the code verification pipeline.

    Encodes the conditional [before_merging] pipeline, the [merge_train]
    and its unconditional variant [schedule_extended_test]. *)
type code_verification_pipeline =
  | Before_merging
  | Schedule_extended_test
  | Merge_train

(* Define the [start] job.

   The purpose of this job is to implement a manual trigger
   for [Before_merging] pipelines, instead of running it on
   each update to the merge request. *)
let job_start =
  Tezos_ci.job
    ~__POS__
    ~image:Tezos_ci.Images.datadog_ci
    ~stage:Tezos_ci.Stages.start
    ~rules:[job_rule ~allow_failure:No ~when_:Manual ()]
    ~timeout:(Minutes 10)
    ~name:"trigger"
    [
      "echo 'Trigger pipeline!'";
      "CI_MERGE_REQUEST_IID=${CI_MERGE_REQUEST_IID:-none}";
      "DATADOG_SITE=datadoghq.eu datadog-ci tag --level pipeline --tags \
       pipeline_type:$PIPELINE_TYPE --tags mr_number:$CI_MERGE_REQUEST_IID";
    ]

(* Encodes the conditional [before_merging] pipeline and its unconditional variant
   [schedule_extended_test]. *)
let jobs pipeline_type =
  match pipeline_type with
  | Schedule_extended_test | Merge_train ->
      [Tezos_ci.job_datadog_pipeline_trace]
  | Before_merging -> [job_start]

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
open Tezos_ci
open Changesets

(** Variants of the code verification pipeline.

    Encodes the conditional [before_merging] pipeline, the [merge_train]
    and its unconditional variant [schedule_extended_test]. *)
type code_verification_pipeline =
  | Before_merging
  | Schedule_extended_test
  | Merge_train

let code_verification_pipeline_name = function
  | Before_merging -> "before_merging"
  | Schedule_extended_test -> "schedule_extended_test"
  | Merge_train -> "merge_train"

(** Configuration of manual jobs for [make_rules] *)
type manual =
  | No  (** Do not add rule for manual start. *)
  | Yes  (** Add rule for manual start. *)
  | On_changes of Changeset.t  (** Add manual start on certain [changes:] *)

(* [make_rules] makes rules for jobs that are:
   - automatic in scheduled pipelines;
   - conditional in [before_merging] pipelines.

   If a job has non-optional dependencies, then [dependent] must be
   set to [true] to ensure that we only run the job in case previous
   jobs succeeded (setting [when: on_success]).

   If [label] is set, add rule that selects the job in
   [Before_merging] pipelines for merge requests with the given
   label. Rules for manual start can be configured using [manual].

   If [label], [changes] and [manual] are omitted, then rules will
   enable the job [On_success] in the [before_merging] pipeline. This
   is safe, but prefer specifying a [changes] clause if possible.

   If [final_pipeline_disable] is set to true (default false), this job is
   disabled in final [Before_merging] pipelines. *)
let make_rules ~pipeline_type ?label ?changes ?(manual = No)
    ?(dependent = false) ?(final_pipeline_disable = false) () =
  match pipeline_type with
  | Schedule_extended_test ->
      (* The scheduled pipeline always runs all jobs unconditionally
           -- unless they are dependent on a previous job (which is
           not [job_start] defined below), in the pipeline. *)
      [job_rule ~when_:(if dependent then On_success else Always) ()]
  | Before_merging | Merge_train -> (
      (* MR labels can be used to force tests to run. *)
      (if final_pipeline_disable then
         [job_rule ~if_:Rules.is_final_pipeline ~when_:Never ()]
       else [])
      @ (match label with
        | Some label ->
            [job_rule ~if_:Rules.(has_mr_label label) ~when_:On_success ()]
        | None -> [])
      (* Modifying some files can force tests to run. *)
      @ (match changes with
        | None -> []
        | Some changes ->
            [job_rule ~changes:(Changeset.encode changes) ~when_:On_success ()])
      (* It can be relevant to start some jobs manually. *)
      @
      match manual with
      | No -> []
      | Yes -> [job_rule ~when_:Manual ()]
      | On_changes changes ->
          [job_rule ~when_:Manual ~changes:(Changeset.encode changes) ()])

(* Define the [start] job.

   The purpose of this job is to implement a manual trigger
   for [Before_merging] pipelines, instead of running it on
   each update to the merge request. *)
let job_start =
  job
    ~__POS__
    ~image:Images.datadog_ci
    ~stage:Stages.start
    ~rules:[job_rule ~allow_failure:No ~when_:Manual ()]
    ~timeout:(Minutes 10)
    ~name:"trigger"
    [
      "echo 'Trigger pipeline!'";
      "CI_MERGE_REQUEST_IID=${CI_MERGE_REQUEST_IID:-none}";
      "DATADOG_SITE=datadoghq.eu datadog-ci tag --level pipeline --tags \
       pipeline_type:$PIPELINE_TYPE --tags mr_number:$CI_MERGE_REQUEST_IID";
    ]

(* Short-cut for jobs that have no dependencies except [job_start] on
   [Before_merging] pipelines. *)
let dependencies_needs_start pipeline_type =
  match pipeline_type with
  | Before_merging -> Dependent [Job job_start]
  | Schedule_extended_test | Merge_train -> Dependent []

(* Use this function to define jobs that depend on the pipeline type.
   Without this function, you risk defining the same job multiple times
   for the same pipeline type. *)
let depending_on_pipeline_type :
    (code_verification_pipeline -> 'a) -> code_verification_pipeline -> 'a =
  (* Same as [Cacio.parameterize]: this is just a memoization function.
     We just specialize its type to make it more clear what we are doing. *)
  Cacio.parameterize

let build_arm_rules ~pipeline_type =
  make_rules ~pipeline_type ~label:"ci--arm64" ~manual:Yes ()

(* Encodes the conditional [before_merging] pipeline and its unconditional variant
   [schedule_extended_test]. *)
let jobs pipeline_type =
  let make_rules = make_rules ~pipeline_type in
  (* Stages *)
  let start_stage =
    match pipeline_type with
    | Schedule_extended_test | Merge_train -> [job_datadog_pipeline_trace]
    | Before_merging -> [job_start]
  in

  let dependencies_needs_start = dependencies_needs_start pipeline_type in

  (* Test jobs*)
  let test =
    let create_job =
      Homebrew.make_job_create_homebrew_formula
        ~rules:(make_rules ~manual:No ~changes:changeset_homebrew ())
        ~dependencies:dependencies_needs_start
        ()
    in
    match pipeline_type with
    | Before_merging | Merge_train -> [create_job]
    | Schedule_extended_test -> []
  in

  start_stage @ test

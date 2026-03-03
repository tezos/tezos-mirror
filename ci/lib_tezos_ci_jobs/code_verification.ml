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
open Common.Docker
open Common.Build
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

  (* Used in trigger job definitions. For code verification pipelines,
     add type of parent pipeline. This definition shadows
     [Tezos_ci.trigger_job]. *)
  let trigger_job ~__POS__ ?rules ?dependencies ?description ?variables
      child_pipeline_path =
    trigger_job
      ~__POS__
      ?rules
      ?dependencies
      ?description
      ?variables
      ~parent_pipeline_name:(code_verification_pipeline_name pipeline_type)
      child_pipeline_path
  in
  let dependencies_needs_start = dependencies_needs_start pipeline_type in

  (* Octez static binaries *)
  let job_static_x86_64_experimental =
    job_build_static_binaries
      ~__POS__
      ~arch:Amd64
      ~cpu:Very_high
      ~storage:Ramfs
        (* Even though not many tests depend on static executables, some
         of those that do are limiting factors in the total duration
         of pipelines. So we start this job as early as possible,
         without waiting for sanity_ci. *)
      ~dependencies:dependencies_needs_start
      ~rules:(make_rules ~changes:changeset_octez ())
      ()
  in
  let job_static_arm64_experimental =
    job_build_static_binaries
      ~__POS__
      ~arch:Arm64
      ~storage:Ramfs
      ~dependencies:dependencies_needs_start (* See rationale above *)
      ~rules:(make_rules ~manual:(On_changes changeset_octez) ())
      ()
  in

  (* Build jobs *)
  let build =
    [
      job_static_x86_64_experimental;
      job_static_arm64_experimental;
      job_build_layer1_profiling
        ~rules:(make_rules ~changes:changeset_octez ())
        ();
    ]
  in

  (* Test jobs*)
  let test =
    (* This job triggers the debian child pipeline automatically if any
       files in the changeset is modified. It's the same as
       job_debian_repository_trigger that can be run manually.
        We want both:
       - to trigger the debian repository test automatically when relevant
          files change, if the whole pipeline was triggered;
       - and to be able to trigger the debian repository test manually,
          whether relevant files changed or not, without having to trigger the
          whole pipeline.
         To achieve that we need to duplicate the job, because
         it needs two different sets of dependencies. *)
    let job_debian_repository_trigger_auto =
      trigger_job
        ~__POS__
        ~rules:(make_rules ~manual:No ~changes:changeset_debian_packages ())
        ~stage:Stages.test
        ~dependencies:dependencies_needs_start
        Debian_repository.child_pipeline_partial_auto
    in

    let job_homebrew_trigger_auto =
      trigger_job
        ~__POS__
        ~rules:(make_rules ~manual:No ~changes:changeset_homebrew ())
        ~stage:Stages.test
        ~dependencies:dependencies_needs_start
        Homebrew.child_pipeline_full_auto
    in

    match pipeline_type with
    | Before_merging | Merge_train ->
        [job_debian_repository_trigger_auto; job_homebrew_trigger_auto]
    | Schedule_extended_test -> []
  in

  (* Manual jobs *)
  let manual =
    (* On scheduled pipelines we build and test the full packages test matrix.
       On [Before_merging] pipelines only a subset of the packages are built
       and tested. There is a similar job job_debian_repository_trigger_auto
       in the test stage that is started automatically if any files related to
       packaging is changed. *)
    let job_debian_repository_trigger_partial : tezos_job =
      (* Same as [job_debian_repository_trigger_auto] but manual,
         so that one can trigger it without triggering the whole main pipeline.
         See comment near the definition of [job_debian_repository_trigger_auto]. *)
      trigger_job
        ~__POS__
        ~rules:(make_rules ~manual:Yes ())
        ~dependencies:(Dependent [])
        ~stage:Stages.manual
        Debian_repository.child_pipeline_partial
    in

    let job_homebrew_repository_trigger : tezos_job =
      (* We leave the possibility to run this pipeline manually, in particular
         to generate the formula on scheduled pipelines *)
      trigger_job
        ~__POS__
        ~rules:(make_rules ~manual:Yes ())
        ~dependencies:(Dependent [])
        ~stage:Stages.manual
        Homebrew.child_pipeline_full
    in
    let job_base_images_trigger =
      trigger_job
        ~__POS__
        ~rules:(make_rules ~manual:Yes ())
        ~stage:Stages.manual
        ~variables:[("DOCKER_FORCE_BUILD", "true")]
        ~dependencies:(Dependent [])
        Base_images.child_pipeline
    in
    let security_scan_trigger =
      trigger_job
        ~__POS__
        ~rules:(make_rules ~manual:Yes ())
        ~stage:Stages.manual
        ~dependencies:(Dependent [])
        Security_scans.child_pipeline
    in

    match pipeline_type with
    | Before_merging | Merge_train ->
        (* Note: manual jobs in stage [manual] (which is the final
             stage) in [Before_merging] pipelines should be [Dependent]
             by default, and in particular [Dependent []] if they have
             no need for artifacts from other jobs. Making these
             dependent on [job_start] is redundant since they are
             already manual, and what's more, puts the pipeline in a
             confusing "pending state" with a yellow "pause" icon on the
             [manual] stage. *)
        let job_docker_amd64_test_manual : Tezos_ci.tezos_job =
          job_docker_build
            ~__POS__
            ~arch:Amd64
            ~dependencies:(Dependent [])
            ~rules:(make_rules ~changes:changeset_docker_files ~manual:Yes ())
            Test_manual
        in
        let job_docker_arm64_test_manual : Tezos_ci.tezos_job =
          job_docker_build
            ~__POS__
            ~arch:Arm64
            ~storage:Ramfs
            ~dependencies:(Dependent [])
            ~rules:(make_rules ~changes:changeset_docker_files ~manual:Yes ())
            Test_manual
        in
        let job_docker_verify_test_amd64 : tezos_job =
          job_docker_authenticated
            ~__POS__
            ~name:"oc.script.docker_verify_image_amd64"
            ~stage:Stages.manual
            ~variables:[("IMAGE_ARCH_PREFIX", "amd64_")]
            ~rules:(make_rules ~manual:Yes ())
            ~dependencies:(Dependent [Job job_docker_amd64_test_manual])
            ["./scripts/ci/docker_verify_signature.sh"]
        in
        let job_docker_verify_test_arm64 : tezos_job =
          job_docker_authenticated
            ~__POS__
            ~name:"oc.script.docker_verify_image_arm64"
            ~stage:Stages.manual
            ~variables:[("IMAGE_ARCH_PREFIX", "arm64_")]
            ~rules:(make_rules ~manual:Yes ())
            ~dependencies:(Dependent [Job job_docker_arm64_test_manual])
            ["./scripts/ci/docker_verify_signature.sh"]
        in
        let jobs =
          [
            job_docker_amd64_test_manual;
            job_docker_arm64_test_manual;
            job_docker_verify_test_arm64;
            job_docker_verify_test_amd64;
          ]
        in
        if pipeline_type = Merge_train then jobs
        else
          [
            job_homebrew_repository_trigger;
            job_debian_repository_trigger_partial;
            job_base_images_trigger;
            security_scan_trigger;
          ]
          @ jobs
    (* No manual jobs on the scheduled pipeline *)
    | Schedule_extended_test -> []
  in
  start_stage @ build @ test @ manual
  @
  (* base image build jobs. *)
  match pipeline_type with
  (* In [before_merging] parent pipeline, base image jobs should start only
       if [trigger] is started and if the changesets are touched. *)
  | Before_merging -> Base_images.jobs ~start_job:job_start ~changeset:true ()
  (* In [merge_train] pipeline, base image jobs should start as early as
       as possible, but only if the changesets are touched. *)
  | Merge_train -> Base_images.jobs ~changeset:true ()
  (* Not added in [schedule_extended_test]
       as they run nightly in [base_images.daily]. *)
  | Schedule_extended_test -> []

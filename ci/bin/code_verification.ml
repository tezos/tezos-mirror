(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

open Gitlab_ci
open Gitlab_ci.Types
open Gitlab_ci.Util
open Tezos_ci
open Common

(* Encodes the conditional [before_merging] pipeline and its unconditional variant
   [schedule_extended_test]. *)
type code_verification_pipeline = Before_merging | Schedule_extended_test

(* Encodes the conditional [before_merging] pipeline and its unconditional variant
   [schedule_extended_test]. *)
let jobs pipeline_type =
  (* Externalization *)
  let job_external_split ?(before_merging_suffix = "before_merging")
      ?(scheduled_suffix = "scheduled_extended_test") job =
    job_external
      ~filename_suffix:
        (match pipeline_type with
        | Before_merging -> before_merging_suffix
        | Schedule_extended_test -> scheduled_suffix)
      job
  in
  (* [make_rules] makes rules for jobs that are:
     - automatic in scheduled pipelines;
     - conditional in [before_merging] pipelines.

     If [label], [changes] and [manual] are omitted, then rules will
     enable the job [On_success] in the [before_merging]
     pipeline. This is safe, but prefer specifying a [changes] clause
     if possible. *)
  let make_rules ?label ?changes ?(manual = false) () =
    match pipeline_type with
    | Schedule_extended_test ->
        (* The scheduled pipeline always runs all tests unconditionally. *)
        [job_rule ~when_:Always ()]
    | Before_merging ->
        (* MR labels can be used to force tests to run. *)
        (match label with
        | Some label ->
            [job_rule ~if_:Rules.(has_mr_label label) ~when_:On_success ()]
        | None -> [])
        (* Modifying some files can force tests to run. *)
        @ (match changes with
          | None -> []
          | Some changes -> [job_rule ~changes ~when_:On_success ()])
        (* For some tests, it can be relevant to have a manual trigger. *)
        @ if manual then [job_rule ~when_:Manual ()] else []
  in
  (* Stages *)
  (* All stages should be empty, as explained below, until the full pipeline is generated. *)
  let trigger, dependencies_needs_trigger =
    match pipeline_type with
    | Schedule_extended_test -> ([], Staged [])
    | Before_merging ->
        (* Define the [trigger] job

           §1: The purpose of this job is to launch the CI manually in certain cases.
           The objective is not to run computing when it is not
           necessary and the decision to do so belongs to the developer

           §2: We also perform some fast sanity checks. *)
        let job_trigger =
          job
            ~__POS__
            ~image:Images.alpine
            ~stage:Stages.trigger
            ~allow_failure:No
            ~rules:
              [
                job_rule
                  ~if_:(If.not Rules.assigned_to_marge_bot)
                  ~allow_failure:No
                  ~when_:Manual
                  ();
                job_rule ~when_:Always ();
              ]
            ~timeout:(Minutes 10)
            ~name:"trigger"
            [
              "echo 'Trigger pipeline!'";
              (* Check that [.gitlab-ci.yml]'s [build_deps_image_version] and
                 [scripts/version.sh]'s [opam_repository_tag] are the same. *)
              "./scripts/ci/check_opam_repository_tag.sh";
              (* Check that the Alpine version of the trigger job's image
                 corresponds to the value in scripts/version.sh. *)
              "./scripts/ci/check_alpine_version.sh";
            ]
          |> job_external
        in
        (* TODO: put job_trigger here when full pipeline is generated *)
        ([], Dependent [Optional job_trigger])
  in
  let sanity = [] in
  let job_docker_rust_toolchain =
    job_docker_rust_toolchain
      ~__POS__
      ~rules:(make_rules ~changes:changeset_octez_or_kernels ~manual:true ())
      ~dependencies:dependencies_needs_trigger
      ()
    |> job_external_split
  in
  let build =
    let build_arm_rules = make_rules ~label:"ci--arm64" ~manual:true () in
    let _job_build_arm64_release : Tezos_ci.tezos_job =
      job_build_arm64_release ~rules:build_arm_rules () |> job_external_split
    in
    let _job_build_arm64_exp_dev_extra : Tezos_ci.tezos_job =
      job_build_arm64_exp_dev_extra ~rules:build_arm_rules ()
      |> job_external_split
    in
    let _job_static_x86_64_experimental =
      job_build_static_binaries
        ~__POS__
        ~arch:Amd64
          (* Even though not many tests depend on static executables, some
             of those that do are limiting factors in the total duration
             of pipelines. So we start this job as early as possible,
             without waiting for sanity_ci. *)
        ~dependencies:dependencies_needs_trigger
        ~rules:(make_rules ~changes:changeset_octez ())
        ()
      |> job_external_split
    in
    (* TODO: The code is a bit convulted here because these jobs are
       either in the build or in the manual stage depeneding on the
       pipeline type. However, we can put them in the build stage on
       [before_merging] pipelines as long as we're careful to put
       [allow_failure: true]. *)
    (match pipeline_type with
    | Schedule_extended_test ->
        let _job_build_dpkg_amd64 = job_build_dpkg_amd64 () |> job_external in
        let _job_build_rpm_amd64 = job_build_rpm_amd64 () |> job_external in
        ()
    | Before_merging -> ()) ;
    (* TODO: include the jobs defined above when full pipeline is generated *)
    []
  in
  let packaging = [] in
  let test = [] in
  let doc = [] in
  let manual =
    match pipeline_type with
    | Before_merging ->
        let _job_docker_amd64_test_manual : Tezos_ci.tezos_job =
          job_docker_build
            ~__POS__
            ~external_:true
            ~dependencies:(Dependent [Artifacts job_docker_rust_toolchain])
            ~arch:Amd64
            Test_manual
        in
        let _job_docker_arm64_test_manual : Tezos_ci.tezos_job =
          job_docker_build
            ~__POS__
            ~external_:true
            ~dependencies:(Dependent [Artifacts job_docker_rust_toolchain])
            ~arch:Arm64
            Test_manual
        in
        let _job_build_dpkg_amd64_manual =
          job_build_bin_package
            ~__POS__
            ~name:"oc.build:dpkg:amd64"
            ~target:Dpkg
            ~arch:Tezos_ci.Amd64
            ~rules:[job_rule ~when_:Manual ()]
            ~stage:Stages.manual
            ()
          |> job_external ~directory:"build" ~filename_suffix:"manual"
        in
        let _job_build_rpm_amd64_manual =
          job_build_bin_package
            ~__POS__
            ~rules:[job_rule ~when_:Manual ()]
            ~name:"oc.build:rpm:amd64"
            ~target:Rpm
            ~arch:Tezos_ci.Amd64
            ~stage:Stages.manual
            ()
          |> job_external ~directory:"build" ~filename_suffix:"manual"
        in
        (* TODO: include the jobs defined above when full pipeline is generated *)
        []
    (* No manual jobs on the scheduled pipeline *)
    | Schedule_extended_test -> []
  in
  (* Empty placeholder: this has the effect of not overwriting the pipeline file in question.
     Once all the jobs in these pipelines are defined, we will return them here which
     will cause the pipeline files to contain the definition of all those jobs.

     Until that time, all the jobs are written ot external files
     (using {!job_external} or {!jobs_external}) and included by hand
     in the files [.gitlab/ci/pipelines/before_merging.yml] and
     [.gitlab/ci/pipelines/schedule_extended_test.yml]. *)
  trigger @ sanity @ build @ packaging @ test @ doc @ manual

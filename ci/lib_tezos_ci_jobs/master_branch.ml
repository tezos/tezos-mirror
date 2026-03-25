(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs of the [master_branch] pipeline.

   This pipeline runs for each merge on the [master] branch. To goal
   of this pipeline is to publish artifacts for the latest development
   version of Octez (e.g. the latest version on 'master'), including:

   - docker images,
   - static binaries, and
   - documentation. *)

open Common.Docker
open Gitlab_ci.Util
open Tezos_ci

let rules_always = [job_rule ~when_:Always ()]

let job_docker_amd64_experimental : tezos_job =
  job_docker_build ~__POS__ ~rules:rules_always ~arch:Amd64 Experimental

let job_docker_arm64_experimental : tezos_job =
  job_docker_build
    ~__POS__
    ~rules:rules_always
    ~arch:Arm64
    ~storage:Ramfs
    Experimental

let job_docker_merge_manifests =
  job_docker_merge_manifests
    ~__POS__
    ~ci_docker_hub:true
    ~job_docker_amd64:job_docker_amd64_experimental
    ~job_docker_arm64:job_docker_arm64_experimental

let octez_distribution_docker_jobs =
  [
    (* Stage: build *)
    job_docker_amd64_experimental;
    job_docker_arm64_experimental;
    (* Stage: prepare_release *)
    job_docker_merge_manifests;
  ]

(* Defines the jobs of the [schedule_docker_build_pipeline] pipeline.

    This pipeline runs scheduled on the [master] branch. The goal
   of this pipeline is to publish fresh Docker images for 'master' using the latest Alpine packages. *)

let jobs =
  (* Like in the {!Schedule_extended_test} variant of
     {!Code_verification} pipelines, we'd like to run as many jobs as
     possible in [master_branch] pipelines. Therefore, the default
     [when_] should be [Always] for jobs without dependencies.

     [changes:] clauses can be used on [master_branch] pipelines. In
     push pipelines like this one, [changes:]-clauses match against
     the diff of the branch's [HEAD] between two pushes. That is, the
     the difference between the previous state and the new state of
     the branch. In the case of the [master] branch, this is just the
     contents of the most recently merged MR. For more info on
     [changes:] in different pipelines, see
     {{:https://docs.gitlab.com/ee/ci/jobs/job_troubleshooting.html#jobs-or-pipelines-run-unexpectedly-when-using-changes}
     GitLab Docs: Jobs or pipelines run unexpectedly when using changes}. *)
  [(* Stage: sanity *) job_datadog_pipeline_trace]
  (* Jobs to build and update on Docker Hub the Octez Docker image.  *)
  @ octez_distribution_docker_jobs

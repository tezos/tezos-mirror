(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Definition of the global pipelines of this repository.

   Global pipelines are pipelines that are shared between components:
   components add jobs to them with [Cacio.register_jobs], and no single
   component owns them. They are declared here, and registered with CIAO by
   [Cacio.close].

   This library exists so that Cacio itself does not have to know about the
   pipelines of this particular repository. It cannot be merged into
   [ci/lib_tezos_ci_jobs], which would be the natural place for it, because
   [tezos_ci_jobs] depends on the component libraries (release_site_ci,
   grafazos_ci, teztale_ci, rollup_node_ci and sdk_bindings_ci) while those
   components need to add jobs to global pipelines, i.e. they need to depend
   on this library. This dependency of [tezos_ci_jobs] on components could be
   removed, in which case this library could be merged into it. *)

open Gitlab_ci
open Tezos_ci

(* The "custom extended test" pipelines test the codebase with some particular options.
   This allows testing behaviors that are not enabled by default on the node,
   and are thus not tested in Tezt jobs of [before_merging] pipelines in particular.

   Note: this method is simple to implement but is not ideal.
   It duplicates a lot of tests that do not actually contribute
   to testing the special options, and there is no reason why at least some tests
   could be run with special options in dedicated jobs in [before_merging] pipelines. *)

(* All jobs in scheduled pipelines have "interruptible: false"
   to prevent them from being canceled after a push to master.
   Instead of modifying the definition of each job, we override the value
   by passing [~interruptible_pipeline:false] to [new_global_pipeline]. *)

let before_merging =
  Cacio.new_global_pipeline
    "before_merging"
    (lazy Rules.(If.(merge_request && not merge_train)))
    ~with_job_trigger:true
    ~with_condition:true
    ~with_datadog_pipeline_trace:false
    ~description:
      "Lints code in merge requests, checks that it compiles and runs tests.\n\n\
       This pipeline is created on each push to a branch with an associated \
       open merge request, typically by the developer. It runs sanity checks, \
       linters and checks that code of the MR compiles and that the tests \
       pass. Must be manually started through the job 'trigger'."

let merge_train =
  Cacio.new_global_pipeline
    "merge_train"
    (lazy Rules.(If.(on_tezos_namespace && merge_request && merge_train)))
    ~with_condition:true
    ~allow_manual_jobs:false
    ~auto_cancel:Types.{on_job_failure = true; on_new_commit = false}
    ~description:
      "A merge-train-specific version of 'before_merging'.\n\n\
       This pipeline contains the same set of jobs as 'before_merging' but \
       with auto-cancelling enabled on job failures. That is, if one job in \
       the pipeline fails, the full pipeline is cancelled. This ensures that \
       pipelines running in a merge train, that are bound to fail (due to some \
       failing job), does so as early as possible. This prevents unneccessary \
       delays in merging MRs further down the train.\n\n\
       The merge train pipeline is created by GitLab when a merge request is \
       added to the merge train (typically by marge-bot)."

(* Add jobs to both the [before_merging] and the [merge_train] pipelines.

   Manual jobs are only added to [before_merging]: [merge_train] must finish
   ASAP so as to not block other MRs, so it does not make sense to have to wait
   on a manual action there. Manual, allowed to fail jobs would not block the
   pipeline, but they are better reserved for other pipelines. *)
let register_merge_request_jobs jobs =
  Cacio.register_jobs before_merging jobs ;
  let non_manual_jobs =
    List.filter
      (fun (trigger, _) ->
        match trigger with
        | Cacio.Manual -> false
        | Cacio.Auto | Cacio.Immediate -> true)
      jobs
  in
  Cacio.register_jobs merge_train non_manual_jobs

let master_branch =
  Cacio.new_global_pipeline
    "master_branch"
    (lazy Rules.(If.(on_tezos_namespace && push && on_branch "master")))
    ~interruptible_publish:true
    ~description:
      "Publishes artifacts (docs, static binaries) from master on each merge.\n\n\
       This pipeline publishes the documentation at tezos.gitlab.io, builds \
       static binaries, and the 'master' tag of the Octez Docker distribution. \
       This pipeline is created automatically by GitLab on each push, \
       typically resulting from the merge of a merge request, to the 'master' \
       branch on tezos/tezos."

(* Matches Octez major release tags, e.g. [octez-v1.0] or [octez-v2.0-rc4]. *)
let octez_major_release_tag_re = "/^octez-v\\d+\\.0(?:\\-rc\\d+)?$/"

(* Matches Octez minor release tags, e.g. [octez-v1.2]. *)
let octez_minor_release_tag_re = "/^octez-v\\d+\\.[1-9][0-9]*$/"

(* Matches Octez beta release tags, e.g. [octez-v1.2-beta5]. *)
let octez_beta_release_tag_re = "/^octez-v\\d+\\.\\d+\\-beta\\d*$/"

(* Matches Octez packaging revision tags, e.g. [octez-v1.0-2]. *)
let octez_packaging_revision_tag_re = "/^octez-v\\d+\\.\\d+\\-\\d+$/"

(* Matches either Octez release tags or Octez beta release tags,
   e.g. [octez-v1.2], [octez-v1.2-rc4] or [octez-v1.2-beta5]. *)
let octez_release_tags =
  [
    octez_major_release_tag_re;
    octez_minor_release_tag_re;
    octez_beta_release_tag_re;
  ]

let has_any_tag tags =
  match List.map Rules.has_tag_match tags with
  | [] ->
      (* We could return [Rules.never], but this looks like a programming mistake. *)
      invalid_arg "has_any_tag: empty list"
  | [tag] -> tag
  | head :: tail -> List.fold_left If.( || ) head tail

(* Lazy: [Cacio.get_release_tag_rexes] only knows all release tags once every
   component has declared its release pipelines. *)
let has_non_release_tag =
  lazy
    (let release_tags =
       octez_release_tags
       @ [octez_packaging_revision_tag_re]
       @ Cacio.get_release_tag_rexes ()
     in
     If.(
       Predefined_vars.ci_commit_tag != null && not (has_any_tag release_tags)))

let release_description =
  "\n\n\
   For more information on Octez' release system, see: \
   https://octez.tezos.com/docs/releases/releases.html"

(* TODO: rename 'octez_docker_latest_release' ?? *)
let octez_latest_release =
  Cacio.new_global_pipeline
    "octez_latest_release"
    (lazy Rules.(If.(on_tezos_namespace && push && on_branch "latest-release")))
    ~description:
      ("Updates 'latest' tag of the Octez Docker distribution on Docker Hub.\n\n\
        This pipeline is created on each push to the 'latest-release' branch \
        of 'tezos/tezos', typically performed by the release manager. On each \
        release, the 'latest-release' branch is updated to point to the git \
        tag of the release. This resulting pipeline then updates the Docker \
        tag 'latest' of the Octez Docker distribution published to Docker hub \
        (https://hub.docker.com/r/tezos/tezos) to point to the Docker release \
        associated with the git tag pushed to the 'latest-release' branch."
     ^ release_description)

let octez_latest_release_test =
  Cacio.new_global_pipeline
    "octez_latest_release_test"
    (lazy
      Rules.(
        If.(not_on_tezos_namespace && push && on_branch "latest-release-test")))
    ~description:
      "Dry-run pipeline for 'octez_latest_release' pipelines.\n\n\
       This pipeline is used to dry run the 'octez_latest_release' pipeline, \
       checking that it works as intended, without updating any Docker tags. \
       Developers or release managers trigger it manually by pushing to the \
       branch 'latest-release-test' of a fork of 'tezos/tezos', e.g. to the \
       'nomadic-labs/tezos' project."

(* TODO: simplify dry run pipelines by having them all be on tezos/tezos? *)
let octez_major_release_tag =
  Cacio.new_global_pipeline
    "octez_major_release_tag"
    (lazy
      Rules.(
        If.(
          on_tezos_namespace && push && has_tag_match octez_major_release_tag_re)))
    ~variables:[("DOCKER_FORCE_BUILD", "true")]
    ~description:
      ("Release tag pipelines for major Octez release.\n\n\
        This pipeline is created when the release manager pushes a tag in the \
        format octez-vX.0(-rcN).\n\
        Publishes release assets for all the components of Octez."
     ^ release_description)

let octez_minor_release_tag =
  Cacio.new_global_pipeline
    "octez_minor_release_tag"
    (lazy
      Rules.(
        If.(
          on_tezos_namespace && push && has_tag_match octez_minor_release_tag_re)))
    ~variables:[("DOCKER_FORCE_BUILD", "true")]
    ~description:
      ("Release tag pipelines for minor Octez release.\n\n\
        This pipeline is created when the release manager pushes a tag in the \
        format octez-vX.Y.\n\
        Publishes release assets for Octez L1 only." ^ release_description)

let octez_beta_release_tag =
  Cacio.new_global_pipeline
    "octez_beta_release_tag"
    (lazy
      Rules.(
        If.(
          on_tezos_namespace && push && has_tag_match octez_beta_release_tag_re)))
    ~description:
      ("Beta release tag pipelines for Octez.\n\n\
        This pipeline is created when the release manager pushes a tag in the \
        format octez-vX.Y(-betaN). It is as Octez release tag pipelines, but \
        does not publish to opam." ^ release_description)

let octez_major_release_tag_test =
  Cacio.new_global_pipeline
    "octez_major_release_tag_test"
    (lazy
      Rules.(
        If.(
          not_on_tezos_namespace && push
          && has_tag_match octez_major_release_tag_re)))
    ~description:
      "Dry-run pipeline for 'octez_major_release_tag'.\n\n\
       This pipeline checks that 'octez_major_release_tag' pipelines work as \
       intended, without publishing any release. Developers or release \
       managers can create this pipeline by pushing a tag to a fork of \
       'tezos/tezos', e.g. to the 'nomadic-labs/tezos' project."

let octez_minor_release_tag_test =
  Cacio.new_global_pipeline
    "octez_minor_release_tag_test"
    (lazy
      Rules.(
        If.(
          not_on_tezos_namespace && push
          && has_tag_match octez_minor_release_tag_re)))
    ~description:
      "Dry-run pipeline for 'octez_minor_release_tag'.\n\n\
       This pipeline checks that 'octez_minor_release_tag' pipelines work as \
       intended, without publishing any release. Developers or release \
       managers can create this pipeline by pushing a tag to a fork of \
       'tezos/tezos', e.g. to the 'nomadic-labs/tezos' project."

let octez_beta_release_tag_test =
  Cacio.new_global_pipeline
    "octez_beta_release_tag_test"
    (lazy
      Rules.(
        If.(
          not_on_tezos_namespace && push
          && has_tag_match octez_beta_release_tag_re)))
    ~description:
      "Dry run pipeline for 'octez_beta_release_tag'.\n\n\
       This pipeline checks that 'octez_beta_release_tag' pipelines work as \
       intended, without publishing any release. Developers or release \
       managers can create this pipeline by pushing a tag to a fork of \
       'tezos/tezos', e.g. to the 'nomadic-labs/tezos' project."

let octez_packaging_revision =
  Cacio.new_global_pipeline
    "octez_packaging_revision"
    (lazy
      Rules.(
        If.(
          on_tezos_namespace && push
          && Rules.has_tag_match octez_packaging_revision_tag_re)))
    ~variables:[("DOCKER_FORCE_BUILD", "true")]
    ~description:
      "Packaging revision pipeline for Octez.\n\n\
       This pipeline is created when a packaging revision tag in the format \
       octez-vX.Y-N is pushed to tezos/tezos."

let octez_packaging_revision_test =
  Cacio.new_global_pipeline
    "octez_packaging_revision_test"
    (lazy
      Rules.(
        If.(
          not_on_tezos_namespace && push
          && Rules.has_tag_match octez_packaging_revision_tag_re)))
    ~interruptible_publish:true
    ~variables:[("DOCKER_FORCE_BUILD", "true")]
    ~description:
      "Dry run pipeline for 'octez_packaging_revision_tag'.\n\n\
       This pipeline checks that 'octez_packaging_revision_tag' pipelines work \
       as intended, without publishing any assets. Developers or release \
       managers can create this pipeline by pushing a tag to a fork of \
       'tezos/tezos', e.g. to the 'nomadic-labs/tezos' project."

let non_release_tag =
  Cacio.new_global_pipeline
    "non_release_tag"
    (lazy
      Rules.(If.(on_tezos_namespace && push && Lazy.force has_non_release_tag)))
    ~description:
      ("Tag pipeline for non-release tags.\n\n\
        Created on each push of a tag that does not match e.g. \
        octez(-evm-node)-vX.Y(-rcN). This pipeline creates a release on GitLab \
        and associated artifacts, like 'octez_release_tag' pipelines, but does \
        not publish it." ^ release_description)

let non_release_tag_test =
  Cacio.new_global_pipeline
    "non_release_tag_test"
    (lazy
      Rules.(
        If.(not_on_tezos_namespace && push && Lazy.force has_non_release_tag)))
    ~description:
      "Dry-run pipeline for 'non_release_tag'.\n\n\
       This pipeline checks that 'non_release_tag' pipelines work as intended, \
       without publishing any release. Developers, or release managers, can \
       create this pipeline by pushing a tag to a fork of 'tezos/tezos', e.g. \
       to the 'nomadic-labs/tezos' project."

(* Add jobs to the release pipelines of all components. *)
let register_release_jobs jobs =
  Cacio.register_jobs octez_major_release_tag jobs ;
  Cacio.register_jobs octez_beta_release_tag jobs

(* Add jobs to the test release pipelines of all components. *)
let register_test_release_jobs jobs =
  Cacio.register_jobs octez_major_release_tag_test jobs ;
  Cacio.register_jobs octez_beta_release_tag_test jobs

let schedule_extended_test =
  Cacio.new_global_pipeline
    "schedule_extended_test"
    (lazy Rules.schedule_extended_tests)
    ~interruptible_pipeline:false
    ~description:
      "Scheduled, full version of 'before_merging', daily on 'master'.\n\n\
       This pipeline unconditionally executes all jobs in 'before_merging' \
       pipelines, daily on the 'master_branch'. Regular 'before_merging' \
       pipelines run only subset of all jobs depending on files modified by \
       the MR. This \"safety net\"-pipeline ensures that all jobs run at least \
       daily."

let debian_daily =
  Cacio.new_global_pipeline
    "debian.daily"
    (lazy Rules.debian_daily)
    ~description:
      "Daily pipeline containing all Debian jobs (build and extended tests)."

let homebrew_daily =
  Cacio.new_global_pipeline
    "homebrew.daily"
    (lazy Rules.homebrew_daily)
    ~interruptible_pipeline:false
    ~description:
      "Daily pipeline containing all Homebrew jobs (build and extended tests)."

(* Rebuilds the base images on the [master-ci-images] branch.
   [DOCKER_FORCE_BUILD] disables the Docker layer cache, so the images are
   rebuilt fresh. This periodic refresh is necessary to avoid image deletion
   due to the registry retention policy.
   [CI_COMMIT_REF_SLUG] is overridden to [master] so the rebuilt images are
   tagged [master-<sha>] rather than [master-ci-images-<sha>].
   TODO (#8374): drop the [CI_COMMIT_REF_SLUG] override once base-image tags
   no longer embed the ref slug. *)
let base_images_refresh =
  Cacio.new_global_pipeline
    "base_images.refresh"
    (lazy Rules.base_images_refresh)
    ~interruptible_pipeline:false
    ~variables:
      [("CI_COMMIT_REF_SLUG", "master"); ("DOCKER_FORCE_BUILD", "true")]
    ~description:
      "Refresh pipeline: rebuild the base images from scratch on the \
       [master-ci-images] branch (same jobs as [base_images.daily])."

let base_images_daily =
  Cacio.new_global_pipeline
    "base_images.daily"
    (lazy Rules.base_images_daily)
    ~interruptible_pipeline:false
    ~description:
      "Daily pipeline containing all Base Images jobs (build and merge)."

let schedule_extended_rpc_test =
  Cacio.new_global_pipeline
    "schedule_extended_rpc_test"
    (lazy Rules.schedule_extended_rpc_tests)
    ~interruptible_pipeline:false
    ~description:
      "Scheduled run of all tezt tests with external RPC servers, weekly on \
       'master'.\n\n\
       This scheduled pipeline exercices the full tezt tests suites, but with \
       Octez nodes configured to use external RPC servers."

let schedule_extended_validation_test =
  Cacio.new_global_pipeline
    "schedule_extended_validation_test"
    (lazy Rules.schedule_extended_validation_tests)
    ~interruptible_pipeline:false
    ~description:
      "Scheduled run of all tezt tests with single-process validation, weekly \
       on 'master'.\n\n\
       This scheduled pipeline exercices the full tezt tests suites, but with \
       Octez nodes configured to use single-process validation."

let schedule_extended_baker_remote_mode_test =
  Cacio.new_global_pipeline
    "schedule_extended_baker_remote_mode_test"
    (lazy Rules.schedule_extended_baker_remote_mode_tests)
    ~interruptible_pipeline:false
    ~description:
      "Scheduled run of all tezt tests with baker using remote node, weekly on \
       'master'.\n\n\
       This scheduled pipeline exercices the full tezt tests suites."

let schedule_extended_dal_use_baker =
  Cacio.new_global_pipeline
    "schedule_extended_dal_use_baker"
    (lazy Rules.schedule_extended_dal_use_baker)
    ~interruptible_pipeline:false
    ~description:
      "Scheduled run of all tezt tests with dal using baker commands weekly on \
       'master'.\n\n\
       This scheduled pipeline exercices the full tezt tests suites."

let custom_extended_test_pipelines =
  [
    schedule_extended_rpc_test;
    schedule_extended_validation_test;
    schedule_extended_baker_remote_mode_test;
    schedule_extended_dal_use_baker;
  ]

(* Add jobs to all the "custom extended test" pipelines.
   They all contain exactly the same jobs. *)
let register_custom_extended_test_jobs jobs =
  List.iter
    (fun pipeline -> Cacio.register_jobs pipeline jobs)
    custom_extended_test_pipelines

let schedule_test_release =
  Cacio.new_global_pipeline
    "schedule_test_release"
    (lazy Rules.schedule_test_release)
    ~description:
      "Scheduled pipeline that runs a test release pipeline. The jobs are the \
       same as a release pipeline but run in dry-mode."

let schedule_security_scans =
  Cacio.new_global_pipeline
    "schedule_security_scans"
    (lazy Rules.schedule_security_scans)
    ~description:
      "Scheduled pipeline for various security scans. Currently scanning for \
       vulnerabilities in Docker images"

let schedule_docker_master_snapshot =
  Cacio.new_global_pipeline
    "schedule_docker_master_snapshot"
    (lazy Rules.schedule_docker_master_snapshot)
    ~interruptible_pipeline:false
    ~description:
      "Scheduled pipeline publishing a dated master Docker image to Docker \
       Hub.\n\n\
       This pipeline publishes the Octez Docker image tagged as \
       [master-YYYYMMDD] (where the date is computed at build time) to \
       DockerHub (https://hub.docker.com/r/tezos/tezos), then promotes it to \
       the rolling [weekly] tag."

let schedule_docker_build_pipeline =
  Cacio.new_global_pipeline
    "schedule_docker_build_pipeline"
    (lazy Rules.schedule_docker_build)
    ~variables:[("DOCKER_FORCE_BUILD", "true")]
    ~description:
      "Scheduled pipeline for forcing building fresh Docker image (skipping \
       any cache mechanism) for the current master branch of Octez. The newly \
       built images should contains the latest available Alpine packages"

let publish_test_release_page =
  Cacio.new_global_pipeline
    "publish_test_release_page"
    (lazy Rules.(If.(api_release_page && not_on_tezos_namespace)))
    ~description:"Pipeline that updates and publishes the test release page."

let publish_release_page =
  Cacio.new_global_pipeline
    "publish_release_page"
    (lazy Rules.(If.(api_release_page && on_tezos_namespace)))
    ~description:"Pipeline that updates and publishes the release page."

(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs of the [release_tag] family of pipelines.

   These pipeline runs on each pushes to the various release tags (see
   [main.ml] for the set of regular expressions that define the
   language of release tags).

   The goal of these pipelines is to create
   {{:https://gitlab.com/tezos/tezos/-/releases}Octez releases on
   GitLab}, the associated artifacts, and to push releases to opam. *)

open Tezos_ci
open Common.Build
open Common.Docker

(** Type of release tag pipelines.

    The semantics of the type is summed up in this table:

   |                       | Release_tag | Beta_release_tag | Non_release_tag |
   |-----------------------+-------------+------------------+-----------------|
   | GitLab release type   | Release     | Release          | Create          |
   | Experimental binaries | No          | No               | No              |
   | Docker build type     | Release     | Release          | Release         |
   | Publishes to opam     | Yes         | No               | No              |

    - All release tag pipelines types publish [Release] type Docker builds.
    - No release tag pipelines include experimental binaries.
    - [Release_tag] and [Beta_release_tag] pipelines creates GitLab
    and publishes releases. [Non_release_tag] pipelines create the
    GitLab release but do not publish them.
    - Only [Release_tag] pipelines publish to opam. *)
type release_tag_pipeline_type =
  | Release_tag
  | Beta_release_tag
  | Non_release_tag
  | Schedule_test

(* Release jobs must not be retried: they publish artifacts and retrying could
   result in publishing the same artifact twice. *)
let no_retry = Gitlab_ci.Types.{max = 0; when_ = []}

let monitoring_child_pipeline =
  (* Using lazy lets us define the pipeline at toplevel,
     but actually execute the code after the jobs have been defined with Cacio
     (they have not yet been defined when this toplevel definition is executed),
     and only once (there are multiple pipelines with a trigger job for octez_monitoring,
     but a pipeline cannot be registered more than once). *)
  lazy
    (Pipeline.register_child
       "octez_monitoring"
       ~description:"Octez monitoring jobs"
       ~inherit_:
         (Gitlab_ci.Types.Variable_list
            ["ci_image_name"; "ci_image_name_protected"; "jsonnet_image_name"])
       ~jobs:([job_datadog_pipeline_trace] @ Cacio.get_octez_monitoring_jobs ()))

let job_docker =
  Cacio.parameterize @@ fun mode ->
  Cacio.parameterize @@ fun trigger ->
  Cacio.parameterize @@ fun arch ->
  job_docker_build
    ~__POS__
    ~dependencies:(Dependent [])
    ?rules:
      (match trigger with
      | `manual ->
          Some [Gitlab_ci.Util.job_rule ~when_:Manual ~allow_failure:No ()]
      | `auto -> None)
    ~arch
    ?storage:(match arch with Arm64 -> Some Ramfs | _ -> None)
    (match mode with `test -> Test | `real -> Release)

let job_docker_merge =
  Cacio.parameterize @@ fun mode ->
  Cacio.parameterize @@ fun trigger ->
  job_docker_merge_manifests
    ~__POS__
    ~ci_docker_hub:(match mode with `test -> false | `real -> true)
    ~job_docker_amd64:(job_docker mode trigger Amd64)
    ~job_docker_arm64:(job_docker mode trigger Arm64)

(* On release pipelines the static binaries do not have any dependencies
   on previous stages and can start immediately. *)
let job_build_static =
  Cacio.parameterize @@ fun trigger ->
  Cacio.parameterize @@ fun arch ->
  job_build_static_binaries
    ~__POS__
    ~dependencies:(Dependent [])
    ?rules:
      (match trigger with
      | `manual ->
          Some [Gitlab_ci.Util.job_rule ~when_:Manual ~allow_failure:No ()]
      | `auto -> None)
    ~arch
    ?cpu:(match arch with Amd64 -> Some Very_high | _ -> None)
    ~storage:Ramfs
    ~release:true
    ()

let job_build_homebrew_release =
  add_artifacts
    ~name:"build-$CI_COMMIT_REF_SLUG"
    ~expire_in:(Duration (Days 1))
    ~when_:On_success
    ["public/homebrew/*"]
    Homebrew.job_create_homebrew_formula

let job_gitlab_release =
  Cacio.parameterize @@ fun mode ->
  job
    ~__POS__
    ~image:Images.Base_images.ci_release
    ~stage:Stages.publish
    ~interruptible:false
    ~dependencies:
      (Dependent
         [
           Job (job_docker_merge mode `auto);
           Artifacts (job_build_static `auto Amd64);
           Artifacts (job_build_static `auto Arm64);
           Artifacts job_build_homebrew_release;
         ])
    ~id_tokens:Tezos_ci.id_tokens
    ~name:"gitlab:release"
    [
      "./scripts/ci/restrict_export_to_octez_source.sh";
      "./scripts/releases/gitlab-release.sh";
    ]
    ~retry:no_retry
    ~tag:Gcp_not_interruptible

let job_gitlab_publish =
  Cacio.parameterize @@ fun mode ->
  job
    ~__POS__
    ~image:Images.Base_images.ci_release
    ~stage:Stages.publish
    ~interruptible:false
    ~dependencies:
      (Dependent
         [
           Artifacts (job_build_static `auto Amd64);
           Artifacts (job_build_static `auto Arm64);
           Artifacts job_build_homebrew_release;
         ])
    ?before_script:
      (match mode with
      | `scheduled_test -> Some ["git tag octez-v0.0"]
      | `non_release_tag -> None)
    ?variables:
      (match mode with
      | `scheduled_test -> Some [("CI_COMMIT_TAG", "octez-v0.0")]
      | `non_release_tag -> None)
    ~id_tokens:Tezos_ci.id_tokens
    ~name:"gitlab:publish"
    [
      ("${CI_PROJECT_DIR}/scripts/ci/create_gitlab_package.sh"
      ^
      match mode with `scheduled_test -> " --dry-run" | `non_release_tag -> "");
    ]
    ~retry:no_retry
    ~tag:Gcp_not_interruptible

let job_release_page =
  Cacio.parameterize @@ fun mode ->
  Cacio.parameterize
  @@ fun (wait_for : [`wait_for_nothing | `wait_for_build]) ->
  job
    ~__POS__
    ~image:Images.CI.release_page
    ~stage:Stages.publish
    ~description:
      "A job to update the Octez release page. If running in a test pipleine, \
       the assets are pushed in the [release-page-test.nomadic-labs.com] \
       bucket. Otherwise they are pushed in [site.prod.octez.tezos.com]. Then \
       its [index.html] is updated accordingly."
    ~name:"publish:release-page"
    ~rules:[Gitlab_ci.Util.job_rule ~when_:Manual ()]
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~expire_in:(Duration (Days 1))
         ["./index.md"; "index.html"])
    ~before_script:["eval $(opam env)"]
    ~after_script:["cp /tmp/release_page*/index.md ./index.md"]
    ?dependencies:
      (match wait_for with
      | `wait_for_nothing -> None
      | `wait_for_build ->
          Some
            (Dependent
               [
                 Artifacts (job_build_static `auto Amd64);
                 Artifacts (job_build_static `auto Arm64);
               ]))
    ~variables:
      (match mode with
      | `test ->
          (* The S3_BUCKET, AWS keys and DISTRIBUTION_ID
            depends on the release type (tests or not). *)
          [
            ("S3_BUCKET", "release-page-test.nomadic-labs.com");
            ("DISTRIBUTION_ID", "E19JF46UG3Z747");
            ("AWS_ACCESS_KEY_ID", "${AWS_KEY_RELEASE_PUBLISH}");
            ("AWS_SECRET_ACCESS_KEY", "${AWS_SECRET_RELEASE_PUBLISH}");
          ]
      | `real ->
          [
            ("S3_BUCKET", "site-prod.octez.tezos.com");
            ("BUCKET_PATH", "/releases");
            ("URL", "octez.tezos.com");
            ("DISTRIBUTION_ID", "${CLOUDFRONT_DISTRIBUTION_ID}");
          ])
    ["./scripts/releases/publish_release_page.sh"]
    ~retry:no_retry
    ~tag:Gcp_not_interruptible

let job_opam_release =
  Cacio.parameterize @@ fun mode ->
  job
    ~__POS__
    ~image:Images.CI.prebuild
    ~stage:Stages.publish
    ~description:
      "Update opam package descriptions on tezos/tezos opam-repository fork.\n\n\
       This job does preliminary work for releasing Octez opam packages on \
       opam repository, by pushing a branch with updated package descriptions \
       (.opam files) to https://github.com/tezos/opam-repository. It _does \
       not_ automatically create a corresponding pull request on the official \
       opam repository."
    ~interruptible:false
    ~name:"opam:release"
    [
      ("./scripts/ci/opam-release.sh"
      ^ match mode with `test -> " --dry-run" | `real -> "");
    ]
    ~retry:no_retry
    ~tag:Gcp_not_interruptible

let job_dispatch_call =
  job
    ~__POS__
    ~image:Images.CI.prebuild
    ~stage:Stages.publish
    ~description:
      "A job release that triggers pipelines from other repositories after a \
       release.\n\
       For now, it triggers the release pipeline from \
       tez-capital/tezos-macos-pipeline"
    ~interruptible:false
    ~name:"dispatch-call"
    ~dependencies:
      (Dependent
         [
           Job (job_release_page `real `wait_for_build);
           Job (job_gitlab_release `real);
         ])
    ["./scripts/releases/dispatch-call.sh"]

(** Create an Octez release tag pipeline of type {!release_tag_pipeline_type}.

    If [test] is true (default is [false]), then the Docker images are
    built of the [Test] type and are published to the GitLab registry
    instead of Docker hub.

    If [major] is false (default is [true]), then components jobs are
    excluded from the Octez jobs.

    On release pipelines these jobs can start immediately *)
let octez_jobs ?(test = false) ?(major = true) release_tag_pipeline_type =
  let mode = if test then `test else `real in
  let jobs_debian_repository =
    Debian_repository.jobs ~limit_dune_build_jobs:true Release
  in
  let job_gitlab_release_or_publish =
    match release_tag_pipeline_type with
    | Non_release_tag -> job_gitlab_publish `non_release_tag
    | Schedule_test -> job_gitlab_publish `scheduled_test
    | _ -> job_gitlab_release mode
  in
  let job_release_page = job_release_page mode `wait_for_build in
  let job_promote_to_latest_test =
    job_docker_promote_to_latest
      ~ci_docker_hub:false
      ~dependencies:(Dependent [Job (job_docker_merge mode `auto)])
      ()
  in
  let job_trigger_monitoring =
    trigger_job
      ~__POS__
      ~dependencies:(Dependent [])
      ~stage:Stages.build
      (Lazy.force monitoring_child_pipeline)
  in
  [
    (* Stage: start *)
    job_datadog_pipeline_trace;
    (* Stage: build *)
    job_build_static `auto Amd64;
    job_build_static `auto Arm64;
    job_docker mode `auto Amd64;
    job_docker mode `auto Arm64;
    job_build_homebrew_release;
    job_docker_merge mode `auto;
    job_gitlab_release_or_publish;
    job_trigger_monitoring;
  ]
  @ jobs_debian_repository
  (* Include components release jobs only if this is a major release. *)
  @ (if not major then []
     else
       match (test, release_tag_pipeline_type) with
       | false, (Release_tag | Beta_release_tag | Non_release_tag) ->
           Cacio.get_global_release_jobs ()
       | true, (Release_tag | Beta_release_tag | Non_release_tag) ->
           Cacio.get_global_test_release_jobs ()
       | true, Schedule_test -> Cacio.get_global_scheduled_test_release_jobs ()
       | false, Schedule_test ->
           failwith
             "test = false is inconsistent with release_tag_pipeline_type = \
              Schedule_test")
  @
  match (test, release_tag_pipeline_type) with
  | false, Release_tag ->
      [job_opam_release `real; job_release_page; job_dispatch_call]
  | true, Release_tag ->
      [
        (* This job normally runs in the {!Octez_latest_release} pipeline
           that is triggered manually after a release is made. However, to
           make release testing easier, we include it here directly. Thus,
           release testers are not required to trigger two separate pipelines
           (indeed, the second `latest_release_test` pipeline is rarely tested). *)
        job_promote_to_latest_test;
        job_opam_release `test;
        job_release_page;
      ]
  | false, Beta_release_tag -> [job_release_page; job_dispatch_call]
  | true, Beta_release_tag -> [job_release_page]
  | _ -> []

let job_docker_promote_to_version =
  Cacio.parameterize @@ fun mode ->
  job_docker_authenticated
    ~__POS__
    ~dependencies:(Dependent [Job (job_docker_merge mode `manual)])
    ~stage:Stages.publish
    ~name:"oc.docker:promote_revision_to_version"
    ~description:
      "Promote the Docker image from the packaging revision tag to the \
       canonical version tag (e.g., octez-v1.2 from octez-v1.2-1)"
    ~rules:[Gitlab_ci.Util.job_rule ~when_:Manual ~allow_failure:No ()]
    ~ci_docker_hub:(match mode with `test -> false | `real -> true)
    ["./scripts/ci/docker_promote_to_version.sh"]
    ~retry:no_retry
    ~tag:Gcp_not_interruptible

let job_create_gitlab_package =
  job
    ~__POS__
    ~image:Images.Base_images.ci_release
    ~stage:Stages.publish
    ~name:"gitlab:create_package"
    ~description:
      "Create GitLab packages with static binaries from this packaging revision"
    ~dependencies:
      (Dependent
         [
           Artifacts (job_build_static `manual Amd64);
           Artifacts (job_build_static `manual Arm64);
         ])
    ~rules:[Gitlab_ci.Util.job_rule ~when_:Manual ~allow_failure:No ()]
    ~id_tokens:Tezos_ci.id_tokens
    ["./scripts/ci/create_gitlab_package.sh"]
    ~retry:no_retry
    ~tag:Gcp_not_interruptible

let job_update_gitlab_release =
  job
    ~__POS__
    ~image:Images.Base_images.ci_release
    ~stage:Stages.publish
    ~name:"gitlab:update_release"
    ~description:
      "Update existing GitLab release with new static binaries from this \
       packaging revision"
    ~dependencies:(Dependent [Job job_create_gitlab_package])
    ~id_tokens:Tezos_ci.id_tokens
    ["./scripts/releases/update_gitlab_release.sh"]
    ~retry:no_retry
    ~tag:Gcp_not_interruptible

let octez_packaging_revision_jobs ?(test = false) () =
  let mode = if test then `test else `real in
  let jobs_debian_repository =
    Debian_repository.jobs ~limit_dune_build_jobs:true ~manual:true Release
  in
  (* We want to be able to trigger each "batch" of jobs manually.
     There are two batches: one with the static jobs, and one that publishes.
     The static jobs are independent so they are both manual,
     but [job_update_gitlab_release] depends on [job_create_gitlab_package]
     so it does not have to be manual, only [job_create_gitlab_package] does. *)
  [
    (* Stage: start *)
    job_datadog_pipeline_trace;
    (* Docker images *)
    job_docker mode `manual Amd64;
    job_docker mode `manual Arm64;
    job_docker_merge mode `manual;
    job_docker_promote_to_version mode;
    (* Static binaries *)
    job_build_static `manual Amd64;
    job_build_static `manual Arm64;
    (* Release update *)
    job_create_gitlab_package;
    job_update_gitlab_release;
  ]
  @ jobs_debian_repository
  @
  if test then Cacio.get_global_packaging_revision_test_jobs ()
  else Cacio.get_global_packaging_revision_jobs ()

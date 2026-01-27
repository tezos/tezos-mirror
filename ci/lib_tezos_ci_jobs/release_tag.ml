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
open Tezos_ci.Cache
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

let monitoring_child_pipeline =
  (* The Teztale build job is defined using Cacio, which does not provide a way
     to include it in the monitoring child pipeline, because we plan to remove
     this child pipeline. In the meantime, we redefine the job here. *)
  let job_teztale_build ?cpu ~arch ?storage () =
    let arch_string = Runner.Arch.show_easy_to_distinguish arch in
    job
      ~__POS__
      ~arch
      ?cpu
      ?storage
      ~name:("teztale.build:static-" ^ arch_string)
      ~image:Images.CI.build
      ~stage:Stages.build
      ~variables:[("PROFILE", "static")]
      ~artifacts:
        (Gitlab_ci.Util.artifacts
           ~name:"teztale-binaries"
           ~expire_in:Never
           ~when_:On_success
           ["teztale-binaries/" ^ arch_string ^ "/octez-teztale-*"])
      ~before_script:
        [
          "./scripts/ci/take_ownership.sh";
          ". ./scripts/version.sh";
          "eval $(opam env)";
        ]
      ~after_script:
        [
          "mkdir -p ./teztale-binaries/" ^ arch_string;
          "mv octez-teztale-* ./teztale-binaries/" ^ arch_string ^ "/";
        ]
      ["make teztale"]
    |> enable_cargo_cache |> enable_sccache
  in
  Pipeline.register_child
    "octez_monitoring"
    ~description:"Octez monitoring jobs"
    ~inherit_:
      (Gitlab_ci.Types.Variable_list
         ["ci_image_name"; "ci_image_name_protected"; "jsonnet_image_name"])
    ~jobs:
      [
        job_datadog_pipeline_trace;
        job_build_layer1_profiling ~expire_in:Never ();
        job_teztale_build ~arch:Arm64 ~storage:Ramfs ();
        job_teztale_build ~arch:Amd64 ~cpu:Very_high ();
      ]

let job_release_page ~test ?dependencies () =
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
    ?dependencies
    ~variables:
      (if test then
         (* The S3_BUCKET, AWS keys and DISTRIBUTION_ID
            depends on the release type (tests or not). *)
         [
           ("S3_BUCKET", "release-page-test.nomadic-labs.com");
           ("DISTRIBUTION_ID", "E19JF46UG3Z747");
           ("AWS_ACCESS_KEY_ID", "${AWS_KEY_RELEASE_PUBLISH}");
           ("AWS_SECRET_ACCESS_KEY", "${AWS_SECRET_RELEASE_PUBLISH}");
         ]
       else
         [
           ("S3_BUCKET", "site-prod.octez.tezos.com");
           ("BUCKET_PATH", "/releases");
           ("URL", "octez.tezos.com");
           ("DISTRIBUTION_ID", "${CLOUDFRONT_DISTRIBUTION_ID}");
         ])
    ["./scripts/releases/publish_release_page.sh"]
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
    ~tag:Gcp_not_interruptible

(** Create an Octez release tag pipeline of type {!release_tag_pipeline_type}.

    If [test] is true (default is [false]), then the Docker images are
    built of the [Test] type and are published to the GitLab registry
    instead of Docker hub.

    If [major] is false (default is [true]), then components jobs are
    excluded from the Octez jobs.

    On release pipelines these jobs can start immediately *)
let octez_jobs ?(test = false) ?(major = true) release_tag_pipeline_type =
  let variables =
    match release_tag_pipeline_type with
    | Schedule_test -> Some [("CI_COMMIT_TAG", "octez-v0.0")]
    | _ -> None
  in
  let job_docker_amd64 =
    job_docker_build
      ~dependencies:(Dependent [])
      ~__POS__
      ~arch:Amd64
      (if test then Test else Release)
  in
  let job_docker_arm64 =
    job_docker_build
      ~dependencies:(Dependent [])
      ~__POS__
      ~arch:Arm64
      ~storage:Ramfs
      (if test then Test else Release)
  in
  let job_docker_merge =
    job_docker_merge_manifests
      ~__POS__
      ~ci_docker_hub:(not test)
      ~job_docker_amd64
      ~job_docker_arm64
  in
  (* on release pipelines the static binaries do not have any dependencies
     on previous stages and can start immediately *)
  let job_static_arm64_release =
    job_build_static_binaries
      ~dependencies:(Dependent [])
      ~__POS__
      ~arch:Arm64
      ~storage:Ramfs
      ~release:true
      ()
  in
  let job_static_x86_64_release =
    job_build_static_binaries
      ~dependencies:(Dependent [])
      ~__POS__
      ~arch:Amd64
      ~cpu:Very_high
      ~storage:Ramfs
      ~release:true
      ()
  in
  let job_build_homebrew_release =
    add_artifacts
      ~name:"build-$CI_COMMIT_REF_SLUG"
      ~expire_in:(Duration (Days 1))
      ~when_:On_success
      ["public/homebrew/*"]
      Homebrew.job_create_homebrew_formula
  in
  let job_gitlab_release ~dependencies : Tezos_ci.tezos_job =
    job
      ~__POS__
      ~image:Images.ci_release
      ~stage:Stages.publish
      ~interruptible:false
      ~dependencies:(Dependent (Job job_docker_merge :: dependencies))
      ~id_tokens:Tezos_ci.id_tokens
      ~name:"gitlab:release"
      ?variables
      [
        "./scripts/ci/restrict_export_to_octez_source.sh";
        "./scripts/releases/gitlab-release.sh";
      ]
      ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
      ~tag:Gcp_not_interruptible
  in
  let job_gitlab_publish ~dependencies () : Tezos_ci.tezos_job =
    let before_script =
      match release_tag_pipeline_type with
      | Schedule_test -> Some ["git tag octez-v0.0"]
      | _ -> None
    in
    job
      ~__POS__
      ~image:Images.ci_release
      ~stage:Stages.publish
      ~interruptible:false
      ~dependencies:(Dependent dependencies)
      ?before_script
      ?variables
      ~id_tokens:Tezos_ci.id_tokens
      ~name:"gitlab:publish"
      [
        ("${CI_PROJECT_DIR}/scripts/ci/create_gitlab_package.sh"
        ^
        match release_tag_pipeline_type with
        | Schedule_test -> " --dry-run"
        | _ -> "");
      ]
      ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
      ~tag:Gcp_not_interruptible
  in
  let jobs_debian_repository =
    Debian_repository.jobs ~limit_dune_build_jobs:true Release
  in
  let job_gitlab_release_or_publish =
    let dependencies =
      [
        Artifacts job_static_x86_64_release;
        Artifacts job_static_arm64_release;
        Artifacts job_build_homebrew_release;
      ]
    in
    match release_tag_pipeline_type with
    | Non_release_tag | Schedule_test -> job_gitlab_publish ~dependencies ()
    | _ -> job_gitlab_release ~dependencies
  in
  let job_release_page =
    job_release_page
      ~test
      ~dependencies:
        (Dependent
           [
             Artifacts job_static_x86_64_release;
             Artifacts job_static_arm64_release;
           ])
      ()
  in
  let job_opam_release ?(dry_run = false) () : Tezos_ci.tezos_job =
    job
      ~__POS__
      ~image:Images.CI.prebuild
      ~stage:Stages.publish
      ~description:
        "Update opam package descriptions on tezos/tezos opam-repository fork.\n\n\
         This job does preliminary work for releasing Octez opam packages on \
         opam repository, by pushing a branch with updated package \
         descriptions (.opam files) to \
         https://github.com/tezos/opam-repository. It _does not_ automatically \
         create a corresponding pull request on the official opam repository."
      ~interruptible:false
      ?variables
      ~name:"opam:release"
      [("./scripts/ci/opam-release.sh" ^ if dry_run then " --dry-run" else "")]
      ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
      ~tag:Gcp_not_interruptible
  in
  let job_promote_to_latest_test =
    job_docker_promote_to_latest
      ~ci_docker_hub:false
      ~dependencies:(Dependent [Job job_docker_merge])
      ()
  in
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
        (Dependent [Job job_release_page; Job job_gitlab_release_or_publish])
      ["./scripts/releases/dispatch-call.sh"]
  in
  let job_trigger_monitoring =
    trigger_job
      ~__POS__
      ~dependencies:(Dependent [])
      ~stage:Stages.build
      monitoring_child_pipeline
  in
  [
    (* Stage: start *)
    job_datadog_pipeline_trace;
    (* Stage: build *)
    job_static_x86_64_release;
    job_static_arm64_release;
    job_docker_amd64;
    job_docker_arm64;
    job_build_homebrew_release;
    job_docker_merge;
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
      [job_opam_release (); job_release_page; job_dispatch_call]
  | true, Release_tag ->
      [
        (* This job normally runs in the {!Octez_latest_release} pipeline
           that is triggered manually after a release is made. However, to
           make release testing easier, we include it here directly. Thus,
           release testers are not required to trigger two separate pipelines
           (indeed, the second `latest_release_test` pipeline is rarely tested). *)
        job_promote_to_latest_test;
        job_opam_release ~dry_run:true ();
        job_release_page;
      ]
  | false, Beta_release_tag -> [job_release_page; job_dispatch_call]
  | true, Beta_release_tag -> [job_release_page]
  | _ -> []

let octez_packaging_revision_jobs ?(test = false) () =
  let jobs_debian_repository =
    Debian_repository.jobs ~limit_dune_build_jobs:true ~manual:true Release
  in
  let job_docker_amd64 =
    job_docker_build
      ~dependencies:(Dependent [])
      ~rules:[Gitlab_ci.Util.job_rule ~when_:Manual ()]
      ~__POS__
      ~arch:Amd64
      (if test then Test else Release)
  in
  let job_docker_arm64 =
    job_docker_build
      ~dependencies:(Dependent [])
      ~rules:[Gitlab_ci.Util.job_rule ~when_:Manual ()]
      ~__POS__
      ~arch:Arm64
      ~storage:Ramfs
      (if test then Test else Release)
  in
  let job_docker_merge =
    job_docker_merge_manifests
      ~__POS__
      ~ci_docker_hub:(not test)
      ~job_docker_amd64
      ~job_docker_arm64
  in
  let job_docker_promote_to_version =
    job_docker_authenticated
      ~__POS__
      ~dependencies:(Dependent [Job job_docker_merge])
      ~stage:Stages.publish
      ~name:"docker:promote_revision_to_version"
      ~rules:[Gitlab_ci.Util.job_rule ~when_:Manual ()]
      ~ci_docker_hub:(not test)
      ["./scripts/ci/docker_promote_to_version.sh"]
      ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
      ~tag:Gcp_not_interruptible
  in
  [
    (* Stage: start *)
    job_datadog_pipeline_trace;
    job_docker_amd64;
    job_docker_arm64;
    job_docker_merge;
    job_docker_promote_to_version;
  ]
  @ jobs_debian_repository

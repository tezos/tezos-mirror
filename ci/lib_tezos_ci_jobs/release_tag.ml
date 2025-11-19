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

(* Some jobs have been migrated to Cacio, in the shared component. *)
module CI = Cacio.Shared

(* Release jobs must not be retried: they publish artifacts and retrying could
   result in publishing the same artifact twice. *)
let no_retry = Gitlab_ci.Types.{max = 0; when_ = []}

let job_docker = Docker.job_docker `released

let job_docker_merge_manifests = Docker.job_docker_merge_manifests `released

let vuln_predicate = "vuln-predicate.json"

let scanning_report = "gl-container-scanning-report.json"

let job_docker_container_scanning =
  Cacio.parameterize @@ fun mode ->
  CI.job
    "docker:container_scanning"
    ~__POS__
    ~description:
      "Scan released Docker images for vulnerabilities using GCP Artifact \
       Registry scanning"
    ~stage:Publish
    ~image:Images_external.docker
    ~needs:[(Job, job_docker_merge_manifests mode)]
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~reports:
           (Gitlab_ci.Util.reports ~container_scanning:scanning_report ())
         [vuln_predicate])
    ~variables:
      [("CI_DOCKER_HUB", match mode with `test -> "false" | `real -> "true")]
    ~script:
      [
        "./scripts/ci/docker_image_names.sh";
        ". ./scripts/ci/docker.env";
        ". ./scripts/ci/docker.sh";
        sf
          "./scripts/ci/container_scanning_gcp.sh %s %s"
          scanning_report
          vuln_predicate;
      ]

(* This job normally runs in the {!Octez_latest_release} pipeline
   that is triggered manually after a release is made.
   However, to make release testing easier, we include it in other test release pipelines.
   Thus, release testers are not required to trigger two separate pipelines
   (indeed, the second [latest_release_test] pipeline is rarely tested).
   This version of the job is only for testing (mode [`test]);
   the [`real] version is in [octez_latest_release.ml]. *)
let job_docker_promote_to_latest =
  Cacio.parameterize @@ fun mode ->
  CI.job
    "docker:promote_to_latest"
    ~__POS__
    ~description:"Add the latest tag to the new Docker images."
    ~image:Images_external.docker
    ~stage:Publish
    ~needs:
      (match mode with
      | `test_wait ->
          (* [test_wait] is used in tag test pipelines where the Docker
             image is built in the same pipeline.  We wait for the image
             build and vulnerability scan to finish before promoting. *)
          [
            (Job, job_docker_merge_manifests `test);
            (Artifacts, job_docker_container_scanning `test);
          ]
      | `test | `real ->
          (* In latest release pipelines, the Docker image already exists,
             as it was created by another pipeline (the tag pipeline). *)
          [])
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
    ~services:[{name = "docker:${DOCKER_VERSION}-dind"}]
    ~variables:
      ([
         ("DOCKER_VERSION", Docker.version);
         ( "CI_DOCKER_HUB",
           match mode with `test | `test_wait -> "false" | `real -> "true" );
       ]
      @
      match mode with
      | `test_wait -> [("VULN_PREDICATE", vuln_predicate)]
      | `test | `real ->
          (* TODO: wire VULN_PREDICATE for real/test modes once the
             container_scanning job is added to production release
             pipelines (Major, Minor, Beta/RC; octez_latest_release.ml). *)
          [])
    ~script:
      [
        "./scripts/ci/docker_initialize.sh";
        "./scripts/ci/docker_promote_to_latest.sh";
      ]

let job_build_homebrew_release =
  add_artifacts
    ~name:"build-$CI_COMMIT_REF_SLUG"
    ~expire_in:(Duration (Days 1))
    ~when_:On_success
    ["public/homebrew/*"]
    Homebrew.job_create_homebrew_formula

let job_gitlab_release =
  Cacio.parameterize @@ fun mode ->
  CI.job
    "gitlab:release"
    ~__POS__
    ~description:"Create a GitLab release."
    ~image:Images.Base_images.ci_release
    ~stage:Publish
    ~needs:
      [
        ( Artifacts,
          Build.job_build_static_linux_released_binaries Amd64 `release );
        ( Artifacts,
          Build.job_build_static_linux_released_binaries Arm64 `release );
        (Job, job_docker_merge_manifests mode);
      ]
    ~needs_legacy:[(Artifacts, job_build_homebrew_release)]
    ~id_tokens:Tezos_ci.id_tokens
    ~script:
      [
        "./scripts/ci/restrict_export_to_octez_source.sh";
        "./scripts/releases/gitlab-release.sh";
      ]

let job_gitlab_publish =
  Cacio.parameterize @@ fun mode ->
  CI.job
    "gitlab:publish"
    ~__POS__
    ~description:"Create a GitLab package."
    ~image:Images.Base_images.ci_release
    ~stage:Publish
    ~needs:
      [
        ( Artifacts,
          Build.job_build_static_linux_released_binaries Amd64 `release );
        ( Artifacts,
          Build.job_build_static_linux_released_binaries Arm64 `release );
      ]
    ~needs_legacy:[(Artifacts, job_build_homebrew_release)]
    ?variables:
      (match mode with
      | `scheduled_test -> Some [("CI_COMMIT_TAG", "octez-v0.0")]
      | `non_release_tag -> None)
    ~id_tokens:Tezos_ci.id_tokens
    ~script:
      ((match mode with
       | `scheduled_test -> ["git tag octez-v0.0"]
       | `non_release_tag -> [])
      @ [
          ("${CI_PROJECT_DIR}/scripts/ci/create_gitlab_package.sh"
          ^
          match mode with
          | `scheduled_test -> " --dry-run"
          | `non_release_tag -> "");
        ])

let release_page_variables ~mode =
  match mode with
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
      ]

let job_release_page =
  Cacio.parameterize @@ fun mode ->
  Cacio.parameterize @@ fun wait_for ->
  CI.job
    "publish:release-page"
    ~__POS__
    ~image:Images.CI.release_page
    ~stage:Publish
    ~description:
      "A job to update the Octez release page. If running in a test pipleine, \
       the assets are pushed in the [release-page-test.nomadic-labs.com] \
       bucket. Otherwise they are pushed in [site.prod.octez.tezos.com]. Then \
       its [index.html] is updated accordingly."
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~expire_in:(Duration (Days 1))
         ["index.html"; "older_releases.html"; "index.md"; "older_releases.md"])
    ?needs:
      (match wait_for with
      | `wait_for_nothing -> None
      | `wait_for_build ->
          Some
            [
              ( Artifacts,
                Build.job_build_static_linux_released_binaries Amd64 `release );
              ( Artifacts,
                Build.job_build_static_linux_released_binaries Arm64 `release );
            ])
    ~variables:(release_page_variables ~mode)
    ~script:["eval $(opam env)"; "./scripts/releases/publish-release-page.sh"]

let job_opam_release =
  Cacio.parameterize @@ fun mode ->
  CI.job
    "opam:release"
    ~__POS__
    ~image:Images.CI.prebuild
    ~stage:Publish
    ~description:
      "Update opam package descriptions on tezos/tezos opam-repository fork.\n\n\
       This job does preliminary work for releasing Octez opam packages on \
       opam repository, by pushing a branch with updated package descriptions \
       (.opam files) to https://github.com/tezos/opam-repository. It _does \
       not_ automatically create a corresponding pull request on the official \
       opam repository."
    ~script:
      [
        ("./scripts/ci/opam-release.sh"
        ^ match mode with `test -> " --dry-run" | `real -> "");
      ]

let job_update_release_page =
  job
    ~__POS__
    ~image:Images.CI.release_page
    ~stage:Stages.publish
    ~description:
      "A job to update the Octez release page. If running in a test pipleine, \
       the assets are pushed in the [release-page-test.nomadic-labs.com] \
       bucket. Otherwise they are pushed in [site.prod.octez.tezos.com]. Then \
       its [index.html] is updated accordingly."
    ~name:"update_release-page"
    ~rules:[Gitlab_ci.Util.job_rule ~when_:Manual ()]
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~expire_in:(Duration (Days 1))
         ["./index.md"; "index.html"])
    ~before_script:["eval $(opam env)"]
    ~after_script:["cp /tmp/release_page*/index.md ./index.md"]
    ~variables:
      [
        ("S3_BUCKET", "site-prod.octez.tezos.com");
        ("BUCKET_PATH", "/releases");
        ("URL", "octez.tezos.com");
        ("DISTRIBUTION_ID", "${CLOUDFRONT_DISTRIBUTION_ID}");
      ]
    ["./scripts/releases/update_publish_release_page.sh"]
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
    ~tag:Gcp_not_interruptible

let job_dispatch_call =
  CI.job
    "dispatch-call"
    ~__POS__
    ~image:Images.CI.prebuild
    ~stage:Publish
    ~description:
      "A job release that triggers pipelines from other repositories after a \
       release.\n\
       For now, it triggers the release pipeline from \
       tez-capital/tezos-macos-pipeline"
    ~needs:
      [
        (Job, job_gitlab_release `real);
        (Job, job_release_page `real `wait_for_build);
      ]
    ~script:["./scripts/releases/dispatch-call.sh"]

let () =
  (* Major *)
  (* TODO: add (Auto, job_docker_container_scanning `real) and
     (Auto, job_docker_promote_to_latest `real) with VULN_PREDICATE
     to production release pipelines (Major, Minor, Beta/RC) once the
     scanning job is validated in test pipelines. Same for
     octez_latest_release.ml. *)
  Cacio.register_jobs
    Major_release_tag
    [
      (Auto, job_docker_merge_manifests `real);
      (Auto, job_gitlab_release `real);
      (Manual, job_release_page `real `wait_for_build);
      (Auto, job_opam_release `real);
      (Auto, job_dispatch_call);
      (Auto, Debian_repository.job_apt_repo_debian Release);
      (Auto, Debian_repository.job_apt_repo_ubuntu Release);
    ] ;
  Cacio.register_jobs
    Major_release_tag_test
    [
      (Auto, job_docker_merge_manifests `test);
      (Auto, job_docker_container_scanning `test);
      (Auto, job_gitlab_release `test);
      (Manual, job_release_page `test `wait_for_build);
      (Auto, job_opam_release `test);
      (Auto, job_docker_promote_to_latest `test_wait);
      (Auto, Debian_repository.job_apt_repo_debian Release);
      (Auto, Debian_repository.job_apt_repo_ubuntu Release);
    ] ;
  (* Minor *)
  Cacio.register_jobs
    Minor_release_tag
    [
      (Auto, job_docker_merge_manifests `real);
      (Auto, job_gitlab_release `real);
      (Manual, job_release_page `real `wait_for_build);
      (Auto, job_opam_release `real);
      (Auto, job_dispatch_call);
      (Auto, Debian_repository.job_apt_repo_debian Release);
      (Auto, Debian_repository.job_apt_repo_ubuntu Release);
    ] ;
  Cacio.register_jobs
    Minor_release_tag_test
    [
      (Auto, job_docker_merge_manifests `test);
      (Auto, job_docker_container_scanning `test);
      (Auto, job_gitlab_release `test);
      (Manual, job_release_page `test `wait_for_build);
      (Auto, job_opam_release `test);
      (Auto, job_docker_promote_to_latest `test_wait);
      (Auto, Debian_repository.job_apt_repo_debian Release);
      (Auto, Debian_repository.job_apt_repo_ubuntu Release);
    ] ;
  (* Beta *)
  Cacio.register_jobs
    Beta_release_tag
    [
      (Auto, job_docker_merge_manifests `real);
      (Auto, job_gitlab_release `real);
      (Manual, job_release_page `real `wait_for_build);
      (Auto, job_dispatch_call);
      (Auto, Debian_repository.job_apt_repo_debian Release);
      (Auto, Debian_repository.job_apt_repo_ubuntu Release);
    ] ;
  Cacio.register_jobs
    Beta_release_tag_test
    [
      (Auto, job_docker_merge_manifests `test);
      (Auto, job_docker_container_scanning `test);
      (Auto, job_gitlab_release `test);
      (Manual, job_release_page `test `wait_for_build);
      (Auto, Debian_repository.job_apt_repo_debian Release);
      (Auto, Debian_repository.job_apt_repo_ubuntu Release);
    ] ;
  (* Non-release *)
  Cacio.register_jobs
    Non_release_tag
    [
      (Auto, job_docker_merge_manifests `real);
      (Auto, job_gitlab_publish `non_release_tag);
      (Auto, Debian_repository.job_apt_repo_debian Release);
      (Auto, Debian_repository.job_apt_repo_ubuntu Release);
    ] ;
  Cacio.register_jobs
    Non_release_tag_test
    [
      (Auto, job_docker_merge_manifests `test);
      (Auto, job_docker_container_scanning `test);
      (Auto, job_gitlab_publish `non_release_tag);
      (Auto, Debian_repository.job_apt_repo_debian Release);
      (Auto, Debian_repository.job_apt_repo_ubuntu Release);
    ] ;
  (* Scheduled *)
  Cacio.register_jobs
    Scheduled_test_release
    [
      (Auto, job_docker_merge_manifests `test);
      (Auto, job_docker_container_scanning `test);
      (Auto, job_gitlab_publish `scheduled_test);
      (Auto, Debian_repository.job_apt_repo_debian Release);
      (Auto, Debian_repository.job_apt_repo_ubuntu Release);
    ] ;
  (* Release page *)
  Cacio.register_jobs
    Publish_release_page
    [(Manual, job_release_page `real `wait_for_nothing)] ;
  Cacio.register_jobs
    Test_publish_release_page
    [(Manual, job_release_page `test `wait_for_nothing)] ;
  (* Octez Latest Release *)
  Cacio.register_jobs
    Octez_latest_release
    [(Auto, job_docker_promote_to_latest `real)] ;
  Cacio.register_jobs
    Octez_latest_release_test
    [(Auto, job_docker_promote_to_latest `test)] ;
  ()

(** Create an Octez release tag pipeline of type {!pipeline_type},
    which is expected to be a release pipeline type. *)
let octez_jobs (pipeline_type : Cacio.global_pipeline) =
  [
    (* Stage: start *)
    job_datadog_pipeline_trace;
    (* Stage: build *)
    job_build_homebrew_release;
  ]
  @ Cacio.get_jobs pipeline_type

let job_docker_promote_to_version =
  Cacio.parameterize @@ fun mode ->
  CI.job
    "oc.docker:promote_revision_to_version"
    ~__POS__
    ~description:
      "Promote the Docker image from the packaging revision tag to the \
       canonical version tag (e.g., octez-v1.2 from octez-v1.2-1)"
    ~image:Images_external.docker
    ~stage:Publish
    ~allow_failure:No
    ~needs:[(Job, job_docker_merge_manifests mode)]
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
    ~services:[{name = "docker:${DOCKER_VERSION}-dind"}]
    ~variables:
      [
        ("DOCKER_VERSION", Docker.version);
        ("CI_DOCKER_HUB", match mode with `test -> "false" | `real -> "true");
      ]
    ~script:
      [
        "./scripts/ci/docker_initialize.sh";
        "./scripts/ci/docker_promote_to_version.sh";
      ]

let job_create_gitlab_package =
  CI.job
    "gitlab:create_package"
    ~__POS__
    ~image:Images.Base_images.ci_release
    ~stage:Publish
    ~description:
      "Create GitLab packages with static binaries from this packaging revision"
    ~needs:
      [
        ( Artifacts,
          Build.job_build_static_linux_released_binaries Amd64 `release );
        ( Artifacts,
          Build.job_build_static_linux_released_binaries Arm64 `release );
      ]
    ~id_tokens:Tezos_ci.id_tokens
    ~allow_failure:No
    ~script:["./scripts/ci/create_gitlab_package.sh"]
    ~retry:no_retry
    ~tag:Gcp_not_interruptible

let job_update_gitlab_release =
  CI.job
    "gitlab:update_release"
    ~__POS__
    ~image:Images.Base_images.ci_release
    ~stage:Publish
    ~description:
      "Update existing GitLab release with new static binaries from this \
       packaging revision"
    ~needs:[(Job, job_create_gitlab_package)]
    ~id_tokens:Tezos_ci.id_tokens
    ~script:["./scripts/releases/update_gitlab_release.sh"]
    ~retry:no_retry
    ~tag:Gcp_not_interruptible

let job_release_page_packaging_revision =
  Cacio.parameterize @@ fun mode ->
  CI.job
    "publish:release-page-packaging-revision"
    ~__POS__
    ~image:Images.CI.release_page
    ~stage:Publish
    ~description:
      "A job to update the Octez release page for a packaging revision. \
       Updates build number in versions.json, re-uploads binaries, and \
       regenerates the release page and RSS feed."
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~expire_in:(Duration (Days 1))
         ["index.html"; "older_releases.html"; "index.md"; "older_releases.md"])
    ?needs:
      (Some
         [
           ( Artifacts,
             Build.job_build_static_linux_released_binaries Amd64 `release );
           ( Artifacts,
             Build.job_build_static_linux_released_binaries Arm64 `release );
         ])
    ~variables:(release_page_variables ~mode)
    ~script:
      [
        "eval $(opam env)";
        "./scripts/releases/publish-release-page-packaging-revision.sh";
      ]

let () =
  Cacio.register_jobs
    Packaging_revision
    [
      (Manual, job_create_gitlab_package);
      (Auto, job_update_gitlab_release);
      (Manual, job_docker `real Amd64);
      (Manual, job_docker `real Arm64);
      (Auto, job_docker_merge_manifests `real);
      (Manual, job_docker_promote_to_version `real);
      (Manual, job_release_page_packaging_revision `real);
      (Manual, Debian_repository.job_build_data_packages);
      (Manual, Debian_repository.job_build_debian Release);
      (Manual, Debian_repository.job_build_ubuntu Release);
      (Auto, Debian_repository.job_apt_repo_debian Release);
      (Auto, Debian_repository.job_apt_repo_ubuntu Release);
    ] ;
  Cacio.register_jobs
    Packaging_revision_test
    [
      (Manual, job_create_gitlab_package);
      (Auto, job_update_gitlab_release);
      (Manual, job_docker `test Amd64);
      (Manual, job_docker `test Arm64);
      (Auto, job_docker_merge_manifests `test);
      (Manual, job_docker_promote_to_version `test);
      (Manual, job_release_page_packaging_revision `test);
      (Manual, Debian_repository.job_build_data_packages);
      (Manual, Debian_repository.job_build_debian Release);
      (Manual, Debian_repository.job_build_ubuntu Release);
      (Auto, Debian_repository.job_apt_repo_debian Release);
      (Auto, Debian_repository.job_apt_repo_ubuntu Release);
    ] ;
  ()

let octez_packaging_revision_jobs ?(test = false) () =
  (* We want to be able to trigger each "batch" of jobs manually.
     There are two batches: one with the static jobs, and one that publishes.
     The static jobs are independent so they are both manual,
     but [job_update_gitlab_release] depends on [job_create_gitlab_package]
     so it does not have to be manual, only [job_create_gitlab_package] does. *)
  [(* Stage: start *) job_datadog_pipeline_trace]
  @
  if test then Cacio.get_jobs Packaging_revision_test
  else Cacio.get_jobs Packaging_revision

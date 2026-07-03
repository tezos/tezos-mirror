(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module CI = Cacio.Shared

(* This constant is meant to replace similar constants that are defined elsewhere,
   in particular the one in [job_docker_authenticated] in [ci/lib_tezos_ci/tezos_ci.ml].
   During the migration to Cacio however, those other instances will continue to exist. *)
let version = Tezos_ci.Images.Base_images.docker_version

(* The CI image tags are resolved at runtime by the job's shell from the
   [Images.CI.runtime] dependency dotenv, exactly as the jobs' [image:] fields
   already reference them. *)
let docker_release_script =
  "./scripts/ci/docker_release.sh --runtime-image \
   ${ci_image_name}/runtime:${ci_image_tag} --build-deps-image \
   ${ci_image_name}/build:${ci_image_tag}"

(* [docker_release_script] extended for with-EVM builds with the L2 builder
   image: the [debian-rust:trixie] base image, used directly. Builds without
   EVM artifacts do not use it. *)
let release_script contents =
  match contents with
  | `experimental_with_evm ->
      Format.asprintf
        "%s --rust-toolchain-image %a"
        docker_release_script
        Tezos_ci.Image.pp
        Tezos_ci.Images.Base_images.debian_rust_trixie
  | `released | `experimental -> docker_release_script

let make_job_docker ~__POS__ ~name ~description ~scripts contents mode arch =
  let arch_str = Tezos_ci.Runner.Arch.show_uniform arch in
  CI.job
    name
    ~__POS__
    ~description
    ~stage:Build
    ~only_if_changed:
      [
        "build.Dockerfile";
        "Dockerfile";
        "docker-bake.hcl";
        (* The job runs this script, which in turn calls create_docker_image.sh. *)
        "scripts/ci/docker_release.sh";
        "scripts/create_docker_image.sh";
      ]
    ~allow_failure:No
    ~retry:
      {
        (* Set retry to 1 because the job is a bit flaky.
           The runner sometimes dies, causing the job to fail with EOF.
           Perhaps surprisingly, this surfaces as a [Script_failure]. *)
        Gitlab_ci.Types.max = 1;
        when_ = [Script_failure; Runner_system_failure];
      }
    ~arch
    ~image:Tezos_ci.Images.Base_images.alpine_docker_ci
      (* The L2 builder image (used for with-EVM builds) is the
         [debian-rust:trixie] base image, pulled directly by the build, so it is
         not a job dependency. *)
    ~image_dependencies:[Tezos_ci.Images.CI.runtime]
    ~services:[{name = Tezos_ci.Images.Base_images.dind_service}]
    ~variables:
      [
        ("DOCKER_VERSION", version);
        ("CI_DOCKER_HUB", match mode with `real -> "true" | `test -> "false");
        ( "DOCKER_BUILD_TARGET",
          match contents with
          | `experimental_with_evm -> "with-evm-artifacts"
          | `released | `experimental -> "without-evm-artifacts" );
        ("IMAGE_ARCH_PREFIX", arch_str ^ "_");
        ( "EXECUTABLE_FILES",
          match contents with
          | `experimental_with_evm | `experimental ->
              "script-inputs/released-executables \
               script-inputs/experimental-executables"
          | `released -> "script-inputs/released-executables" );
      ]
      (* Docker Hub credentials (CI_DOCKER_AUTH) are scoped to the
       [docker-publish] environment; only [`real] jobs (CI_DOCKER_HUB=true)
       authenticate and thus need access. *)
    ?environment:
      (match mode with
      | `real ->
          Some Gitlab_ci.Types.{name = "docker-publish"; action = Some Access}
      | `test -> None)
    ~script:scripts

let job_docker_snapshot =
  Cacio.parameterize @@ fun contents ->
  Cacio.parameterize @@ fun arch ->
  make_job_docker
    ~__POS__
    ~name:("oc.docker:snapshot:" ^ Tezos_ci.Runner.Arch.show_uniform arch)
    ~description:
      "Build the Docker image for Octez for the specified architecture, tagged \
       as [master-YYYYMMDD] for publishing to DockerHub."
    ~scripts:
      [
        "./scripts/ci/docker_initialize.sh --image-names";
        (* Override the image tag computed by [docker_initialize.sh --image-names]
           (which defaults to the branch name) with a dated master tag. *)
        "./scripts/ci/docker_image_names.sh --image-tag master-$(date +%Y%m%d)";
        release_script contents;
      ]
    contents
    `real
    arch

let job_docker =
  Cacio.parameterize @@ fun contents ->
  Cacio.parameterize @@ fun mode ->
  Cacio.parameterize @@ fun arch ->
  make_job_docker
    ~__POS__
    ~name:("oc.docker:" ^ Tezos_ci.Runner.Arch.show_uniform arch)
    ~description:
      "Build the Docker image for Octez for the specified architecture, with \
       experimental executables."
    ~scripts:
      [
        "./scripts/ci/docker_initialize.sh --image-names";
        release_script contents;
      ]
    contents
    mode
    arch

let make_job_docker_merge_manifests ~__POS__ ~name ~description ~needs ~scripts
    mode =
  CI.job
    name
    ~__POS__
    ~description
    ~stage:Publish
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
    ~image:Tezos_ci.Images.Base_images.alpine_docker_ci
    ~needs
    ~services:[{name = Tezos_ci.Images.Base_images.dind_service}]
    ~variables:
      [
        ("DOCKER_VERSION", version);
        ("CI_DOCKER_HUB", match mode with `real -> "true" | `test -> "false");
      ]
    ?environment:
      (match mode with
      | `real ->
          Some Gitlab_ci.Types.{name = "docker-publish"; action = Some Access}
      | `test -> None)
    ~script:scripts

let job_docker_merge_manifests_snapshot =
  Cacio.parameterize @@ fun contents ->
  make_job_docker_merge_manifests
    ~__POS__
    ~name:"docker:merge_manifests:snapshot"
    ~description:
      "Merge the Docker snapshot images generated by jobs \
       oc.docker:snapshot:{amd64,arm64} into a single multi-architecture image \
       tagged [master-YYYYMMDD]."
    ~needs:
      [
        (Job, job_docker_snapshot contents Amd64);
        ( Job,
          job_docker_snapshot
            (match contents with
            | `experimental_with_evm ->
                (* No EVM node on arm64. *) `experimental
            | `experimental | `released -> contents)
            Arm64 );
      ]
    ~scripts:
      [
        "./scripts/ci/docker_initialize.sh --image-names";
        (* Override the image tag computed by [docker_initialize.sh --image-names]
           (which defaults to the branch name) with a dated master tag. *)
        "./scripts/ci/docker_image_names.sh --image-tag master-$(date +%Y%m%d)";
        "./scripts/ci/docker_merge_manifests.sh";
      ]
    `real

let job_docker_merge_manifests =
  Cacio.parameterize @@ fun contents ->
  Cacio.parameterize @@ fun mode ->
  make_job_docker_merge_manifests
    ~__POS__
    ~name:"docker:merge_manifests"
    ~description:
      "Merge the Docker images generated by jobs oc.docker:{amd64,arm64} into \
       a single multi-architecture image."
    ~needs:
      [
        (Job, job_docker contents mode Amd64);
        ( Job,
          job_docker
            (* EVM node is not available on arm64. *)
            (match contents with
            | `experimental_with_evm -> `experimental
            | (`experimental | `released) as c -> c)
            mode
            Arm64 );
      ]
    ~scripts:
      [
        "./scripts/ci/docker_initialize.sh --image-names";
        "./scripts/ci/docker_merge_manifests.sh";
      ]
    mode

let job_docker_promote_weekly =
  CI.job
    "docker:promote_weekly"
    ~__POS__
    ~description:
      "Promote the master snapshot Docker image to the rolling [weekly] tag."
    ~stage:Publish
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
    ~needs:[(Job, job_docker_merge_manifests_snapshot `experimental_with_evm)]
    ~image:Tezos_ci.Images.Base_images.alpine_docker_ci
    ~services:[{name = "docker:${DOCKER_VERSION}-dind"}]
    ~variables:[("DOCKER_VERSION", version); ("CI_DOCKER_HUB", "true")]
    ~environment:Gitlab_ci.Types.{name = "docker-publish"; action = Some Access}
    ~script:
      [
        "./scripts/ci/docker_initialize.sh";
        "./scripts/ci/docker_image_names.sh --image-tag master-$(date +%Y%m%d)";
        "./scripts/ci/docker-promote.sh --target-tag weekly";
      ]

let job_script_docker_verify_image =
  Cacio.parameterize @@ fun arch ->
  let arch_string = Tezos_ci.Runner.Arch.show_uniform arch in
  CI.job
    ("oc.script.docker_verify_image_" ^ arch_string)
    ~__POS__
    ~description:"Verify the signature of the Docker image."
    ~stage:Test
    ~only_if_changed:
      [
        "build.Dockerfile";
        "Dockerfile";
        "docker-bake.hcl";
        (* Scripts that are directly called by this job. *)
        "scripts/ci/docker_initialize.sh";
        "scripts/ci/docker_verify_signature.sh";
        (* Scripts that are called by [docker_initialize.sh]. *)
        "scripts/ci/docker_wait_for_daemon.sh";
        "scripts/ci/docker_check_version.sh";
        "scripts/ci/docker_registry_auth.sh";
        (* Scripts that are called by [docker_initialize.sh --image-names]. *)
        "scripts/ci/docker_image_names.sh";
        (* Scripts that are called by [docker_image_names.sh]. *)
        "scripts/ci/docker_registry.inc.sh";
        (* Scripts that are called by the Docker build jobs that this job depends on. *)
        "scripts/ci/docker_release.sh";
        (* Scripts that are called by [docker_release.sh]. *)
        "scripts/create_docker_image.sh";
        "scripts/ci/docker_smoke_test.sh";
        "scripts/ci/docker_push_all.sh";
        "scripts/ci/docker_sign.sh";
        (* TODO: since the Docker build jobs already call [docker_verify_signature.sh],
           why have a job dedicated to running [docker_verify_signature.sh]? *)
        "scripts/ci/docker_verify_signature.sh";
        (* Scripts that are called by [create_docker_image.sh]. *)
        "scripts/version.sh";
      ]
    ~needs:
      [
        ( Job,
          job_docker
            (match arch with
            | Amd64 -> `experimental_with_evm
            | Arm64 -> `experimental)
            `test
            arch );
      ]
    ~image:Tezos_ci.Images.Base_images.alpine_docker_ci
    ~variables:
      [("DOCKER_VERSION", version); ("IMAGE_ARCH_PREFIX", arch_string ^ "_")]
    ~services:[{name = Tezos_ci.Images.Base_images.dind_service}]
    ~script:
      [
        "./scripts/ci/docker_initialize.sh --image-names";
        "./scripts/ci/docker_verify_signature.sh";
      ]

let register () =
  Cacio.register_jobs
    Before_merging
    [
      (Auto, job_docker `experimental_with_evm `test Amd64);
      (Auto, job_docker `experimental `test Arm64);
      (Manual, job_script_docker_verify_image Amd64);
      (Manual, job_script_docker_verify_image Arm64);
    ] ;
  Cacio.register_jobs
    Merge_train
    [
      (Auto, job_docker `experimental_with_evm `test Amd64);
      (Auto, job_docker `experimental `test Arm64);
    ] ;
  Cacio.register_jobs
    Master
    [(Auto, job_docker_merge_manifests `experimental_with_evm `real)] ;
  Cacio.register_jobs
    Scheduled_docker_build
    [(Auto, job_docker_merge_manifests `experimental_with_evm `real)] ;
  Cacio.register_jobs
    Scheduled_docker_master_snapshot
    [
      (Auto, job_docker_merge_manifests_snapshot `experimental_with_evm);
      (Auto, job_docker_promote_weekly);
    ] ;
  ()

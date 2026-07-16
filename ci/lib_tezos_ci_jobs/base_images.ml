(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Gitlab_ci.Types
open Tezos_ci
module CI = Cacio.Shared

(* Helper function to standardise the path of a base image built in the same
   pipeline. Used to build more complex base images and avoid code duplications *)
let base_dep_img_name image =
  let prefix = "${GCP_REGISTRY}/$CI_PROJECT_NAMESPACE/tezos" in
  let tag = "${RELEASE}-${CI_COMMIT_REF_SLUG}" in
  Format.sprintf "%s/%s:%s" prefix image tag

type compilation =
  | Amd64_only (* Built on amd64 runner *)
  | Arm64_only
    (* Built on arm64 runner. The image will be suffixed with -arm64 *)
  | Emulated
    (* Built on amd64 runners. Arm64 image is built using qumu.
       Use 1 runner / 1 job to create one multi-arch image *)
  | Native
(* Both amd64 and arm64 images are built on their respective native architectures
   Use 3 runners. 2 jobs for building the images, one job for merging the manifest
   must be added to create one multi-arch image. Both images are suffixed,
   repesctively, with -amd64 or -arm64 *)

type upstream_image = Pipeline_dep of string | Upstream of string
(* Datatype to differenciate between variable passed with IMAGE_PATH to the
   dockerfile via --build-arg IMAGE=IMAGE_PATH.
   Upstream is the name of an upstream image ( eg. debian:trixie )
   Pipeline_dep is the name of an image generate in this pipeline ( eg.
   ${GCP_REGISTRY}/$CI_PROJECT_NAMESPACE/tezos/debian:trixie-$COMMIT_REF_SLUG )
*)

module Files = struct
  let build_script = ["scripts/ci/build-base-images.sh"]

  (* Used in jobs merging manifests of natively build Docker images. *)
  let merge_script = ["scripts/ci/docker-merge-base-images.sh"]

  (* Direct changesets of jobs, i.e. files directly used by the corresponding jobs.

     The full changeset of a job should also contain the files indirectly used.
     - Example: [image A] is built on top of [image B], then the
     changeset of the image A build ([job A])job should contain the
     files needed to build B.


     Also, the full changeset of a job should contain the changeset of the jobs that depend on it.
     - Example: [image A] is built on top of [image B] so [job A] depends on
     [job B]. If [job A] is in the pipeline then [job B] should be too. So, the
     full changeset of [job B] needs to include the changeset of [job A].
     NB: This is enforced by Cacio, so this part will be simplified if the jobs are migrated.

   *)

  let debian_base =
    [
      "images/base-images/Dockerfile.debian";
      (* scripts in Dockerfile *)
      "scripts/kiss-fetch.sh";
      "images/scripts/install_datadog_static.sh";
      "images/scripts/install-gcloud.sh";
    ]
    @ build_script

  let debian_homebrew =
    [
      "images/base-images/Dockerfile.debian-homebrew";
      (* script used in Dockerfile *)
      "scripts/packaging/homebrew_install.sh";
    ]
    @ build_script

  let debian_build =
    [
      "images/base-images/Dockerfile.debian-build";
      (* files copied in Dockerfile except Makefile as many changes to
         it should be irrelevant to debian packages *)
      "images/scripts/install_sccache_static.sh";
      "images/scripts/install_opam_static.sh";
      "scripts/kiss-fetch.sh";
      "scripts/kiss-logs.sh";
      "scripts/version.sh";
      "scripts/install_build_deps.sh";
      "scripts/install_build_deps.rust.sh";
      "opam/virtual/octez-deps.opam.locked";
      "opam/virtual/release-tools-deps.opam.locked";
      "opam/virtual/dream-httpaf.opam.locked";
      "opam/virtual/dream.opam.locked";
    ]
    @ build_script

  let debian_systemd =
    [
      "images/base-images/Dockerfile.debian-systemd";
      (* scripts in Dockerfile *)
      "scripts/ci/systemd-entrypoint.service";
      "scripts/ci/systemd-entrypoint.sh";
      "scripts/ci/octez-packages-version.sh";
      "scripts/packaging/tests/tests-common.inc.sh";
      "script-inputs/active_protocol_versions_without_number";
    ]
    @ build_script

  let debian_jsonnet =
    ["images/base-images/Dockerfile.debian-jsonnet"] @ build_script

  let debian_rust_build =
    [
      "images/base-images/Dockerfile.rust";
      (* script used in Dockerfile *)
      "images/scripts/install_sccache_static.sh";
    ]
    @ build_script

  let rust_sdk_bindings =
    [
      "images/base-images/Dockerfile.rust-sdk-bindings";
      (* script used in Dockerfile *)
      "images/scripts/install_sccache_static.sh";
    ]
    @ build_script

  let alpine_docker_ci =
    [
      "images/base-images/Dockerfile.alpine-docker-ci";
      "images/scripts/install-gcloud-apk.sh";
      "scripts/ci/docker_initialize.sh";
      "images/scripts/install_datadog_static.sh";
    ]
    @ build_script

  let ci_releases =
    [
      "scripts/kiss-fetch.sh";
      "images/scripts/install-release-cli.sh";
      "images/scripts/install_datadog_static.sh";
      "images/scripts/install-gcloud.sh";
      "images/base-images/Dockerfile.debian-release";
    ]

  (* Inputs of the static "alpine-*" CI images built by [docker buildx bake]
     (images/ci/ci-images.hcl): the bake file, the per-image Dockerfiles and
     everything they COPY from the build context (repository root). *)
  let ci_images =
    [
      "images/ci/**";
      "opam/virtual/*.opam.locked";
      "pyproject.toml";
      "poetry.lock";
      "scripts/version.sh";
      "scripts/kiss-fetch.sh";
      "scripts/kiss-logs.sh";
      "scripts/install_dal_trusted_setup.sh";
      "images/scripts/install_opam_static.sh";
      "images/scripts/install_sccache_static.sh";
      "images/scripts/install_datadog_static.sh";
      "images/common/install-nvm.sh";
    ]
end

module Distribution = struct
  type t = Debian | Ubuntu

  let name = function Debian -> "debian" | Ubuntu -> "ubuntu"

  let releases = function
    | Debian -> ["bookworm"; "trixie"]
    | Ubuntu -> ["22.04"; "24.04"; "26.04"]

  let release_matrix distro = [("RELEASE", releases distro)]

  let dockerfile = function
    | Debian | Ubuntu -> "images/base-images/Dockerfile.debian"
end

(* ── Cacio helpers ───────────────────────────────────────────────────────── *)

(* Partial application of [CI.job] that fixes the docker-auth settings:
   alpine_docker_ci image, dind service, DOCKER_VERSION variable, and
   docker_initialize.sh prepended to the script (Cacio has no before_script).
   All other [CI.job] arguments are left for the caller. *)
let docker_job ?(extra_variables = []) ?(retry = Tezos_ci.dind_retry) ~script =
  CI.job
    ~stage:Cacio.Build
    ~image:Images.Base_images.alpine_docker_ci
    ~services:[{name = Images.Base_images.dind_service}]
    ~retry
    ~variables:
      (("DOCKER_VERSION", Images.Base_images.docker_version) :: extra_variables)
    ~script:("./scripts/ci/docker_initialize.sh" :: script)

(* Partial application of [docker_job] that fixes the image-build variables
   (DISTRIBUTION, IMAGE_PATH, PLATFORM) and the compilation strategy
   (tag and matrix). All other arguments are left for the caller. *)
let base_image_job ~image_name ?(base_name = Upstream image_name) ~matrix
    ~compilation ?(extra_variables = []) dockerfile =
  let platform, extra_tags =
    match compilation with
    | Amd64_only -> ("linux/amd64", [])
    | Arm64_only -> ("", [("TAGS", [Runner.Tag.show Gcp_arm64])])
    | Emulated -> ("linux/amd64,linux/arm64", [])
    | Native ->
        ( "",
          [
            ( "TAGS",
              [Runner.Tag.show Gcp_very_high_cpu; Runner.Tag.show Gcp_arm64] );
          ] )
  in
  let emulated = extra_tags = [] in
  docker_job
    ~extra_variables:
      ([
         ("DISTRIBUTION", image_name);
         ( "IMAGE_PATH",
           match base_name with
           | Upstream name -> Format.asprintf "%s:$RELEASE" name
           | Pipeline_dep name -> base_dep_img_name name );
         ("PLATFORM", platform);
       ]
      @ extra_variables)
    ~script:[Printf.sprintf "scripts/ci/build-base-images.sh %s" dockerfile]
    ~retry:Tezos_ci.dind_retry
    ~tag:(if emulated then Gcp_very_high_cpu else Dynamic)
    ~parallel:(Matrix [matrix @ extra_tags])

(* ── Base distribution jobs ─────────────────────────────────────────────── *)

(* ── Cacio: debian ───────────────────────────────────────────────────────── *)

(* Root job. Cacio propagates the changeset to dependent jobs automatically
   via [~needs], so only the files this job directly depends on are listed. *)
let job_debian_based_images =
  base_image_job
    ~image_name:"debian"
    ~matrix:Distribution.(release_matrix Debian)
    ~compilation:Emulated
    (Distribution.dockerfile Debian)
    ~__POS__
    ~description:"Build debian base images"
    ~only_if_changed:Files.debian_base
    "images.debian"

(* ── Cacio: ubuntu ───────────────────────────────────────────────────────── *)

let job_ubuntu_based_images =
  base_image_job
    ~image_name:"ubuntu"
    ~matrix:Distribution.(release_matrix Ubuntu)
    ~compilation:Emulated
    (Distribution.dockerfile Ubuntu)
    ~__POS__
    ~description:"Build ubuntu base images"
    ~only_if_changed:Files.debian_base
    "images.ubuntu"

(* ── Standalone jobs (no job dependencies within this file) ─────────────── *)

(* ── Cacio: ci-release ───────────────────────────────────────────────────── *)

let job_ci_release_based_images =
  base_image_job
    ~image_name:"ci-release"
    ~base_name:(Pipeline_dep "debian")
    ~matrix:[("RELEASE", ["trixie"])]
    ~compilation:Amd64_only
    "images/base-images/Dockerfile.debian-release"
    ~__POS__
    ~description:"Build ci-release base images"
    ~only_if_changed:Files.(ci_releases @ debian_base)
    ~needs:[(Cacio.Job, job_debian_based_images)]
    "images.ci-release"

(* ── Cacio: alpine-docker-ci ─────────────────────────────────────────────── *)

(* This job is special: it bootstraps from the upstream docker:<version> image
   (not alpine-docker-ci itself) and has a custom build procedure. *)
let job_docker_ci_based_images =
  let docker_version = Images.Base_images.docker_version in
  CI.job
    "images.alpine-docker-ci"
    ~__POS__
    ~description:
      "Build alpine-docker-ci base images (bootstrapped from upstream docker)"
    ~stage:Cacio.Build
    ~image:Images.upstream_docker
    ~services:[{name = Images.Base_images.dind_service}]
    ~retry:Tezos_ci.dind_retry
    ~tag:Gcp_very_high_cpu
    ~only_if_changed:Files.alpine_docker_ci
    ~variables:
      [
        ("RELEASE", docker_version);
        ("DISTRIBUTION", "alpine-docker-ci");
        ("IMAGE_PATH", "");
        ("GCLOUD_VERSION", "543.0.0");
        ("HADOLINT_VERSION", "2.10.0");
        ("DOCKER_VERSION", docker_version);
        ("DOCKER_DIGEST", Images.Base_images.docker_digest);
        ("REGCTL_VERSION", "v0.4.3");
        ("PLATFORM", "linux/amd64,linux/arm64");
        ("CI_DOCKER_HUB", "false");
      ]
    ~script:
      [
        "images/scripts/install-gcloud-apk.sh";
        "export PATH=$PATH:/google-cloud-sdk/bin";
        "scripts/ci/docker_initialize.sh";
        "scripts/ci/build-base-images.sh \
         images/base-images/Dockerfile.alpine-docker-ci";
      ]

(* ── Rust family ────────────────────────────────────────────────────────── *)

(* ── Cacio: debian-rust ──────────────────────────────────────────────────── *)

let job_rust_based_images =
  base_image_job
    ~image_name:"debian-rust"
    ~base_name:(Pipeline_dep "debian")
    ~matrix:[("RELEASE", ["trixie"])]
    ~compilation:Native
    "images/base-images/Dockerfile.rust"
    ~__POS__
    ~description:"Build debian-rust base images"
    ~only_if_changed:Files.(debian_rust_build @ debian_base)
    ~needs:[(Cacio.Job, job_debian_based_images)]
    "images.debian-rust"

(* ── Cacio: debian-rust.merge ────────────────────────────────────────────── *)

(* Note: dedicated merge job exist because QEMU compilation takes too much time.
   Without them the build job reaches a timeout. *)

let job_rust_based_images_merge =
  docker_job
    ~extra_variables:
      [
        ("RELEASE", "trixie");
        ("IMAGE_NAME", "${GCP_REGISTRY}/tezos/tezos/debian-rust");
      ]
    ~script:["scripts/ci/docker-merge-base-images.sh"]
    ~__POS__
    ~description:"Merge debian-rust base image manifests"
    ~only_if_changed:Files.(merge_script @ debian_rust_build @ debian_base)
    ~needs:[(Cacio.Job, job_rust_based_images)]
    "images.debian-rust.merge"

(* ── debian-homebrew ────────────────────────────────────────────────────── *)

(* ── Cacio: debian-homebrew ──────────────────────────────────────────────── *)

(* debian_base is included so that a change to debian files triggers a
   rebuild of this derived image even when the debian job itself is not
   explicitly in scope. *)
let job_debian_homebrew_based_images =
  base_image_job
    ~image_name:"debian-homebrew"
    ~base_name:(Pipeline_dep "debian")
    ~matrix:[("RELEASE", ["trixie"])]
    ~compilation:Amd64_only
    "images/base-images/Dockerfile.debian-homebrew"
    ~__POS__
    ~description:"Build debian-homebrew base images"
    ~only_if_changed:Files.(debian_homebrew @ debian_base)
    ~needs:[(Cacio.Job, job_debian_based_images)]
    "images.debian-homebrew"

(* ── debian-build and ubuntu-build families ─────────────────────────────── *)

(* ── Cacio: debian-build ─────────────────────────────────────────────────── *)

let job_debian_build_based_images =
  base_image_job
    ~image_name:"debian-build"
    ~base_name:(Pipeline_dep "debian")
    ~matrix:Distribution.(release_matrix Debian)
    ~compilation:Native
    "images/base-images/Dockerfile.debian-build"
    ~__POS__
    ~description:"Build debian-build base images"
    ~only_if_changed:Files.(debian_build @ debian_base)
    ~needs:[(Cacio.Job, job_debian_based_images)]
    "images.debian-build"

(* ── Cacio: debian-build.merge ───────────────────────────────────────────── *)

let job_debian_build_based_images_merge =
  docker_job
    ~extra_variables:
      [("IMAGE_NAME", "${GCP_REGISTRY}/tezos/tezos/debian-build")]
    ~script:["scripts/ci/docker-merge-base-images.sh"]
    ~__POS__
    ~description:"Merge debian-build base image manifests"
    ~parallel:(Matrix [Distribution.(release_matrix Debian)])
    ~only_if_changed:Files.(merge_script @ debian_build @ debian_base)
    ~needs:[(Cacio.Job, job_debian_build_based_images)]
    "images.debian-build.merge"

(* ── Cacio: ubuntu-build ─────────────────────────────────────────────────── *)

let job_ubuntu_build_based_images =
  base_image_job
    ~image_name:"ubuntu-build"
    ~base_name:(Pipeline_dep "ubuntu")
    ~matrix:Distribution.(release_matrix Ubuntu)
    ~compilation:Native
    "images/base-images/Dockerfile.debian-build"
    ~__POS__
    ~description:"Build ubuntu-build base images"
    ~only_if_changed:Files.(debian_build @ debian_base)
    ~needs:[(Cacio.Job, job_ubuntu_based_images)]
    "images.ubuntu-build"

(* ── Cacio: ubuntu-build.merge ───────────────────────────────────────────── *)

let job_ubuntu_build_based_images_merge =
  docker_job
    ~extra_variables:
      [("IMAGE_NAME", "${GCP_REGISTRY}/tezos/tezos/ubuntu-build")]
    ~script:["scripts/ci/docker-merge-base-images.sh"]
    ~__POS__
    ~description:"Merge ubuntu-build base image manifests"
    ~parallel:(Matrix [Distribution.(release_matrix Ubuntu)])
    ~only_if_changed:Files.(merge_script @ debian_build @ debian_base)
    ~needs:[(Cacio.Job, job_ubuntu_build_based_images)]
    "images.ubuntu-build.merge"

(* ── Systemd images ──────────────────────────────────────────────────────── *)

(* ── Cacio: debian-systemd ───────────────────────────────────────────────── *)

let job_debian_systemd_based_images =
  base_image_job
    ~image_name:"debian-systemd"
    ~base_name:(Pipeline_dep "debian")
    ~matrix:Distribution.(release_matrix Debian)
    ~compilation:Amd64_only
    "images/base-images/Dockerfile.debian-systemd"
    ~__POS__
    ~description:"Build debian-systemd base images"
    ~only_if_changed:Files.(debian_systemd @ debian_base)
    ~needs:[(Cacio.Job, job_debian_based_images)]
    "images.debian-systemd"

(* ── Cacio: ubuntu-systemd ───────────────────────────────────────────────── *)

let job_ubuntu_systemd_based_images =
  base_image_job
    ~image_name:"ubuntu-systemd"
    ~base_name:(Pipeline_dep "ubuntu")
    ~matrix:Distribution.(release_matrix Ubuntu)
    ~compilation:Amd64_only
    "images/base-images/Dockerfile.debian-systemd"
    ~__POS__
    ~description:"Build ubuntu-systemd base images"
    ~only_if_changed:Files.(debian_systemd @ debian_base)
    ~needs:[(Cacio.Job, job_ubuntu_based_images)]
    "images.ubuntu-systemd"

(* ── debian-jsonnet ──────────────────────────────────────────────────────── *)

(* ── Cacio: debian-jsonnet ───────────────────────────────────────────────── *)

let job_jsonnet_based_images =
  base_image_job
    ~image_name:"debian-jsonnet"
    ~base_name:(Pipeline_dep "debian")
    ~matrix:[("RELEASE", ["trixie"])]
    ~compilation:Amd64_only
    "images/base-images/Dockerfile.debian-jsonnet"
    ~__POS__
    ~description:"Build debian-jsonnet base images"
    ~only_if_changed:Files.(debian_jsonnet @ debian_base)
    ~needs:[(Cacio.Job, job_debian_based_images)]
    "images.debian-jsonnet"

(* ── rust-sdk-bindings ───────────────────────────────────────────────────── *)

(* ── Cacio: rust-sdk-bindings ────────────────────────────────────────────── *)

let job_rust_sdk_bindings_based_images =
  base_image_job
    ~image_name:"debian-rust-sdk-bindings"
    ~base_name:(Pipeline_dep "debian")
    ~matrix:[("RELEASE", ["trixie"])]
    ~compilation:Amd64_only
    "images/base-images/Dockerfile.rust-sdk-bindings"
    ~__POS__
    ~description:"Build debian-rust-sdk-bindings base images"
    ~only_if_changed:Files.(rust_sdk_bindings @ debian_base)
    ~needs:[(Cacio.Job, job_debian_based_images)]
    "images.debian-rust-sdk-bindings"

(* ── Cacio: static "alpine-*" CI images (docker buildx bake) ─────────────── *)

(* Build the static "alpine-*" CI images (runtime, monitoring, prebuild,
   build, test, e2etest, release-page) with [docker buildx bake]
   (images/ci/ci-images.hcl). Each architecture is built natively; the
   per-arch images are then merged into multi-arch manifests by
   [job_alpine_ci_merge].

   [--allow=fs.read=/tmp/npm_token.txt] grants the filesystem entitlement that
   recent buildx requires to read the npm_token secret declared on the e2etest
   target (its src is outside the build context). *)
let job_alpine_ci =
  Cacio.parameterize @@ fun arch ->
  let arch_str = Runner.Arch.show_uniform arch in
  docker_job
    ~extra_variables:
      [
        ("REGISTRY", "${GCP_REGISTRY}/${CI_PROJECT_NAMESPACE}/tezos");
        ( "TAG",
          Format.sprintf
            "${CI_COMMIT_REF_SLUG}-${CI_COMMIT_SHORT_SHA}-%s"
            arch_str );
      ]
    ~script:
      [
        "docker buildx create --driver docker-container --use --name tezos";
        Format.sprintf
          "docker buildx bake --allow=fs.read=/tmp/npm_token.txt --push --set \
           \"*.platform=linux/%s\" -f images/ci/ci-images.hcl"
          arch_str;
      ]
    ~__POS__
    ~description:("Build the static alpine-* CI images for " ^ arch_str)
    ~timeout:(Minutes 90)
    ~arch
      (* amd64 builds on a very-high-CPU runner to shorten the build; arm64
         builds on a ramfs (memory-backed) work volume for build I/O. *)
    ?cpu:
      (match arch with
      | Runner.Arch.Amd64 -> Some Runner.CPU.Very_high
      | _ -> None)
    ?storage:
      (match arch with
    | Runner.Arch.Arm64 -> Some Runner.Storage.Ramfs
    | _ -> None)
      (* Inherits [dind_retry] from [docker_job], which retries on both
         [Script_failure] and [Runner_system_failure] to recover preemptions. *)
    ~only_if_changed:Files.ci_images
    ("images.alpine-ci-all:" ^ arch_str)

let job_alpine_ci_merge =
  docker_job
    ~extra_variables:
      [
        ("REGISTRY", "${GCP_REGISTRY}/${CI_PROJECT_NAMESPACE}/tezos");
        ("TAG", "${CI_COMMIT_REF_SLUG}-${CI_COMMIT_SHORT_SHA}");
      ]
    ~script:
      ("docker buildx create --driver docker-container --use --name tezos"
      :: List.map
           (fun image_short_name ->
             Format.sprintf
               "docker buildx imagetools create --tag \
                \"${REGISTRY}/alpine-%s:${TAG}\" \
                \"${REGISTRY}/alpine-%s:${TAG}-amd64\" \
                \"${REGISTRY}/alpine-%s:${TAG}-arm64\""
               image_short_name
               image_short_name
               image_short_name)
           [
             "runtime";
             "monitoring";
             "prebuild";
             "build";
             "release-page";
             "test";
             "e2etest";
           ])
    ~__POS__
    ~description:
      "Merge the per-arch static alpine-* CI images into multi-arch manifests"
    ~only_if_changed:Files.ci_images
    ~needs:
      [
        (Cacio.Job, job_alpine_ci Runner.Arch.Amd64);
        (Cacio.Job, job_alpine_ci Runner.Arch.Arm64);
      ]
    "images.alpine-ci-all.merge"

(* ── Cacio pipeline registrations ───────────────────────────────────────── *)

let () =
  let jobs =
    [
      (Cacio.Auto, job_ci_release_based_images);
      (Cacio.Auto, job_docker_ci_based_images);
      (Cacio.Auto, job_debian_homebrew_based_images);
      (Cacio.Auto, job_jsonnet_based_images);
      (Cacio.Auto, job_rust_sdk_bindings_based_images);
      (Cacio.Auto, job_debian_systemd_based_images);
      (Cacio.Auto, job_ubuntu_systemd_based_images);
      (Cacio.Auto, job_debian_build_based_images);
      (Cacio.Auto, job_debian_build_based_images_merge);
      (Cacio.Auto, job_ubuntu_build_based_images);
      (Cacio.Auto, job_ubuntu_build_based_images_merge);
      (Cacio.Auto, job_rust_based_images);
      (Cacio.Auto, job_rust_based_images_merge);
      (Cacio.Auto, job_debian_based_images);
      (Cacio.Auto, job_ubuntu_based_images);
      (Cacio.Auto, job_alpine_ci Runner.Arch.Amd64);
      (Cacio.Auto, job_alpine_ci Runner.Arch.Arm64);
      (Cacio.Auto, job_alpine_ci_merge);
    ]
  in
  Cacio.register_merge_request_jobs jobs ;
  Cacio.register_jobs Cacio.Base_images_daily jobs

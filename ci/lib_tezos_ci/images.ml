(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* The image catalogs: external images, and the base images built by
   the [base_images] pipelines. *)

(* [Base_images.base_images_tag] fails at module initialisation, so this
   module must be initialised after [Util] registers the printer that renders
   the failure as its message. Mentioning [Util] makes sure of that. It is
   opened, as in the rest of the library, so that [failwith] is [Util.failwith]
   everywhere. *)
open Util

(* The docker version used in Docker-in-Docker CI jobs.
   This is the version of the Docker daemon available in the
   [alpine_docker_ci] base image (alpine-docker-ci).
   https://hub.docker.com/layers/library/docker/28.5.1-dind/images/sha256-bfb73322f4302ed5d380fcba01957ba871458a32245d0862758b08e0fe7916d4 *)
let docker_version = "28.5.1"

(* Index digest of the upstream [docker:<docker_version>] image. The
   [docker:<v>] (CLI) and [docker:<v>-dind] (daemon) tags share the same
   index digest, so this is used to pin both: the [docker:<v>-dind] service,
   the [docker:<v>] builder image ([upstream_docker]), and the
   [FROM docker:<v>] base of the alpine-docker-ci image. Update together with
   [docker_version]. *)
let docker_digest =
  "sha256:ea9d20492ca1caaaba78e68453433895d256173c79281756e88b745647fcbcfd"

module Image = struct
  (* Images are built outside of the pipelines that use them (see the
     [base_images] pipelines) and referenced by their path. *)
  type t = Gitlab_ci.Types.image

  let mk_external ~image_path : t = Image image_path

  let pp ppf (img : t) =
    match img with Image image_name -> Format.fprintf ppf "%s" image_name
end

(* Register external images.

   Use this module to register images that are built outside the
   [tezos/tezos] CI. *)
module Images_external = struct
  (* https://hub.docker.com/layers/nixos/nix/2.34.6/images/sha256-ffb94901e4ef854fe5a8e1a3170a1b8480ed505eaaa37091809664390acd8d20 *)
  let nix =
    Image.mk_external
      ~image_path:
        "nixos/nix:2.34.6@sha256:e2fe74e96e965653c7b8f16ac64d1e56581c63c84d7fa07fb0692fd055cd06b0"

  (* Upstream Docker image from Docker Hub, used as the builder image
     for the [alpine-docker-ci] base image job in [base_images.ml].
     Version and digest are defined alongside [docker_digest] above. *)
  let upstream_docker =
    Image.mk_external
      ~image_path:(sf "docker:%s@%s" docker_version docker_digest)

  (* Image used in initial pipeline job that sends to Datadog useful
     info for CI visibility.

     https://hub.docker.com/layers/datadog/ci/v5.18.0/images/sha256-07475bc65fc5efec461feeaca7a16cc32c13742393db6cce5352fc5f7e1bd5a2

     The [datadog-ci] version must be kept consistent across all CI
     images that use it. It lives in two independent places that have
     to be bumped together:
       - the external [datadog/ci] image pinned below (tag + Docker Hub
         digest), and
       - the static binary installer [images/scripts/install_datadog_static.sh]
         (the [DATADOG_RELEASE] variable AND the four GitHub-release
         SHA-256 checksums), used by the internal [monitoring] and
         [alpine_docker_ci] images.
     When bumping the version, update BOTH: a new tag here implies a new
     Docker Hub digest, and a new [DATADOG_RELEASE] there implies four new
     checksums. *)

  let datadog_ci =
    Image.mk_external
      ~image_path:
        "datadog/ci:v5.18.0@sha256:07475bc65fc5efec461feeaca7a16cc32c13742393db6cce5352fc5f7e1bd5a2"

  (* https://hub.docker.com/layers/hadolint/hadolint/v2.14.0-alpine/images/sha256-be27962427a85de242820cb710a374478cce9bfb534a2c07e4fa54741d98908f *)
  let hadolint =
    Image.mk_external
      ~image_path:
        "hadolint/hadolint:2.14.0-alpine@sha256:7aba693c1442eb31c0b015c129697cb3b6cb7da589d85c7562f9deb435a6657c"

  (* We specify the semgrep image by hash to avoid flakiness. Indeed, if we took the
     latest release, then an update in the parser or analyser could result in new
     errors being found even if the code doesn't change. This would place the
     burden for fixing the code on the wrong dev (the devs who happen to open an
     MR coinciding with the semgrep update rather than the dev who wrote the
     infringing code in the first place).

     https://hub.docker.com/layers/returntocorp/semgrep-agent/sha-c6cd7cf/images/sha256-431ee298b85c9595238614d5866e06d1b72030f915df5ee773fb04a3d0933a24

     Update the hash in scripts/semgrep/README.md too when updating it here
     Last update: 2022-01-03

     NB: this image seems unmaintained (latest version
     [returntocorp/semgrep-agent:sha-688b98c] is from October
     2023). We should consider switching to [semgrep/semgrep]
     https://semgrep.dev/docs/update. *)
  let semgrep_agent =
    Image.mk_external
      ~image_path:
        "returntocorp/semgrep-agent:sha-c6cd7cf@sha256:431ee298b85c9595238614d5866e06d1b72030f915df5ee773fb04a3d0933a24"

  (* Image provided by GitLab. More details in the doc:
     - https://docs.gitlab.com/ee/ci/runners/hosted_runners/macos.html#supported-macos-images
     - https://gitlab-org.gitlab.io/ci-cd/shared-runners/images/macos-image-inventory/macos-15-xcode-16/ *)
  let macosx_15 = Image.mk_external ~image_path:"macos-15-xcode-16"

  (* Image used in [schedule_security_scans] pipeline. Trivy scans
     Docker images and codebase for CVEs and SBOMs. *)
  let trivy =
    Image.mk_external
      ~image_path:
        "aquasec/trivy:0.69.3@sha256:bcc376de8d77cfe086a917230e818dc9f8528e3c852f7b1aff648949b6258d1c"

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/8325
     Add npm to the base images and the datadog CI image
     so that we can use those instead of this external image. *)
  let node_alpine = Image.mk_external ~image_path:"node:22-alpine"
end

(* Register base images.

   Use this module to register images that are built in the base
   image pipelines of [tezos/tezos] and pushed to the protected
   registry (the build jobs push to [${GCP_REGISTRY}], which resolves to the
   protected registry on protected refs). *)
module Base_images = struct
  let path_prefix = "${GCP_PROTECTED_REGISTRY}/tezos/tezos"

  (* Version tag shared by all base images below (built together by the
     [base_images.daily] pipeline). Its value lives in [ci/ci_version.sh] and
     is read from the [base_images_tag] environment variable, which the CI
     build sources before running the generator (see [ci/makefile]). *)
  let base_images_tag =
    match Sys.getenv_opt "base_images_tag" with
    | Some tag when tag <> "" -> tag
    | None | Some _ ->
        failwith
          "environment variable base_images_tag is unset or empty; source \
           ci/ci_version.sh before running the CI generator"

  let make_img distro =
    Image.mk_external
      ~image_path:(sf "%s/%s-%s" path_prefix distro base_images_tag)

  (* DEB packaging *)
  let debian_trixie = make_img "debian:trixie"

  let debian_build_trixie = make_img "debian-build:trixie"

  let ubuntu_22_04 = make_img "ubuntu:22.04"

  let ubuntu_24_04 = make_img "ubuntu:24.04"

  let ubuntu_build_24_04 = make_img "ubuntu-build:24.04"

  let ubuntu_26_04 = make_img "ubuntu:26.04"

  let debian_jsonnet_trixie = make_img "debian-jsonnet:trixie"

  let debian_homebrew_trixie = make_img "debian-homebrew:trixie"

  (* [debian-rust-trixie] contains libclang for building rocksdb in CI. *)
  let debian_rust_trixie = make_img "debian-rust:trixie"

  (* [debian-rust-sdk-bindings-trixie]: all dependencies required to build the
     Rust SDK bindings. Built by [images.debian-rust-sdk-bindings]. *)
  let debian_rust_sdk_bindings = make_img "debian-rust-sdk-bindings:trixie"

  let docker_version = docker_version

  let docker_digest = docker_digest

  let dind_service = sf "docker:%s-dind@%s" docker_version docker_digest

  let alpine_docker_ci = make_img (sf "alpine-docker-ci:%s" docker_version)

  (* [ci-release] is built on top of the internal [debian] base image (so it
     inherits jq, gcloud, datadog, ...). Since !22370. *)
  let ci_release = make_img "ci-release:trixie"

  (* [alpine-*] CI images, built and pushed by the [images.alpine-ci-all] jobs.
     Unlike the other base images they have no [<release>] segment (e.g.
     [debian:bookworm]), so [make_img] -- which appends [-<tag>] to a
     [<distro>:<release>] name -- would mangle the tag separator. Reference them
     as [alpine-<component>:<base_images_tag>] instead.
     TODO (#8378): give them a release version so they follow the standard
     scheme and can reuse [make_img]. *)
  let mk_alpine_ci_img name =
    Image.mk_external
      ~image_path:(sf "%s/alpine-%s:%s" path_prefix name base_images_tag)

  let alpine_runtime = mk_alpine_ci_img "runtime"

  let alpine_monitoring = mk_alpine_ci_img "monitoring"

  let alpine_prebuild = mk_alpine_ci_img "prebuild"

  let alpine_build = mk_alpine_ci_img "build"

  let alpine_release_page = mk_alpine_ci_img "release-page"

  let alpine_test = mk_alpine_ci_img "test"

  let alpine_e2etest = mk_alpine_ci_img "e2etest"
end

(** A set of internally and externally built images.

    Use this module to register images built in the CI of
    [tezos/tezos] that are also used in the same pipelines. See
    {!Images_external} for external images.

    To make the distinction between internal and external images
    transparent to job definitions, this module also includes
    {!Images_external}.

    For documentation on the [CI] images, refer to [images/README.md]. *)

(* The external images are also available directly under [Images], so that
   job definitions need not know where an image comes from. *)
include Images_external

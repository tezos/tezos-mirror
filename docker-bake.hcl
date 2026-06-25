# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>

# Build definition for the Octez Docker distribution.
#
# A single buildx bake graph that builds the intermediate "build" image
# (compiled Octez binaries, from build.Dockerfile) ONCE and derives the three
# published variants (debug, bare, minimal, from Dockerfile) from it in the
# same graph.
#
# The variants' `builder` stage is `FROM ${BUILD_IMAGE}:${BUILD_IMAGE_VERSION}`.
# Instead of resolving that to a registry image (which would require pushing the
# multi-GB build image and pulling+extracting it back for every variant), the
# `contexts` block below wires that reference to the in-graph `build` target.
# The build stage is therefore computed once and consumed directly, with no
# registry round-trip for the intermediate image.
#
# All inputs come from the environment (one `variable` block per input).
# scripts/create_docker_image.sh sets them and invokes `docker buildx bake`.
# CI_* provenance labels are already exported by GitLab CI and picked up
# automatically.

variable "IMAGE_NAME" { default = "tezos-" } # base name; the minimal variant uses MINIMAL_IMAGE_NAME
variable "MINIMAL_IMAGE_NAME" { default = "tezos" }
variable "IMAGE_VERSION" { default = "latest" }
variable "CI_IMAGE_NAME" { default = "" }    # BASE_IMAGE
variable "CI_IMAGE_VERSION" { default = "" } # e.g. amd64--<hash>
variable "DOCKER_TARGET" { default = "without-evm-artifacts" }
variable "OCTEZ_EXECUTABLES" { default = "" }
variable "GIT_SHORTREF" { default = "" }
variable "GIT_DATETIME" { default = "" }
variable "GIT_VERSION" { default = "" }
variable "COMMIT_SHORT_SHA" { default = "" }
variable "RUST_TOOLCHAIN_IMAGE_NAME" { default = "" }
variable "RUST_TOOLCHAIN_IMAGE_TAG" { default = "master" }
variable "SCCACHE_GCS_BUCKET" { default = "" }

# CI provenance labels (empty in local dev; auto-populated in GitLab CI).
variable "CI_PIPELINE_ID" { default = "" }
variable "CI_PIPELINE_URL" { default = "" }
variable "CI_JOB_ID" { default = "" }
variable "CI_JOB_URL" { default = "" }
variable "CI_COMMIT_SHA" { default = "" }

# Intermediate image: the compiled Octez binaries. Referenced by the variants
# as a build context (target:build); never loaded or pushed on its own.
target "build" {
  context    = "."
  dockerfile = "build.Dockerfile"
  target     = DOCKER_TARGET
  args = {
    BASE_IMAGE                = CI_IMAGE_NAME
    BASE_IMAGE_VERSION        = "build:${CI_IMAGE_VERSION}"
    OCTEZ_EXECUTABLES         = OCTEZ_EXECUTABLES
    GIT_SHORTREF              = GIT_SHORTREF
    GIT_DATETIME              = GIT_DATETIME
    GIT_VERSION               = GIT_VERSION
    RUST_TOOLCHAIN_IMAGE_NAME = RUST_TOOLCHAIN_IMAGE_NAME
    RUST_TOOLCHAIN_IMAGE_TAG  = RUST_TOOLCHAIN_IMAGE_TAG
    SCCACHE_GCS_BUCKET        = SCCACHE_GCS_BUCKET
  }
  # build.Dockerfile has `RUN --network=host` (cargo CI mirror / sccache).
  # bake requires the entitlement to be declared per target; the CLI also
  # passes `--allow network.host` to confirm it non-interactively.
  entitlements = ["network.host"]
  # Intermediate only: keep it in the build cache, do not export an image.
  output = ["type=cacheonly"]
}

# Attributes shared by the three variant targets.
target "_variant" {
  context    = "."
  dockerfile = "Dockerfile"
  args = {
    BASE_IMAGE                 = CI_IMAGE_NAME
    BASE_IMAGE_VERSION         = "runtime:${CI_IMAGE_VERSION}"
    BASE_IMAGE_VERSION_NON_MIN = "build:${CI_IMAGE_VERSION}"
    BUILD_IMAGE                = "localhost/octez-build"
    BUILD_IMAGE_VERSION        = "local"
    COMMIT_SHORT_SHA           = COMMIT_SHORT_SHA
  }
  # Resolve Dockerfile's `FROM ${BUILD_IMAGE}:${BUILD_IMAGE_VERSION}` to the
  # in-graph `build` target rather than a registry pull. The context key must
  # be byte-identical to that FROM reference after ARG expansion, hence the
  # literal below MUST stay in sync with the BUILD_IMAGE/BUILD_IMAGE_VERSION
  # args above.
  #
  # The reference is a fixed `localhost/`-qualified name, deliberately
  # decoupled from the published IMAGE_NAME. The intermediate build image is
  # never pushed (target "build" is output=cacheonly), so its name is internal
  # to this graph. Using a registry-qualified host (`localhost`) makes the
  # familiar and canonical forms of the reference identical, so the named
  # context matches the FROM on every pipeline. A Docker Hub name (e.g.
  # docker.io/tezos/tezos-build:amd64_master, as produced on master_branch
  # pipelines) does NOT: buildx normalizes it to a differing familiar form,
  # the context lookup misses, and the FROM falls back to a registry pull of
  # the never-pushed image — which fails with "not found". See !22081.
  contexts = {
    "localhost/octez-build:local" = "target:build"
  }
  # The variant solve embeds the linked `build` target's LLB, which uses
  # host networking, so the entitlement must be granted here too.
  entitlements = ["network.host"]
  labels = {
    "com.tezos.build-pipeline-id"    = CI_PIPELINE_ID
    "com.tezos.build-pipeline-url"   = CI_PIPELINE_URL
    "com.tezos.build-job-id"         = CI_JOB_ID
    "com.tezos.build-job-url"        = CI_JOB_URL
    "com.tezos.build-tezos-revision" = CI_COMMIT_SHA
  }
  # Variants are loaded into the local image store. Pushing to the registry is
  # done downstream by scripts/ci/docker_push_all.sh (which also signs), so it
  # is intentionally not wired through bake here.
  output = ["type=docker"]
}

target "debug" {
  inherits = ["_variant"]
  target   = "debug"
  tags     = ["${IMAGE_NAME}debug:${IMAGE_VERSION}"]
}

target "bare" {
  inherits = ["_variant"]
  target   = "bare"
  tags     = ["${IMAGE_NAME}bare:${IMAGE_VERSION}"]
}

target "minimal" {
  inherits = ["_variant"]
  target   = "minimal"
  tags     = ["${MINIMAL_IMAGE_NAME}:${IMAGE_VERSION}"]
}

group "default" {
  targets = ["debug", "bare", "minimal"]
}

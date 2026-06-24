# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>

# Build definitions for the static "alpine-*" CI images, built with
# [docker buildx bake] from the repository root.
#
# The per-image Dockerfiles (images/ci/Dockerfile.*) build from the repository
# root and reference their inputs by real repo-root paths (no symlinks), so
# bake can build them directly.
#
# The inter-image dependencies (runtime -> monitoring -> prebuild -> build ->
# test, prebuild -> release-page, {monitoring,build} -> e2etest) are expressed
# as named build contexts ([contexts = { name = "target:name" }]), matching the
# [FROM <name>] stages in the Dockerfiles.

# ---------------------------------------------------------------------------
# Variables
# ---------------------------------------------------------------------------

# Image registry and tag. Set by the CI job that runs bake.
variable "REGISTRY" { default = "" }
variable "TAG" { default = "latest" }

# Versions (defaults kept in sync with scripts/version.sh).
variable "ALPINE_VERSION" { default = "3.20" }
variable "OPAM_VERSION" { default = "2.3.0" }
variable "OCAML_VERSION" { default = "5.3.0" }
variable "GCLOUD_VERSION" { default = "543.0.0" }

# npm registry configuration, set in GitLab CI/CD (used by e2etest).
variable "NPM_REGISTRY_DOMAIN" { default = "" }
variable "NPM_REGISTRY" { default = "" }

# Image labels, set by the CI job.
variable "CI_PIPELINE_ID" { default = "" }
variable "CI_PIPELINE_URL" { default = "" }
variable "CI_JOB_ID" { default = "" }
variable "CI_JOB_URL" { default = "" }
variable "CI_COMMIT_SHA" { default = "" }

# ---------------------------------------------------------------------------
# Common base (not built directly)
# ---------------------------------------------------------------------------

target "_common" {
  context = "."
  labels = {
    "com.tezos.build-pipeline-id"    = CI_PIPELINE_ID
    "com.tezos.build-pipeline-url"   = CI_PIPELINE_URL
    "com.tezos.build-job-id"         = CI_JOB_ID
    "com.tezos.build-job-url"        = CI_JOB_URL
    "com.tezos.build-tezos-revision" = CI_COMMIT_SHA
  }
}

# ---------------------------------------------------------------------------
# Targets
# ---------------------------------------------------------------------------

target "runtime" {
  inherits   = ["_common"]
  dockerfile = "images/ci/Dockerfile.runtime"
  tags       = ["${REGISTRY}/alpine-runtime:${TAG}"]
  args = {
    BUILD_IMAGE = "alpine:${ALPINE_VERSION}"
  }
}

target "monitoring" {
  inherits   = ["_common"]
  dockerfile = "images/ci/Dockerfile.monitoring"
  tags       = ["${REGISTRY}/alpine-monitoring:${TAG}"]
  contexts = {
    runtime = "target:runtime"
  }
  args = {
    GCLOUD_VERSION = "${GCLOUD_VERSION}"
  }
}

target "prebuild" {
  inherits   = ["_common"]
  dockerfile = "images/ci/Dockerfile.prebuild"
  tags       = ["${REGISTRY}/alpine-prebuild:${TAG}"]
  contexts = {
    monitoring = "target:monitoring"
  }
  args = {
    OPAM_VERSION  = "${OPAM_VERSION}"
    OCAML_VERSION = "${OCAML_VERSION}"
  }
}

target "build" {
  inherits   = ["_common"]
  dockerfile = "images/ci/Dockerfile.build"
  tags       = ["${REGISTRY}/alpine-build:${TAG}"]
  contexts = {
    prebuild = "target:prebuild"
  }
}

target "test" {
  inherits   = ["_common"]
  dockerfile = "images/ci/Dockerfile.test"
  tags       = ["${REGISTRY}/alpine-test:${TAG}"]
  contexts = {
    build = "target:build"
  }
}

target "release-page" {
  inherits   = ["_common"]
  dockerfile = "images/ci/Dockerfile.release-page"
  tags       = ["${REGISTRY}/alpine-release-page:${TAG}"]
  contexts = {
    prebuild = "target:prebuild"
  }
}

target "e2etest" {
  inherits   = ["_common"]
  dockerfile = "images/ci/Dockerfile.e2etest"
  tags       = ["${REGISTRY}/alpine-e2etest:${TAG}"]
  # The "npm ci" step mounts this secret to authenticate against the private
  # npm registry. /tmp/npm_token.txt is created by docker_initialize.sh
  # (-> docker_registry_auth.sh) in the build job's before_script.
  secret = ["id=npm_token,src=/tmp/npm_token.txt"]
  contexts = {
    monitoring = "target:monitoring"
    build      = "target:build"
  }
  args = {
    OCAML_VERSION       = "${OCAML_VERSION}"
    NPM_REGISTRY_DOMAIN = "${NPM_REGISTRY_DOMAIN}"
    NPM_REGISTRY        = "${NPM_REGISTRY}"
  }
}

# ---------------------------------------------------------------------------
# Groups
# ---------------------------------------------------------------------------

group "default" {
  targets = [
    "runtime", "monitoring", "prebuild", "build", "release-page", "test",
    "e2etest",
  ]
}

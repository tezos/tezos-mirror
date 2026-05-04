#!/bin/sh

##############################################################################
#                                                                            #
# SPDX-License-Identifier: MIT                                               #
# SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>       #
#                                                                            #
##############################################################################

# Container scanning for the scheduled security scans pipeline.
#
# Pulls a source image (from DockerHub or another registry), pushes it to
# the GCP Artifact Registry [image-scanning] repository, then delegates
# the actual scan and GitLab report generation to
# [container_scanning_gcp.sh].
#
# Required env vars (set by the scanning job in
# .gitlab/ci/pipelines/schedule_security_scans.yml):
#   FULL_IMAGE_NAME  - source image to scan
#   REPORT           - output report filename (relative to CI_PROJECT_DIR)

set -eu

GCP_SCANNING_REGISTRY="us-central1-docker.pkg.dev/nl-gitlab-runner/image-scanning"

scripts_dir="${CI_PROJECT_DIR}/scripts/ci"

# Authenticate to GCP using the protected service account when on a
# protected branch.
# shellcheck source=scripts/ci/gcp_auth.sh
. "${scripts_dir}/gcp_auth.sh"

echo "==> Authenticating Docker to GCP Artifact Registry"
gcloud auth configure-docker us-central1-docker.pkg.dev --quiet

# Derive a safe image tag from FULL_IMAGE_NAME:
#   tezos/tezos:latest               -> tezos-tezos-latest
#   registry.example.com/img:tag     -> registry.example.com-img-tag
IMAGE_TAG=$(echo "${FULL_IMAGE_NAME}" | sed 's|[/:]|-|g')
GCP_IMAGE="${GCP_SCANNING_REGISTRY}/${IMAGE_TAG}"

echo "==> Pulling source image: ${FULL_IMAGE_NAME}"
docker pull "${FULL_IMAGE_NAME}"

echo "==> Tagging for GCP Artifact Registry: ${GCP_IMAGE}"
docker tag "${FULL_IMAGE_NAME}" "${GCP_IMAGE}"

echo "==> Pushing to GCP Artifact Registry: ${GCP_IMAGE}"
docker push "${GCP_IMAGE}"

# Delegate scanning and report generation to the shared script.
# The security gate is disabled: scheduled scans publish findings to the
# vulnerability dashboard without failing the job.
export GCP_IMAGE
export SECURITY_GATE_DISABLED=true
"${scripts_dir}/container_scanning_gcp.sh" "${REPORT}"

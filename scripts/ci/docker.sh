#!/bin/sh

## Sourceable file with common variables for Octez Docker distribution.
##
## This file expands DOCKER_IMAGE_NAME (set by docker_image_names.sh
## via docker.env) into concrete image names for the debug, bare, and
## minimal variants. It is ONLY used by Octez distribution scripts:
##   docker_release.sh, docker_push_all.sh, docker_merge_manifests.sh,
##   docker_sign.sh, docker_verify_signature.sh,
##   docker_promote_to_latest.sh, docker_promote_to_version.sh
##
## CI image jobs, base image jobs, and other non-distribution jobs
## do NOT source this file.

## Docker image names

# shellcheck disable=SC2034
docker_build_image="${DOCKER_IMAGE_NAME}build"

# Shell parameter expansion: %? trims the last character of variable DOCKER_IMAGE_NAME
# shellcheck disable=SC2034
docker_images="${DOCKER_IMAGE_NAME}debug ${DOCKER_IMAGE_NAME}bare ${DOCKER_IMAGE_NAME%?}"

## Multi-stage docker images: target for each image (match docker_images order)

# shellcheck disable=SC2034
docker_targets='debug bare minimal'

## Multi-arch docker images

# shellcheck disable=SC2034
docker_architectures='amd64 arm64'

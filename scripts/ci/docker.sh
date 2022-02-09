#!/bin/sh

## Sourceable file with common variables for other scripts related to docker images

## Docker image names

# shellcheck disable=SC2034
docker_build_image="${DOCKER_IMAGE_NAME}build"

# shellcheck disable=SC2034
docker_images="${DOCKER_IMAGE_NAME}debug ${DOCKER_IMAGE_NAME}bare ${DOCKER_IMAGE_NAME%?}"

## Multi-stage docker images: target for each image (match docker_images order)

# shellcheck disable=SC2034
docker_targets='debug bare minimal'

## Multi-arch docker images

# shellcheck disable=SC2034
docker_architectures='amd64 arm64'

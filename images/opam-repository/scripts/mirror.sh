#!/bin/sh
set -eu

## Mirror Docker image from GitLab container to public GCP and private AWS ECR
## registries.

# shellcheck source=./scripts/docker.sh
. ./scripts/docker.sh

image_name="${1:-tezos/opam-repository}"
tag_suffix="${2}"
mirror_image_name="${3}"

set -x

for target in ${docker_images}; do
  # Architecture is empty ("") because these images are multi-arch.
  image_tag=$(docker_tag "$target" "" "${tag_suffix}")
  regctl image copy "${image_name}:${image_tag}" \
    "${mirror_image_name}:${image_tag}"
done

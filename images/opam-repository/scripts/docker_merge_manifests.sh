#!/bin/sh
set -eu

## Multi-arch docker images with a single tag
# shellcheck source=./scripts/docker.sh
. ./scripts/docker.sh

image_name="${1:-tezos/opam-repository}"
tag_suffix="${2:-}"

# Loop over images
for target in $docker_images; do
  echo "### Merging tags for docker image: ${target}"

  amd64_image_tag=$(docker_tag "$target" "amd64" "$tag_suffix")
  arm64_image_tag=$(docker_tag "$target" "arm64" "$tag_suffix")
  merged_image_tag=$(docker_tag "$target" "" "$tag_suffix")
  docker manifest create \
    "${image_name}":"${merged_image_tag}" \
    --amend "${image_name}":"${amd64_image_tag}" \
    --amend "${image_name}":"${arm64_image_tag}"

  docker manifest push "${image_name}:${merged_image_tag}"
done

#!/bin/sh
set -eu

## Push all Docker images to GitLab container registry

# shellcheck source=./scripts/docker.sh
. ./scripts/docker.sh

image_name="${1:-tezos/opam-repository}"
tag_suffix="${2:-}"
tag_extra="${3:-}"
target_arch="${4:-}"

for target in $docker_images; do
  image_tag="$(docker_tag "$target" "$target_arch" "$tag_suffix")"
  docker push "${image_name}:${image_tag}"
  if [ -n "$tag_extra" ]; then
    image_tag_extra="$(docker_tag "$target" "$target_arch" "$tag_extra")"
    docker push "${image_name}:${image_tag_extra}"
  fi
done

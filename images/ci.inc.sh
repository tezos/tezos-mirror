#!/bin/sh

## Sourceable file with common variables and functions for scripts
## related to the CI images

export valid_layer_targets='runtime monitoring prebuild build test e2etest release-page'

# Gives the Docker tag for a images/ci image based on:
#  Argument 1: target architecture
#  Optional argument 2: tag suffix, typically an input hash or a sanitized branch name
# echoes "${target_architecture}(--${tag_suffix})"
docker_tag() {
  f_target_arch=${1:?"[docker_tag] mandatory first argument is not set"}
  f_suffix="${2:-}"

  f_image_tag="${f_target_arch}"
  if [ -n "${f_suffix}" ]; then
    f_image_tag="${f_image_tag}--${f_suffix}"
  fi
  echo "$f_image_tag"
}

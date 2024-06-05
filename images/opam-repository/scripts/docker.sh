#!/bin/sh

## Sourceable file with common variables for other scripts related to docker images

export docker_images='runtime-dependencies runtime-prebuild-dependencies runtime-build-dependencies runtime-build-test-dependencies runtime-e2etest-dependencies'

export docker_architectures='amd64 arm64'

docker_cache_disabled() {
  [ "${DOCKER_NO_CACHE:-}" = "1" ] ||
    echo "${CI_MERGE_REQUEST_LABELS:-}" |
    grep -q '(?:^|[,])ci--no-cache(?:$|[,])'
}

docker_cache_disabled_pp() {
  if docker_cache_disabled; then
    echo "docker cache: disabled"
  else
    echo "docker cache: enabled"
  fi
}

# Gives the Docker tag for an image based on:
#  Argument 1: its target,
#  Optional argument 2: target architecture
#  Optional argument 3: tag suffix, typically a commit hash
# echoes "${target}(--${target_architecture})(--${tag_suffix})"
docker_tag() {
  f_target="$1"
  f_target_arch="${2:-}"
  f_suffix="${3:-}"

  f_image_tag="$f_target"
  if [ -n "${f_target_arch}" ]; then
    f_image_tag="${f_image_tag}--${f_target_arch}"
  fi
  if [ -n "${f_suffix}" ]; then
    f_image_tag="${f_image_tag}--${f_suffix}"
  fi

  echo "$f_image_tag"
}

# Build with the '--no-cache' Docker build flag if DOCKER_NO_CACHE is set
# to 1 or if the comma-separated list CI_MERGE_REQUEST_LABELS contains
# 'ci--no-cache'.
docker_build() {
  if docker_cache_disabled; then
    docker build --network host --no-cache "$@"
  else
    docker build --network host "$@"
  fi
}

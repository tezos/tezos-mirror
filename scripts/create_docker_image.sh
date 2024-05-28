#!/bin/sh
# Script used to create all docker images
# Invocation: source ./scripts/version.sh; ./scripts/create_docker_image.sh
#
# Notes on the parameters:
# - commit_short_sha: is used to tag, can be chosen arbitrarily
# - rust_toolchain_image_tag: tag "master" doesn't work (FIXME?)
#   Can be obtained using following line:
#   git ls-files -s -- $(cat images/rust-toolchain/inputs) | git hash-object --stdin
#   if the result does not correspond to an image in registry, the
#   rust-toolchain needs to be rebuilt. See ./images/README.md and
#   ./images/create_rust_toolchain_image.sh for more information.

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

# shellcheck source=scripts/version.sh
. "$script_dir"/version.sh

# usage and help
usage() {
  cat << EOF
Usage:  $(basename "$0") [-h|--help]
  [--image-name <IMAGE_NAME> ]
  [--image-version <IMAGE_TAG> ]
  [--build-deps-image-name <IMAGE_NAME> ]
  [--build-deps-image-version <IMAGE_TAG> ]
  [--executables <EXECUTABLES> ]
  [--commit-short-sha <COMMIT_SHA> ]
  [--docker-target <TARGET> ]
  [--rust-toolchain-image <IMAGE_NAME> ]
  [--rust-toolchain-image-tag <IMAGE_TAG> ]
  [--commit-datetime <DATETIME> ]
  [--commit-tag <COMMIT_TAG> ]
EOF
}

# defaults
image_name="tezos-"
image_version="latest"
build_deps_image_name="registry.gitlab.com/tezos/opam-repository"
build_deps_image_version=$opam_repository_tag
executables=$(cat script-inputs/released-executables)
commit_short_sha=$(git rev-parse --short HEAD)
docker_target="without-evm-artifacts"
rust_toolchain_image="registry.gitlab.com/tezos/tezos/rust-toolchain"
# FIXME: "master" doesn't work as a tag
rust_toolchain_image_tag="latest"
commit_datetime=$(git show -s --pretty=format:%ci HEAD)
commit_tag=$(git describe --tags --always)

print_parameters() {
  echo "Current values:"
  echo "---------------"
  echo "image_name: $image_name"
  echo "image_version: $image_version"
  echo "build_deps_image_name: $build_deps_image_name"
  echo "build_deps_image_version: $build_deps_image_version"
  echo "commit_short_sha: $commit_short_sha"
  echo "docker_target: $docker_target"
  echo "rust_toolchain_image: $rust_toolchain_image"
  echo "rust_toolchain_image_tag: $rust_toolchain_image_tag"
  echo "commit_datetime: $commit_datetime"
  echo "commit_tag: $commit_tag"
  echo "executables:"
  echo "$executables"
}

print_help() {
  # display help
  usage
  echo ""
  echo "Creates the Octez docker images." 1>&2
  echo ""
  print_parameters
}

options=$(getopt -o h \
  -l help,image-name:,image-version:,build-deps-image-name:,build-deps-image-version:,executables:,commit-short-sha:,docker-target:,rust-toolchain-image:,rust-toolchain-image-tag:,commit-datetime:,commit-tag: -- "$@")
eval set - "$options"
# parse options and flags
while true; do
  case "$1" in
  --image-name)
    shift
    image_name="$1"
    ;;
  --image-version)
    shift
    image_version="$1"
    ;;
  --build-deps-image-name)
    shift
    build_deps_image_name="$1"
    ;;
  --build-deps-image-version)
    shift
    build_deps_image_version="$1"
    ;;
  --executables)
    shift
    executables="$1"
    ;;
  --commit-short-sha)
    shift
    commit_short_sha="$1"
    ;;
  --docker-target)
    shift
    docker_target="$1"
    ;;
  --rust-toolchain-image)
    shift
    rust_toolchain_image="$1"
    ;;
  --rust-toolchain-image-tag)
    shift
    rust_toolchain_image_tag="$1"
    ;;
  --commit-datetime)
    shift
    commit_datetime="$1"
    ;;
  --commit-tag)
    shift
    commit_tag="$1"
    ;;
  -h | --help)
    print_help
    exit 1
    ;;
  --)
    # This artificial option is added by 'getopt' and is required to recognize
    # the end arguments.
    shift
    break
    ;;
  :) # If expected argument omitted:
    usage
    exit 1
    ;;
  *) # If unknown (any other) option:
    usage
    exit 1
    ;;
  esac
  shift
done

build_image_name="${image_name}build"

echo "Executables to include in Docker images:"
for executable in $executables; do
  echo "- $executable"
done

echo "### Building tezos..."

docker build \
  --network host \
  -t "$build_image_name:$image_version" \
  -f build.Dockerfile \
  --target "$docker_target" \
  --cache-from "$build_image_name:$image_version" \
  --build-arg "BASE_IMAGE=$build_deps_image_name" \
  --build-arg "BASE_IMAGE_VERSION=runtime-build-dependencies--$build_deps_image_version" \
  --build-arg "OCTEZ_EXECUTABLES=${executables}" \
  --build-arg "GIT_SHORTREF=${commit_short_sha}" \
  --build-arg "GIT_DATETIME=${commit_datetime}" \
  --build-arg "GIT_VERSION=${commit_tag}" \
  --build-arg "RUST_TOOLCHAIN_IMAGE=$rust_toolchain_image" \
  --build-arg "RUST_TOOLCHAIN_IMAGE_TAG=$rust_toolchain_image_tag" \
  "$src_dir"

echo "### Successfully built docker image: $build_image_name:$image_version"

docker build \
  --network host \
  -t "${image_name}debug:$image_version" \
  --build-arg "BASE_IMAGE=$build_deps_image_name" \
  --build-arg "BASE_IMAGE_VERSION=runtime-dependencies--$build_deps_image_version" \
  --build-arg "BASE_IMAGE_VERSION_NON_MIN=runtime-build-dependencies--$build_deps_image_version" \
  --build-arg "BUILD_IMAGE=${build_image_name}" \
  --build-arg "BUILD_IMAGE_VERSION=${image_version}" \
  --build-arg "COMMIT_SHORT_SHA=${commit_short_sha}" \
  --target=debug \
  "$src_dir"

echo "### Successfully built docker image: ${image_name}debug:$image_version"

docker build \
  --network host \
  -t "${image_name}bare:$image_version" \
  --build-arg "BASE_IMAGE=$build_deps_image_name" \
  --build-arg "BASE_IMAGE_VERSION=runtime-dependencies--$build_deps_image_version" \
  --build-arg "BASE_IMAGE_VERSION_NON_MIN=runtime-build-dependencies--$build_deps_image_version" \
  --build-arg "BUILD_IMAGE=${build_image_name}" \
  --build-arg "BUILD_IMAGE_VERSION=${image_version}" \
  --build-arg "COMMIT_SHORT_SHA=${commit_short_sha}" \
  --target=bare \
  "$src_dir"

echo "### Successfully built docker image: ${image_name}bare:$image_version"

docker build \
  --network host \
  -t "${image_name%?}:$image_version" \
  --build-arg "BASE_IMAGE=$build_deps_image_name" \
  --build-arg "BASE_IMAGE_VERSION=runtime-dependencies--$build_deps_image_version" \
  --build-arg "BASE_IMAGE_VERSION_NON_MIN=runtime-build-dependencies--$build_deps_image_version" \
  --build-arg "BUILD_IMAGE=${build_image_name}" \
  --build-arg "BUILD_IMAGE_VERSION=${image_version}" \
  --build-arg "COMMIT_SHORT_SHA=${commit_short_sha}" \
  --target=minimal \
  "$src_dir"

echo "### Successfully built docker image: ${image_name%?}:$image_version"

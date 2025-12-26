#!/bin/sh

set -eu

usage() {
  cat << EOT
Usage: $0 [image_base [image_tag]] [docker build args...]

Creates the 'client-libs-dependencies' image from
image/client-libs-dependencies/Dockerfile.  The built image is tagged
with image_base:image_tag. If omitted, they default to
'${GCP_CI_REGISTRY}/tezos/tezos/client-libs-dependencies' respectively
'latest'.  Remaining arguments, if any, are passed on to 'docker
build'.
EOT
  exit 1
}

images_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
script_dir="$(dirname "$images_dir")/scripts"

#shellcheck source=scripts/version.sh
. "$script_dir"/version.sh

if [ "${1:-}" = "--help" ]; then
  usage
fi

if [ $# -gt 0 ]; then
  image_base="$1"
  shift
else
  image_base="${GCP_CI_REGISTRY}/tezos/tezos/client-lib-dependencies"
fi
if [ $# -gt 0 ]; then
  image_tag="$1"
  shift
else
  image_tag="latest"
fi
image_name="${image_base}:${image_tag}"

# Resolve symlinks in '${images_dir}/client-libs-dependencies' by
# passing it as a tar:ing to Docker's build context.
set -x
cd "${images_dir}/client-libs-dependencies" &&
  tar -cvh . |
  docker build \
    --build-arg BUILD_IMAGE=alpine:${alpine_version} \
    --build-arg SBT_VERSION=1.9.7 \
    --build-arg KSC_TAG=11b8e7f1b1849aaebcae32836b393d12be78b87f \
    --build-arg KSC_VERSION=0.11-SNAPSHOT \
    --build-arg recommended_node_version="${recommended_node_version}" \
    -t "${image_name}" \
    "$@" \
    -

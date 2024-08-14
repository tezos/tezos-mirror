#!/bin/sh

set -eu

usage() {
  cat << EOT
Usage: $0 [image_base [image_tag [docker build args...]]]

Creates the Rust toolchain from image/rust-toolchain/Dockerfile. The
built image is tagged with image_base:image_tag. If these arguments are omitted, then
the image is tagged
'us-central1-docker.pkg.dev/nl-gitlab-runner/protected-registry/tezos/tezos/rust-toolchain:master'.
Remaining arguments, if any, are passed on to 'docker build'.
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
fi
if [ $# -gt 0 ]; then
  image_tag="$1"
  shift
fi
image_base=${image_base:-${GCP_REGISTRY}/tezos/tezos/rust-toolchain}
image_tag=${image_tag:-master}
image_name="${image_base}:${image_tag}"

images_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$images_dir")"
cd "$src_dir"

set -x
docker build images/rust-toolchain -t "${image_name}" "$@"

#!/bin/sh

set -eu

usage() {
  cat << EOT
Usage: $0 [image_base [image_tag [docker build args...]]]

Creates the image from image/image_base/Dockerfile. The
built image is tagged with image_base:image_tag. If these arguments are omitted, then
the image is tagged
'us-central1-docker.pkg.dev/nl-gitlab-runner/protected-registry/tezos/tezos/image_base:master'.
Remaining arguments, if any, are passed on to 'docker build'.
EOT
  exit 1
}

images_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
script_dir="$(dirname "$images_dir")/scripts"

#shellcheck source=scripts/version.sh
. "$script_dir"/version.sh
#shellcheck source=scripts/ci/docker_registry.inc.sh
. "$script_dir"/ci/docker_registry.inc.sh

if [ "${1:-}" = "--help" ]; then
  usage
fi

if [ $# -gt 0 ]; then
  name="$1"
  shift
fi
if [ $# -gt 0 ]; then
  image_base="$1"
  shift
fi
if [ $# -gt 0 ]; then
  image_tag="$1"
  shift
fi
image_base=${image_base:-${GCP_REGISTRY}/tezos/tezos/${image_base}}
image_tag=${image_tag:-master}
image_name="${image_base}:${image_tag}"

# Apply the correct registry based on image type
image_name=$(ensure_correct_image_registry "$image_name")

images_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$images_dir")"
cd "$src_dir"

set -x
cd "images/" &&
  tar -cvh . |
  docker buildx build -f "${name}/Dockerfile" -t "${image_name}" "$@" -

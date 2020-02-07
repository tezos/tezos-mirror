#! /bin/sh

set -e

ci_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
script_dir="$(dirname "$ci_dir")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. "$script_dir"/version.sh

image_name="${1:-tezos_build}"
image_version="${2:-latest}"
base_image="${3-${image_name}_deps}"
base_image_version="${4:-latest}"
commit_short_sha="${5:-unknown}"

echo
echo "### Building tezos..."
echo

docker build \
  -t "$image_name:$image_version" \
  -f build.Dockerfile \
  --cache-from "$image_name:$image_version" \
  --build-arg "BASE_IMAGE=$base_image" \
  --build-arg "BASE_IMAGE_VERSION=${base_image_version}" \
  --build-arg "BUILD_IMAGE_VERSION=${base_image_version}" \
  --build-arg "GIT_SHORTREF=${commit_short_sha}" \
  "$src_dir"

echo
echo "### Successfully build docker image: $image_name:$image_version"
echo

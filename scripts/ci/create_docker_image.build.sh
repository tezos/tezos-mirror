#! /bin/sh

set -e

ci_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
script_dir="$(dirname "$ci_dir")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. "$script_dir"/version.sh

image_name="${1:-tezos_build}"
image_version="${2:-latest}"
base_image="${3:-registry.gitlab.com/tezos/opam-repository}"
base_image_version="${4:-runtime-build-dependencies--${opam_repository_tag}}"
commit_short_sha="${5:-$(git rev-parse --short HEAD)}"
commit_datetime="${6:-$(git show -s --pretty=format:%ci HEAD)}"

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
  --build-arg "GIT_DATETIME=${commit_datetime}" \
  "$src_dir"

echo
echo "### Successfully built docker image: $image_name:$image_version"
echo

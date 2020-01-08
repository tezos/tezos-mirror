#! /bin/sh

set -ex

ci_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
script_dir="$(dirname "$ci_dir")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. "$script_dir"/version.sh

image_name="${1:-tezos}"
image_version="${2:-latest}"
build_image="${3:-registry.gitlab.com/tezos/opam-repository}"
base_image="${4-registry.gitlab.com/tezos/opam-repository}"
base_version="${5-minimal--${opam_repository_tag}}"

echo
echo "### Building minimal docker images..."
echo

docker build \
  -t "$image_name-debug:$image_version" \
  --build-arg "BASE_IMAGE=$base_image" \
  --build-arg "BASE_IMAGE_VERSION=$base_version" \
  --build-arg "BASE_IMAGE_VERSION_NON_MIN=$opam_repository_tag" \
  --build-arg="BUILD_IMAGE=${build_image}" \
  --build-arg="BUILD_IMAGE_VERSION=${image_version}" \
  --target=debug \
  "$src_dir"

echo
echo "### Successfully build docker image: $image_name-debug:$image_version"
echo

docker build \
  -t "$image_name-bare:$image_version" \
  --build-arg "BASE_IMAGE=$base_image" \
  --build-arg "BASE_IMAGE_VERSION=$base_version" \
  --build-arg="BUILD_IMAGE=${build_image}" \
  --build-arg="BUILD_IMAGE_VERSION=${image_version}" \
  --build-arg "BASE_IMAGE_VERSION_NON_MIN=$opam_repository_tag" \
  --target=bare \
  "$src_dir"


echo
echo "### Successfully build docker image: $image_name-bare:$image_version"
echo

docker build \
  -t "$image_name:$image_version" \
  --build-arg "BASE_IMAGE=$base_image" \
  --build-arg "BASE_IMAGE_VERSION=$base_version" \
  --build-arg="BUILD_IMAGE=${build_image}" \
  --build-arg="BUILD_IMAGE_VERSION=${image_version}" \
  --build-arg "BASE_IMAGE_VERSION_NON_MIN=$opam_repository_tag" \
  "$src_dir"

echo
echo "### Successfully build docker image: $image_name:$image_version"
echo

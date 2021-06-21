#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

. "$script_dir"/version.sh

image_name="${1:-tezos-}"
image_version="${2:-latest}"
build_deps_image_name=${3:-registry.gitlab.com/tezos/opam-repository}
build_deps_image_version=${4:-$opam_repository_tag}
commit_short_sha="${5:-$(git rev-parse --short HEAD)}"

build_image_name="${image_name}build"

"$script_dir"/ci/create_docker_image.build.sh \
             "$build_image_name" "$image_version" \
             "$build_deps_image_name" "runtime-build-dependencies--$build_deps_image_version" "$commit_short_sha"

"$script_dir"/ci/create_docker_image.minimal.sh \
             "$image_name" "$image_version" \
             "$build_image_name" "$build_deps_image_name" \
             "runtime-dependencies--$build_deps_image_version" "runtime-build-dependencies--$build_deps_image_version" \
             "$commit_short_sha"

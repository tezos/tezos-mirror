#!/bin/sh

set -e

devtools_docker_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
echo "devtools docker dir: $(pwd - P)"

dockerfile="$devtools_docker_dir"/build.Dockerfile
dockerfile_aux="$devtools_docker_dir"/Dockerfile

testnet_experiments_dir="$(cd "$(dirname "$devtools_docker_dir")" && echo "$(pwd -P)/")"

devtools_dir="$(cd "$(dirname "$testnet_experiments_dir")" && echo "$(pwd -P)/")"

src_dir="$(cd "$(dirname "$devtools_dir")" && echo "$(pwd -P)/")"

script_dir="$src_dir"scripts

cd "$src_dir"

# shellcheck disable=SC1091
. "$script_dir"/version.sh

image_name="${1:-tezos-}"
image_version="${2:-latest}"
build_deps_image_name=${3:-registry.gitlab.com/tezos/opam-repository}
build_deps_image_version=${4:-$opam_repository_tag}
executables=${5:-$(cat "$devtools_docker_dir"/executables)}
commit_short_sha="${6:-$(git rev-parse --short HEAD)}"
docker_target="${7:-without-evm-artifacts}"
rust_toolchain_image="$8"
rust_toolchain_image_version="${9:-$rust_toolchain_image_version}"
commit_datetime="${10:-$(git show -s --pretty=format:%ci HEAD)}"
commit_tag="${11:-$(git describe --tags --always)}"

build_image_name="${image_name}build"

echo "Executables to include in Docker images:"
for executable in $executables; do
  echo "- $executable"
done

echo "### Building tezos..."

docker build \
  -t "$build_image_name:$image_version" \
  -f "$dockerfile" \
  --target "$docker_target" \
  --cache-from "$build_image_name:$image_version" \
  --build-arg "BASE_IMAGE=$build_deps_image_name" \
  --build-arg "BASE_IMAGE_VERSION=runtime-build-dependencies--$build_deps_image_version" \
  --build-arg "OCTEZ_EXECUTABLES=${executables}" \
  --build-arg "GIT_SHORTREF=${commit_short_sha}" \
  --build-arg "GIT_DATETIME=${commit_datetime}" \
  --build-arg "GIT_VERSION=${commit_tag}" \
  --build-arg "RUST_TOOLCHAIN_IMAGE=$rust_toolchain_image" \
  --build-arg "RUST_TOOLCHAIN_IMAGE_VERSION=$rust_toolchain_image_version" \
  "$src_dir"

echo "### Successfully built docker image: $build_image_name:$image_version"

# Create temporary file with git history to import in the final container
tmp_dir="$devtools_docker_dir"tmp
git_history_file="$tmp_dir"/git_history
mkdir "$tmp_dir"
git log --oneline --decorate "$(git merge-base master HEAD)"~1..HEAD > "$git_history_file"

docker build \
  -t "${image_name}bare:$image_version" \
  -f "$dockerfile_aux" \
  --build-arg "BASE_IMAGE=$build_deps_image_name" \
  --build-arg "BASE_IMAGE_VERSION=runtime-dependencies--$build_deps_image_version" \
  --build-arg "BASE_IMAGE_VERSION_NON_MIN=runtime-build-dependencies--$build_deps_image_version" \
  --build-arg "BUILD_IMAGE=${build_image_name}" \
  --build-arg "BUILD_IMAGE_VERSION=${image_version}" \
  --build-arg "COMMIT_SHORT_SHA=${commit_short_sha}" \
  --build-arg "GIT_HISTORY_FILE=${git_history_file#"$src_dir"}" \
  --target=bare \
  "$src_dir"

# Remove temporary file with git history
rm -rf "$tmp_dir"

echo "### Successfully built docker image: ${image_name}bare:$image_version"

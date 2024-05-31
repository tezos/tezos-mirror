#!/bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

# shellcheck source=scripts/version.sh
. "$script_dir"/version.sh

# defaults
image_name="tezos-"
image_version="latest"
build_deps_image_name="registry.gitlab.com/tezos/opam-repository"
build_deps_image_version=$opam_repository_tag
executables=$(cat script-inputs/released-executables)
commit_short_sha=$(git rev-parse --short HEAD)
variants="debug bare minimal"
docker_target="without-evm-artifacts"
rust_toolchain_image_name="us-central1-docker.pkg.dev/nl-gitlab-runner/protected-registry/tezos/tezos/rust-toolchain"
rust_toolchain_image_tag="master"
commit_datetime=$(git show -s --pretty=format:%ci HEAD)
commit_tag=$(git describe --tags --always)

# usage and help
usage() {
  cat << EOF
Usage:  $(basename "$0") [-h|--help]
  [--image-name <IMAGE_NAME> ]
  [--image-version <IMAGE_TAG> ]
  [--build-deps-image-name <IMAGE_NAME> ]
  [--build-deps-image-version <IMAGE_TAG> ]
  [--variants VARIANTS]
  [--docker-target <TARGET> ]
  [--rust-toolchain-image-name <IMAGE_NAME> ]
  [--rust-toolchain-image-tag <IMAGE_TAG> ]
  [--executables <EXECUTABLES> ]
  [--commit-short-sha <COMMIT_SHA> ]
  [--commit-datetime <DATETIME> ]
  [--commit-tag <COMMIT_TAG> ]

DESCRIPTION
    Builds the Octez Docker distribution.

    By default, the distribution is built in three VARIANTS, built under the names:
     - IMAGE_NAME-debug:IMAGE_TAG    (debug variant)
     - IMAGE_NAME-bare:IMAGE_TAG     (bare variant)
     - IMAGE_NAME:IMAGE_TAG          (minimal variant)
    For the default value of IMAGE_NAME, IMAGE_TAG and the other
    parameters, see below. The difference between the three variants
    of the distribution are documented at
    https://hub.docker.com/r/tezos/tezos.

    The build uses following images from the build-deps suite:
     - BUILD_DEPS_IMAGE_NAME/runtime-dependencies:BUILD_DEPS_IMAGE_TAG
     - BUILD_DEPS_IMAGE_NAME/runtime-build-dependencies:BUILD_DEPS_IMAGE_TAG
    The build-deps images are built in the
    https://gitlab.com/tezos/opam-repository project,
    and by default, BUILD_DEPS_IMAGE_NAME refers to the images from
    this repository and BUILD_DEPS_IMAGE_TAG equals 'opam_repository_tag'
    from 'scripts/version.sh'.

    If TARGET is 'with-evm-artifacts' then EVM artifacts are
    included. The image rust-toolchain is used to build these
    artifacts, and is pulled from
    RUST_TOOLCHAIN_IMAGE_NAME:RUST_TOOLCHAIN_IMAGE_TAG. By default,
    RUST_TOOLCHAIN_IMAGE_NAME:RUST_TOOLCHAIN_IMAGE_TAG points to a
    rust-toolchain image built from the latest commit on master. To
    rebuild the rust-toolchain image locally, see ./images/README.md
    and ./images/create_rust_toolchain_image.sh.

    The built distribution includes the set of executables defined by
    EXECUTABLES: for a set of valid values, see
    script-inputs/{released,experimental,dev}-executables. By default,
    the distribution contains the released executables.

    The version reported by the built Octez binaries are controlled
    through the arguments COMMIT_SHA, COMMIT_DATETIME, and COMMIT_TAG.

OPTIONS
    Image naming
        --image-name IMAGE_NAME
            Base for the name of the built images.

        --image-version IMAGE_TAG
            Tag of built images.

    Base image location
        --build-deps-image-name BUILD_DEPS_IMAGE_NAME
            Name of the build-deps image.

        --build-deps-image-version BUILD_DEPS_IMAGE_TAG
            Version of the build-deps image.

        --rust-toolchain-image-name RUST_TOOLCHAIN_IMAGE_NAME
            Name of the rust-toolchain image.

        --rust-toolchain-image-tag RUST_TOOLCHAIN_IMAGE_TAG
            Tag of the rust-toolchain image.

    Image contents
        --executables EXECUTABLES
            Set of executables to include.

        --variants VARIANTS
            Distribution variants to build. A space-separated list of
            values from: "debug", "bare" and "minimal". If empty
            (i.e. '--variants ""'), then only the "build image" of
            'build.Dockerfile' is built. Default: "debug bare
            minimal". The minimal variant is tagged IMAGE_NAME:IMAGE_TAG
            whereas the others are tagged IMAGE_NAME-VARIANT:IMAGE_TAG.

        --docker-target TARGET
            'without-evm-artifacts' (default) or 'with-evm-artifacts'.

    Image metadata
        --commit-short-sha COMMIT_SHA
            Git commit short SHA for the Octez version string.
        --commit-datetime COMMIT_DATETIME
            Git date time for the Octez version string.
        --commit-tag COMMIT_TAG
            Git tags for the Octez version string.

CURRENT VALUES
    IMAGE_NAME: $image_name
    IMAGE_VERSION: $image_version
    BUILD_DEPS_IMAGE_NAME: $build_deps_image_name
    BUILD_DEPS_IMAGE_VERSION: $build_deps_image_version
    VARIANTS: $variants
    DOCKER_TARGET: $docker_target
    RUST_TOOLCHAIN_IMAGE_NAME: $rust_toolchain_image_name
    RUST_TOOLCHAIN_IMAGE_TAG: $rust_toolchain_image_tag
    EXECUTABLES: $(echo "$executables" | tr "\n" " ")
    COMMIT_SHORT_SHA: $commit_short_sha
    COMMIT_DATETIME: $commit_datetime
    COMMIT_TAG: $commit_tag

SEE ALSO
    For more information, see 'images/README.md' and
    'docs/introduction/howtoget.rst'.
EOF
}

options=$(getopt -o h \
  -l help,image-name:,image-version:,build-deps-image-name:,build-deps-image-version:,executables:,commit-short-sha:,variants:,docker-target:,rust-toolchain-image-name:,rust-toolchain-image-tag:,commit-datetime:,commit-tag: -- "$@")
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
  --variants)
    shift
    variants="$1"
    for variant in $variants; do
      case "$variant" in
      debug | minimal | bare) ;;
      *)
        echo "Invalid variant '$variant'. Should be one of 'debug', 'minimal', or 'bare'. See --help."
        exit 1
        ;;
      esac
    done

    ;;
  --docker-target)
    shift
    docker_target="$1"
    ;;
  --rust-toolchain-image-name)
    shift
    rust_toolchain_image_name="$1"
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
    usage
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
  --build-arg "RUST_TOOLCHAIN_IMAGE_NAME=$rust_toolchain_image_name" \
  --build-arg "RUST_TOOLCHAIN_IMAGE_TAG=$rust_toolchain_image_tag" \
  "$src_dir"

echo "### Successfully built docker image: $build_image_name:$image_version"

for variant in $variants; do
  case "$variant" in
  debug)
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
    ;;

  bare)
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
    ;;

  minimal)
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
    ;;
  esac
done

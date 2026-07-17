#!/bin/sh

set -ex

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"
cd "$src_dir"

# shellcheck source=scripts/version.sh
. "$script_dir"/version.sh

# defaults
image_name="tezos-"
image_version="latest"
# shellcheck disable=SC2154
arch=${ARCH:-amd64}
# Use the version of the CI images that corresponds to the state of the repo.
# The image will be fetched from 'ci_image_name' from 'scripts/version.sh'.
ci_image_version=${arch}--$(./images/image_tag.sh images/ci)
executables=$(cat script-inputs/released-executables)
commit_short_sha=$(git rev-parse --short HEAD)
variants="debug bare minimal"
docker_target="without-evm-artifacts"
# Full image reference (name:tag) for the rust-toolchain image (L2 builder).
# Required only for --docker-target with-evm-artifacts (the CI distribution
# jobs pass it via --rust-toolchain-image); unused for other targets.
rust_toolchain_image=""
commit_datetime=$(git show -s --pretty=format:%ci HEAD)
commit_tag=$(git describe --tags --always)
sccache_bucket=""
# Full image references (name:tag) for the runtime and build-dependencies images
# the distribution builds FROM. Default (after option parsing) to the images
# matching the local checkout (via 'ci_image_name'/version.sh); overridable via
# --runtime-image / --build-deps-image (the distribution jobs pass them
# explicitly).
runtime_image=""
build_deps_image=""

# usage and help
usage() {
  cat << EOF
Usage:  $(basename "$0") [-h|--help]
  [--image-name <IMAGE_NAME> ]
  [--image-version <IMAGE_TAG> ]
  [--runtime-image <IMAGE> ]
  [--build-deps-image <IMAGE> ]
  [--variants VARIANTS]
  [--docker-target <TARGET> ]
  [--rust-toolchain-image <IMAGE> ]
  [--executables <EXECUTABLES> ]
  [--commit-short-sha <COMMIT_SHA> ]
  [--commit-datetime <DATETIME> ]
  [--commit-tag <COMMIT_TAG> ]
  [--sccache-bucket <BUCKET> ]

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

    The build starts FROM a runtime image (base of the variants) and a
    build-dependencies image (build environment). By default these are the
    images/ci runtime and build images matching the local checkout, resolved
    via scripts/version.sh and images/image_tag.sh; they can be overridden with
    --runtime-image and --build-deps-image.

    If TARGET is 'with-evm-artifacts' then EVM artifacts are
    included. The rust-toolchain image is used to build these
    artifacts, and is pulled from RUST_TOOLCHAIN_IMAGE, which must then
    be provided via --rust-toolchain-image. See ./images/README.md for
    how the base images are built.

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
        --runtime-image IMAGE
            Full reference (name:tag) of the runtime image the variants are
            built FROM. Default: the images/ci runtime image matching the
            local checkout.

        --build-deps-image IMAGE
            Full reference (name:tag) of the build-dependencies image (build
            environment, also used by the stripper stage). Default: the
            images/ci build image matching the local checkout.

        --rust-toolchain-image RUST_TOOLCHAIN_IMAGE
            Full reference (name:tag) of the rust-toolchain image (L2
            builder), used to build the EVM artifacts. Required for
            --docker-target with-evm-artifacts.

    Image contents
        --executables EXECUTABLES
            Set of executables to include.

        --variants VARIANTS
            Distribution variants to build. A space-separated list of
            values from: "debug", "bare" and "minimal". If empty
            (i.e. '--variants ""'), then no variant image is produced:
            only the intermediate build stage of 'build.Dockerfile' is
            computed into the build cache (it is exported as
            'type=cacheonly', so nothing is loaded into the local image
            store). Default: "debug bare minimal". The minimal variant
            is tagged IMAGE_NAME:IMAGE_TAG whereas the others are tagged
            IMAGE_NAME-VARIANT:IMAGE_TAG.

        --docker-target TARGET
            'without-evm-artifacts' (default) or 'with-evm-artifacts'.

    Image metadata
        --commit-short-sha COMMIT_SHA
            Git commit short SHA for the Octez version string.
        --commit-datetime COMMIT_DATETIME
            Git date time for the Octez version string.
        --commit-tag COMMIT_TAG
            Git tags for the Octez version string.

    Compilation cache
        --sccache-bucket BUCKET
            GCS bucket for sccache Rust compilation caching. When set,
            RUSTC_WRAPPER=sccache is activated in build.Dockerfile,
            caching Rust artifacts in GCS across builds.

CURRENT VALUES
    IMAGE_NAME: $image_name
    IMAGE_VERSION: $image_version
    RUNTIME_IMAGE (default): $ci_image_name/runtime:$ci_image_version
    BUILD_DEPS_IMAGE (default): $ci_image_name/build:$ci_image_version
    VARIANTS: $variants
    DOCKER_TARGET: $docker_target
    RUST_TOOLCHAIN_IMAGE: $rust_toolchain_image
    EXECUTABLES: $(echo "$executables" | tr "\n" " ")
    COMMIT_SHORT_SHA: $commit_short_sha
    COMMIT_DATETIME: $commit_datetime
    COMMIT_TAG: $commit_tag
    SCCACHE_BUCKET: $sccache_bucket

SEE ALSO
    For more information, see 'images/README.md' and
    'docs/introduction/howtoget.rst'.
EOF
}

options=$(getopt -o h \
  -l help,image-name:,image-version:,runtime-image:,build-deps-image:,executables:,commit-short-sha:,variants:,docker-target:,rust-toolchain-image:,commit-datetime:,commit-tag:,sccache-bucket: -- "$@")
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
  --rust-toolchain-image)
    shift
    rust_toolchain_image="$1"
    ;;
  --runtime-image)
    shift
    runtime_image="$1"
    ;;
  --build-deps-image)
    shift
    build_deps_image="$1"
    ;;
  --commit-datetime)
    shift
    commit_datetime="$1"
    ;;
  --commit-tag)
    shift
    commit_tag="$1"
    ;;
  --sccache-bucket)
    shift
    sccache_bucket="$1"
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

echo "Executables to include in Docker images:"
for executable in $executables; do
  echo "- $executable"
done

# The rust-toolchain image is only consumed by the with-evm-artifacts build
# stage, so require it explicitly for that target (the CI distribution jobs
# pass --rust-toolchain-image). Other targets never reference it.
case "$docker_target" in
"with-evm-artifacts")
  if [ -z "$rust_toolchain_image" ]; then
    echo "Error: --rust-toolchain-image is required for --docker-target ${docker_target}." >&2
    exit 1
  fi
  ;;
esac

# Default the image references to the ones matching the local checkout (via
# 'ci_image_name'/version.sh) when not overridden, so callers that omit the
# '--*-image' options keep working unchanged.
runtime_image="${runtime_image:-${ci_image_name}/runtime:${ci_image_version}}"
build_deps_image="${build_deps_image:-${ci_image_name}/build:${ci_image_version}}"

image_test="${build_deps_image}"
if ! docker inspect --type=image "$image_test" > /dev/null 2>&1; then
  echo "Image $image_test does not exist locally, attempt pull."
  # This pull is just to check whether the image exists
  # remotely. Although costly, it would've been pulled regardless in
  # the docker builds below.
  if ! docker pull "$image_test" > /dev/null 2>&1; then
    echo "Failed to pull image $image_test."
    echo "If you have modified any inputs to the CI images, then you have to rebuild them locally through ./images/create_ci_images.sh."
    exit 1
  else
    echo "Pull of $image_test succeeded"
  fi
fi

# The whole distribution is built by a single `docker buildx bake` invocation
# (see docker-bake.hcl). The intermediate build image (build.Dockerfile) is
# computed once and consumed in-graph by the variant builds (Dockerfile) via a
# build context, so it never round-trips through a registry. bake reads its
# inputs from the environment, one variable per `variable` block.
#
# Notes:
# - $GIT_SHORTREF / $GIT_DATETIME / $GIT_VERSION are used by libversion to
#   assign the correct version to the octez binaries at compile time.
# - ${RUST_TOOLCHAIN_IMAGE} is the image used to build L2 binaries.
# - [--allow network.host] is needed because build.Dockerfile has
#   [RUN --network=host] (for sccache). Cf. the matching comment in
#   build.Dockerfile.
# - CI_PIPELINE_ID / CI_PIPELINE_URL / CI_JOB_ID / CI_JOB_URL / CI_COMMIT_SHA
#   are exported by GitLab CI and picked up by bake automatically.

# Map the requested variants to bake targets. With no variants
# (--variants ""), only the intermediate "build" target is computed; it is
# exported as type=cacheonly, so no image is loaded into the local store.
if [ -z "$variants" ]; then
  bake_targets="build"
else
  bake_targets="$variants"
fi

echo "### Building tezos via docker buildx bake (targets: ${bake_targets})..."

# shellcheck disable=SC2086
IMAGE_NAME="$image_name" \
  MINIMAL_IMAGE_NAME="${image_name%?}" \
  IMAGE_VERSION="$image_version" \
  RUNTIME_IMAGE="$runtime_image" \
  BUILD_DEPS_IMAGE="$build_deps_image" \
  DOCKER_TARGET="$docker_target" \
  OCTEZ_EXECUTABLES="$executables" \
  GIT_SHORTREF="$commit_short_sha" \
  GIT_DATETIME="$commit_datetime" \
  GIT_VERSION="$commit_tag" \
  COMMIT_SHORT_SHA="$commit_short_sha" \
  RUST_TOOLCHAIN_IMAGE="$rust_toolchain_image" \
  SCCACHE_GCS_BUCKET="$sccache_bucket" \
  docker buildx bake \
  --allow network.host \
  -f "$src_dir/docker-bake.hcl" \
  $bake_targets

# The variants are loaded into the local image store; the "build" target is
# cache-only (no image). Pushing to the registry is delegated to
# docker_push_all.sh downstream.
echo "### Successfully ran docker buildx bake (targets: ${bake_targets})"

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
  [--ci-image-name <IMAGE_NAME> ]
  [--ci-image-version <IMAGE_TAG> ]
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

    The build uses following images from the CI image suite:
     - CI_IMAGE_NAME/runtime:CI_IMAGE_VERSION
     - CI_IMAGE_NAME/build:CI_IMAGE_VERSION
    The CI images are defined in images/ci,
    and by default, CI_IMAGE_NAME refers to the images build from
    directory in the tezos/tezos CI. CI_IMAGE_VERSION is set using
    images/image_tag.sh such that a version corresponding to the local checkout
    is used.

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
        --ci-image-name CI_IMAGE_NAME
            Name of the CI image.

        --ci-image-version CI_IMAGE_VERSION
            Version of the CI image.

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
    CI_IMAGE_NAME: $ci_image_name
    CI_IMAGE_VERSION: $ci_image_version
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
  -l help,image-name:,image-version:,ci-image-name:,ci-image-version:,executables:,commit-short-sha:,variants:,docker-target:,rust-toolchain-image-name:,rust-toolchain-image-tag:,commit-datetime:,commit-tag: -- "$@")
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
  --ci-image-name)
    shift
    ci_image_name="$1"
    ;;
  --ci-image-version)
    shift
    ci_image_version="$1"
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

image_test="${ci_image_name}/build:${ci_image_version}"
if ! docker inspect --type=image "$image_test" > /dev/null 2>&1; then
  echo "CI image $image_test does not exist locally, attempt pull."
  # This pull is just to check whether the image exists
  # remotely. Although costly, it would've been pulled regardless in
  # the docker builds below.
  if ! docker pull "$image_test" > /dev/null 2>&1; then
    echo "Failed to pull CI image $image_test."
    echo "If you have modified any inputs to the CI images, then you have to rebuild them locally through ./images/create_ci_images.sh."
    exit 1
  fi
fi

echo "### Building tezos..."

docker buildx build \
  --build-arg "BASE_IMAGE=$ci_image_name" \
  --build-arg "BASE_IMAGE_VERSION=build:$ci_image_version" \
  --build-arg "OCTEZ_EXECUTABLES=${executables}" \
  --build-arg "GIT_SHORTREF=${commit_short_sha}" \
  --build-arg "GIT_DATETIME=${commit_datetime}" \
  --build-arg "GIT_VERSION=${commit_tag}" \
  --build-arg "RUST_TOOLCHAIN_IMAGE_NAME=$rust_toolchain_image_name" \
  --build-arg "RUST_TOOLCHAIN_IMAGE_TAG=$rust_toolchain_image_tag" \
  --target "$docker_target" \
  --tag "$build_image_name:$image_version" \
  -f build.Dockerfile \
  "$src_dir"

echo "### Successfully built docker image: $build_image_name:$image_version"

for variant in $variants; do
  case "$variant" in
  debug)
    docker buildx build \
      --build-arg "BASE_IMAGE=$ci_image_name" \
      --build-arg "BASE_IMAGE_VERSION=runtime:$ci_image_version" \
      --build-arg "BASE_IMAGE_VERSION_NON_MIN=build:$ci_image_version" \
      --build-arg "BUILD_IMAGE=${build_image_name}" \
      --build-arg "BUILD_IMAGE_VERSION=${image_version}" \
      --build-arg "COMMIT_SHORT_SHA=${commit_short_sha}" \
      --label "com.tezos.build-pipeline-id"="${CI_PIPELINE_ID}" \
      --label "com.tezos.build-pipeline-url"="${CI_PIPELINE_URL}" \
      --label "com.tezos.build-job-id"="${CI_JOB_ID}" \
      --label "com.tezos.build-job-url"="${CI_JOB_URL}" \
      --label "com.tezos.build-tezos-revision"="${CI_COMMIT_SHA}" \
      --target=debug \
      --tag "${image_name}debug:$image_version" \
      "$src_dir"

    echo "### Successfully built docker image: ${image_name}debug:$image_version"
    ;;

  bare)
    docker buildx build \
      --build-arg "BASE_IMAGE=$ci_image_name" \
      --build-arg "BASE_IMAGE_VERSION=runtime:$ci_image_version" \
      --build-arg "BASE_IMAGE_VERSION_NON_MIN=build:$ci_image_version" \
      --build-arg "BUILD_IMAGE=${build_image_name}" \
      --build-arg "BUILD_IMAGE_VERSION=${image_version}" \
      --build-arg "COMMIT_SHORT_SHA=${commit_short_sha}" \
      --label "com.tezos.build-pipeline-id"="${CI_PIPELINE_ID}" \
      --label "com.tezos.build-pipeline-url"="${CI_PIPELINE_URL}" \
      --label "com.tezos.build-job-id"="${CI_JOB_ID}" \
      --label "com.tezos.build-job-url"="${CI_JOB_URL}" \
      --label "com.tezos.build-tezos-revision"="${CI_COMMIT_SHA}" \
      --target=bare \
      --tag "${image_name}bare:$image_version" \
      "$src_dir"

    echo "### Successfully built docker image: ${image_name}bare:$image_version"
    ;;

  minimal)
    docker buildx build \
      --build-arg "BASE_IMAGE=$ci_image_name" \
      --build-arg "BASE_IMAGE_VERSION=runtime:$ci_image_version" \
      --build-arg "BASE_IMAGE_VERSION_NON_MIN=build:$ci_image_version" \
      --build-arg "BUILD_IMAGE=${build_image_name}" \
      --build-arg "BUILD_IMAGE_VERSION=${image_version}" \
      --build-arg "COMMIT_SHORT_SHA=${commit_short_sha}" \
      --label "com.tezos.build-pipeline-id"="${CI_PIPELINE_ID}" \
      --label "com.tezos.build-pipeline-url"="${CI_PIPELINE_URL}" \
      --label "com.tezos.build-job-id"="${CI_JOB_ID}" \
      --label "com.tezos.build-job-url"="${CI_JOB_URL}" \
      --label "com.tezos.build-tezos-revision"="${CI_COMMIT_SHA}" \
      --target=minimal \
      --tag "${image_name%?}:$image_version" \
      "$src_dir"

    echo "### Successfully built docker image: ${image_name%?}:$image_version"
    ;;
  esac
done

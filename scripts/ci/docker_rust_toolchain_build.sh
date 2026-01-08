#!/bin/sh
#
# Build rust-toolchain image for CI jobs and Octez docker distribution.
#
# Reads the following environment variables:
#  - 'rust_toolchain_image_name'
#  - 'rust_toolchain_image_tag' (optional)
#  - 'DOCKER_FORCE_BUILD': set by the operator
#  - 'CI_COMMIT_REF_NAME': set by GitLab CI
#  - 'CI_DEFAULT_BRANCH': set by GitLab CI
#  - 'CI_PIPELINE_ID': set by GitLab CI
#  - 'CI_PIPELINE_URL': set by GitLab CI
#  - 'CI_JOB_ID': set by GitLab CI
#  - 'CI_JOB_URL': set by GitLab CI
#  - 'CI_COMMIT_SHA': set by GitLab CI
#
# The image is tagged with $rust_toolchain_image_name:REF_TAG and
# $rust_toolchain_image_name:TAG. REF_TAG is a sanitized version of $CI_COMMIT_REF_NAME.
# If $rust_toolchain_image_tag is set, then TAG contains this value. If not, TAG contains
# a hash of this image's inputs.
#
# When running in the CI, $rust_toolchain_image_tag is not set. The
# image is tagged with the input hash. In order for subsequent jobs to
# use the image built by this script, it stores the TAG in a dotenv
# file that is passed as artifacts to those jobs. By also tagging with
# the REF_TAG, we allow subsequent pipelines to refer to
# images built on a given branch, which is used for caching.
#
# When building the image, we get caches from
#  - $rust_toolchain_image_name:REF_TAG and
#  - $rust_toolchain_image_name:$CI_DEFAULT_BRANCH.
# That is, from previous builds on the same branch and from previous
# builds on the master branch.
#
# The inputs of this image are the set of paths defined in
# 'images/rust-toolchain/inputs'.

set -eu

set -x

# rust_toolchain_image_name is set in the variables of '.gitlab-ci.yml'
# shellcheck disable=SC2154
image_base="${rust_toolchain_image_name}"

image_tag="${rust_toolchain_image_tag:-}"
if [ -z "$image_tag" ]; then
  # by default, tag with the hash of this image's input which is the set of paths
  # defined in images/rust-toolchain/inputs.
  image_tag=$(./images/image_tag.sh images/rust-toolchain)
fi
image_name="${image_base}:${image_tag}"

# shellcheck source=./scripts/ci/docker_registry.inc.sh
. ./scripts/ci/docker_registry.inc.sh
docker_image_ref_tag=$(echo "${CI_COMMIT_REF_NAME}" | sanitizeTag)

# Store the image name for jobs that use it.
echo "rust_toolchain_image_tag=$image_tag" > rust_toolchain_image_tag.env

./scripts/ci/docker_initialize.sh

# Build image unless it already exists in the registry.

# Enforce image rebuild ignoring inputs changes (i.e. rebuild the image
# for security updates purposes)
skip_registry_cache_check=${DOCKER_FORCE_BUILD:-"false"}
docker_no_cache_option_placeholder="--no-cache"

if [ "$skip_registry_cache_check" != "true" ]; then
  docker_no_cache_option_placeholder=""
  if docker manifest inspect "${image_name}" > /dev/null; then
    echo "Image ${image_name} already exists in the registry, update tag ${image_base}:${CI_COMMIT_REF_SLUG}."
    regctl image copy "${image_name}" "${image_base}:${CI_COMMIT_REF_SLUG}"
    exit 0
  fi
fi
if [ "$skip_registry_cache_check" = "true" ]; then
  echo "Force rebuild of CI images, using no cached layers"
fi
echo "Build ${image_name}"

# APT_PROXY_DEB is a variable that is set in the tezos group CI configuration
./images/create_image.sh \
  "rust-toolchain" \
  "${image_base}" \
  "${image_tag}" \
  --build-arg=BUILDKIT_INLINE_CACHE=1 \
  --build-arg APT_PROXY="${APT_PROXY_DEB:-}" \
  --build-arg IMAGE="${IMAGE:-}" \
  --cache-from="${image_base}:${docker_image_ref_tag}" \
  --cache-from="${image_base}:${CI_DEFAULT_BRANCH}" \
  --label "com.tezos.build-pipeline-id"="${CI_PIPELINE_ID}" \
  --label "com.tezos.build-pipeline-url"="${CI_PIPELINE_URL}" \
  --label "com.tezos.build-job-id"="${CI_JOB_ID}" \
  --label "com.tezos.build-job-url"="${CI_JOB_URL}" \
  --label "com.tezos.build-tezos-revision"="${CI_COMMIT_SHA}" \
  --push \
  -t "${image_base}:${docker_image_ref_tag}" \
  -t "${image_base}" \
  $docker_no_cache_option_placeholder

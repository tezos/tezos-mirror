#!/bin/sh
#
# Build rust-sdk-bindings image for CI jobs and Octez docker distribution.
#
# Reads the following environment variables:
#  - 'rust_toolchain2_image_name'
#  - 'rust_sdk_bindings_image_tag' (optional)
#  - 'CI_COMMIT_REF_NAME': set by GitLab CI
#  - 'CI_DEFAULT_BRANCH': set by GitLab CI
#  - 'CI_PIPELINE_ID': set by GitLab CI
#  - 'CI_PIPELINE_URL': set by GitLab CI
#  - 'CI_JOB_ID': set by GitLab CI
#  - 'CI_JOB_URL': set by GitLab CI
#  - 'CI_COMMIT_SHA': set by GitLab CI
#
# The image is tagged with $rust_sdk_bindings_image_name:REF_TAG and
# $rust_sdk_bindings_image_name:TAG. REF_TAG is a sanitized version of $CI_COMMIT_REF_NAME.
# If $rust_sdk_bindings_image_tag is set, then TAG contains this value. If not, TAG contains
# a hash of this image's inputs.
#
# When running in the CI, $rust_sdk_bindings_image_tag is not set. The
# image is tagged with the input hash. In order for subsequent jobs to
# use the image built by this script, it stores the TAG in a dotenv
# file that is passed as artifacts to those jobs. By also tagging with
# the REF_TAG, we allow subsequent pipelines to refer to
# images built on a given branch, which is used for caching.
#
# When building the image, we get caches from
#  - $rust_sdk_bindings_image_name:REF_TAG and
#  - $rust_sdk_bindings_image_name:$CI_DEFAULT_BRANCH.
# That is, from previous builds on the same branch and from previous
# builds on the master branch.
#
# The inputs of this image are the set of paths defined in
# 'images/rust-sdk-bindings/inputs'.

set -eu

# rust_sdk_bindings_image_name is set in the variables of '.gitlab-ci.yml'
# shellcheck disable=SC2154
if [ -z "${rust_sdk_bindings_image_name:-}" ] || [ -z "${rust_sdk_bindings_image_name_protected:-}" ]; then
  echo "Impossible to create rust-sdk-bindings docker image. rust_sdk_bindings_image_name is undefined: It should be defined in .gitlab-ci.yml."
  exit 1
else
  image_base="${rust_sdk_bindings_image_name}"
  image_base_protected="${rust_sdk_bindings_image_name_protected}"
fi

image_tag="${rust_sdk_bindings_image_tag:-}"
if [ -z "$image_tag" ]; then
  # by default, tag with the hash of this image's input which is the set of paths
  # defined in images/rust-sdk-bindings/inputs.
  image_tag=$(./images/image_tag.sh images/rust-sdk-bindings)
fi
image_name="${image_base}:${image_tag}"

# shellcheck source=./scripts/ci/docker_registry.inc.sh
. ./scripts/ci/docker_registry.inc.sh

if [ -z "${CI_COMMIT_REF_NAME:-}" ]; then
  echo "Impossible to create rust-sdk-bindings docker image. CI_COMMIT_REF_NAME is undefined."
  exit 1
else
  docker_image_ref_tag=$(echo "${CI_COMMIT_REF_NAME}" | sanitizeTag)
fi

# Store the image name for jobs that use it.
echo "rust_sdk_bindings_image_tag=$image_tag" > rust_sdk_bindings_image_tag.env

echo "Build ${image_name}"

# shellcheck disable=SC2046
./images/create_image.sh \
  "rust-sdk-bindings" \
  "${image_base}" \
  "${image_tag}" \
  --push \
  --build-arg=BUILDKIT_INLINE_CACHE=1 \
  --cache-from="${image_base}:${docker_image_ref_tag}" \
  --cache-from="${image_base_protected}:${CI_DEFAULT_BRANCH}" \
  --label "com.tezos.build-pipeline-id"="${CI_PIPELINE_ID}" \
  --label "com.tezos.build-pipeline-url"="${CI_PIPELINE_URL}" \
  --label "com.tezos.build-job-id"="${CI_JOB_ID}" \
  --label "com.tezos.build-job-url"="${CI_JOB_URL}" \
  --label "com.tezos.build-tezos-revision"="${CI_COMMIT_SHA}" \
  $(if [ "${DOCKER_FORCE_BUILD:-false}" = "true" ]; then echo "--no-cache"; fi) \
  -t "${image_base}:${docker_image_ref_tag}"

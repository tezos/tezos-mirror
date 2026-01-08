#!/bin/sh
#
# Build client-libs-dependencies image for CI jobs.
#
# Reads the following environment variables:
#  - 'client_libs_dependencies_image_name'
#  - 'client_libs_dependencies_image_tag' (optional)
#  - 'DOCKER_FORCE_BUILD': set by the operator
#  - 'CI_COMMIT_REF_NAME': set by GitLab CI
#  - 'CI_DEFAULT_BRANCH': set by GitLab CI
#  - 'CI_PIPELINE_ID': set by GitLab CI
#  - 'CI_PIPELINE_URL': set by GitLab CI
#  - 'CI_JOB_ID': set by GitLab CI
#  - 'CI_JOB_URL': set by GitLab CI
#  - 'CI_COMMIT_SHA': set by GitLab CI
#
# The image is tagged with $client_libs_dependencies_image_name:REF_TAG and
# $client_libs_dependencies_image_name:TAG. REF_TAG is a sanitized version of $CI_COMMIT_REF_NAME.
# If $client_libs_dependencies_image_tag is set, then TAG contains this value. If not, TAG contains
# a hash of this image's inputs.
#
# When running in the CI, $client_libs_dependencies_image_tag is not set. The
# image is tagged with the input hash. In order for subsequent jobs to
# use the image built by this script, it stores the TAG in a dotenv
# file that is passed as artifacts to those jobs. By also tagging with
# the REF_TAG, we allow subsequent pipelines to refer to
# images built on a given branch, which is used for caching.
#
# When building the image, we get caches from
#  - $client_libs_dependencies_image_name:REF_TAG and
#  - $client_libs_dependencies_image_name:$CI_DEFAULT_BRANCH.
# That is, from previous builds on the same branch and from previous
# builds on the master branch.
#
# The inputs of this image is the set of paths defined in
# 'images/client-libs-dependencies/inputs'.

set -eu

set -x

# client_libs_dependencies_image_name is set in the variables of '.gitlab-ci.yml'
# shellcheck disable=SC2154
image_base="${client_libs_dependencies_image_name}"

image_tag="${client_libs_dependencies_image_tag:-}"
if [ -z "$image_tag" ]; then
  # by default, tag with the hash of this image's input which is the set of paths
  # defined in images/client-libs-dependencies/inputs.
  image_tag=$(./images/image_tag.sh images/client-libs-dependencies)
fi
image_name="${image_base}:${image_tag}"

# shellcheck source=./scripts/ci/docker_registry.inc.sh
. ./scripts/ci/docker_registry.inc.sh
docker_image_ref_tag=$(echo "${CI_COMMIT_REF_NAME}" | sanitizeTag)

# Store the image name for jobs that use it.
echo "client_libs_dependencies_image_tag=$image_tag" > client_libs_dependencies_image_tag.env

./scripts/ci/docker_initialize.sh

# Build image unless it already exists in the registry.
#
# Enforce image rebuild ignoring inputs changes (i.e. rebuild the image
# for security updates purposes)
skip_registry_cache_check=${DOCKER_FORCE_BUILD:-"false"}

if [ "$skip_registry_cache_check" != "true" ]; then
  if docker manifest inspect "${image_name}" > /dev/null; then
    echo "Image ${image_name} already exists in the registry, update tag ${image_base}:${docker_image_ref_tag}."
    regctl image copy "${image_name}" "${image_base}:${docker_image_ref_tag}"
    exit 0
  fi
fi
if [ "$skip_registry_cache_check" = "true" ]; then
  echo "Force rebuild of CI images, using no cached layers"
fi
echo "Build ${image_name}"

# shellcheck disable=SC2046
./images/create_client_libs_dependencies_image.sh \
  "${image_base}" \
  "${image_tag}" \
  --build-arg=BUILDKIT_INLINE_CACHE=1 \
  --cache-from="${image_base}:${docker_image_ref_tag}" \
  --cache-from="${image_base}:${CI_DEFAULT_BRANCH}" \
  --label "com.tezos.build-pipeline-id"="${CI_PIPELINE_ID}" \
  --label "com.tezos.build-pipeline-url"="${CI_PIPELINE_URL}" \
  --label "com.tezos.build-job-id"="${CI_JOB_ID}" \
  --label "com.tezos.build-job-url"="${CI_JOB_URL}" \
  --label "com.tezos.build-tezos-revision"="${CI_COMMIT_SHA}" \
  -t "${image_base}:${docker_image_ref_tag}" \
  $(if [ "$skip_registry_cache_check" = "true" ]; then echo "--no-cache"; fi)

./images/client-libs-dependencies/check_versions.sh "${image_base}" "${image_tag}"

# Push image
docker push --all-tags "${image_base}"

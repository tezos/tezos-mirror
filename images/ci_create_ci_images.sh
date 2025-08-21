#!/bin/sh
#
# Build images for CI jobs and the Octez Docker Distribution.
#
# Reads the following environment variables:
#  - 'ci_image_name'
#  - 'ci_image_tag' (optional)
#  - 'DOCKER_FORCE_BUILD': set by the operator
#  - 'ARCH'
#  - 'CI_COMMIT_REF_SLUG': set by GitLab CI
#  - 'CI_DEFAULT_BRANCH': set by GitLab CI
#  - 'CI_PIPELINE_ID': set by GitLab CI
#  - 'CI_PIPELINE_URL': set by GitLab CI
#  - 'CI_JOB_ID': set by GitLab CI
#  - 'CI_JOB_URL': set by GitLab CI
#  - 'CI_COMMIT_SHA': set by GitLab CI
#
# The image is tagged with
# $ci_image_name:$CI_COMMIT_REF_SLUG and
# $ci_image_name:TAG. If
# $ci_image_tag is set, then TAG contains this
# value. If not, TAG contains a hash of this image's inputs.
#
# When running in the CI, $ci_image_tag is not set. The
# image is tagged with the input hash. In order for subsequent jobs to
# use the image built by this script, it stores the TAG in a dotenv
# file that is passed as artifacts to those jobs. By also tagging with
# the $CI_COMMIT_REF_SLUG, we allow subsequent pipelines to refer to
# images built on a given branch, which is used for caching.
#
# When building the image, we get caches from
#  - $ci_image_name:$CI_COMMIT_REF_SLUG and
#  - $ci_image_name:$CI_DEFAULT_BRANCH.
# That is, from previous builds on the same branch and from previous
# builds on the master branch.
#
# The inputs of this image is the set of paths defined in
# 'images/ci/inputs'.

set -eu

if [ -n "${TRACE:-}" ]; then set -x; fi

images_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
repo_dir="$(dirname "$images_dir")"
cd "$repo_dir"

# shellcheck source=./images/ci.inc.sh
. "$images_dir"/ci.inc.sh

# ci_image_name is set in the variables of '.gitlab-ci.yml'
# shellcheck disable=SC2154
image_base="${ci_image_name}"
# shellcheck disable=SC2154
image_name_protected="${ci_image_name_protected}"

arch=${ARCH:-amd64}

image_tag_suffix="${ci_image_tag:-}"
if [ -z "$image_tag_suffix" ]; then
  # by default, tag with the hash of this image's input which is the set of paths
  # defined in images/ci/inputs. Requires wordsplitting on the inputs.
  image_tag_suffix=$(./images/image_tag.sh images/ci)
fi
image_tag=$(docker_tag "$arch" "$image_tag_suffix")

# Store the image name for jobs that use it.
echo "ci_image_tag=$image_tag" > ci_image_tag.env

./scripts/ci/docker_initialize.sh

# Build images unless they already exists in the registry..
exists="true"

# Enforce image rebuild ignoring inputs changes (i.e. rebuild the image
# for security updates purposes)
skip_registry_cache_check=${DOCKER_FORCE_BUILD:-"false"}
docker_no_cache_option_placeholder="--no-cache"

if [ "$skip_registry_cache_check" != "true" ]; then
  docker_no_cache_option_placeholder=""
  for layer_target in $valid_layer_targets; do
    image_name="${image_base}/${layer_target}:${image_tag}"
    if ! docker manifest inspect "${image_name}" > /dev/null 2>&1; then
      echo "Image ${image_name} does not exist in the registry, re-build."
      exists="false"
    else
      image_name_extra=${image_base}/${layer_target}:${arch}--${CI_COMMIT_REF_SLUG}
      echo "Image ${image_name} already exists in the registry, update tag ${image_name_extra}."
      regctl image copy "${image_name}" "${image_name_extra}"
    fi
  done
  if [ "$exists" = "true" ]; then
    echo "CI images at ${image_base} with tag suffix ${image_tag_suffix} already exists in the registry, do nothing."
    exit 0
  fi
fi
if [ "$skip_registry_cache_check" = "true" ]; then
  echo "Force rebuild of CI images, using no cached layers"
fi
echo "Build CI images with image_tag_suffix $image_tag_suffix"

./images/create_ci_images.sh \
  --image-base "${image_base}" \
  --image-base-protected "${image_name_protected}" \
  --tag-suffix "${image_tag_suffix}" \
  --tag-cache-suffix "${CI_DEFAULT_BRANCH}" \
  --tag-extra "${CI_COMMIT_REF_SLUG}" \
  --targetarch "${arch}" \
  -- \
  --label "com.tezos.build-pipeline-id"="${CI_PIPELINE_ID}" \
  --label "com.tezos.build-pipeline-url"="${CI_PIPELINE_URL}" \
  --label "com.tezos.build-job-id"="${CI_JOB_ID}" \
  --label "com.tezos.build-job-url"="${CI_JOB_URL}" \
  --label "com.tezos.build-tezos-revision"="${CI_COMMIT_SHA}" \
  --secret "id=npm_token,src=/tmp/npm_token.txt" \
  --push \
  $docker_no_cache_option_placeholder

./images/ci/scripts/check_versions.sh \
  "${image_base}" "${image_tag_suffix}" "${arch}"

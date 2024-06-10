#!/bin/sh
#
# Build opam-repository images for CI jobs.
#
# Reads the following environment variables:
#  - 'opam_repository_image_name'
#  - 'opam_repository_image_tag' (optional)
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
# $opam_repository_image_name:$CI_COMMIT_REF_SLUG and
# $opam_repository_image_name:TAG. If
# $opam_repository_image_tag is set, then TAG contains this
# value. If not, TAG contains a hash of this image's inputs.
#
# When running in the CI, $opam_repository_image_tag is not set. The
# image is tagged with the input hash. In order for subsequent jobs to
# use the image built by this script, it stores the TAG in a dotenv
# file that is passed as artifacts to those jobs. By also tagging with
# the $CI_COMMIT_REF_SLUG, we allow subsequent pipelines to refer to
# images built on a given branch, which is used for caching.
#
# When building the image, we get caches from
#  - $opam_repository_image_name:$CI_COMMIT_REF_SLUG and
#  - $opam_repository_image_name:$CI_DEFAULT_BRANCH.
# That is, from previous builds on the same branch and from previous
# builds on the master branch.
#
# The inputs of this image is the set of paths defined in
# 'images/opam-repository/inputs'.

set -eu

if [ -n "${TRACE:-}" ]; then set -x; fi

images_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
repo_dir="$(dirname "$images_dir")"
cd "$repo_dir"

# shellcheck source=./images/opam-repository.inc.sh
. "$images_dir"/opam-repository.inc.sh

# opam_repository_image_name is set in the variables of '.gitlab-ci.yml'
# shellcheck disable=SC2154
image_base="${opam_repository_image_name}"

arch=${ARCH:-amd64}

image_tag_suffix="${opam_repository_image_tag:-}"
if [ -z "$image_tag_suffix" ]; then
  # by default, tag with the hash of this image's input which is the set of paths
  # defined in images/opam-repository/inputs. Requires wordsplitting on the inputs.
  image_tag_suffix=$(./images/image_tag.sh images/opam-repository)
fi
image_tag=$(docker_tag "$arch" "$image_tag_suffix")

# Store the image name for jobs that use it.
echo "opam_repository_image_tag=$image_tag" > opam_repository_image_tag.env

./scripts/ci/docker_initialize.sh

# Build images unless they already exists in the registry..
exists="true"
for layer_target in $valid_layer_targets; do
  image_name="${image_base}/${layer_target}:${image_tag}"
  if ! docker manifest inspect "${image_name}" > /dev/null 2>&1; then
    echo "Image ${image_name} does not exists in the registry, re-build."
    exists="false"
  else
    image_name_extra=${image_base}/${layer_target}:${arch}--${CI_COMMIT_REF_SLUG}
    echo "Image ${image_name} already exists in the registry, update tag ${image_name_extra}."
    regctl image copy "${image_name}" "${image_name_extra}"
  fi
done
if [ "$exists" = "true" ]; then
  echo "opam-repository images at ${image_base} with tag suffix ${image_tag_suffix} already exists in the registry, do nothing."
  exit 0
fi

echo "Build opam-repository images with image_tag_suffix $image_tag_suffix"

./images/create_opam_repository_images.sh \
  --image-base "${image_base}" \
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
  --push

./images/opam-repository/scripts/check_versions.sh \
  "${image_base}" "${image_tag_suffix}" "${arch}"

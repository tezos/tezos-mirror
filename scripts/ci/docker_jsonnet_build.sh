#!/bin/sh

# This script builds in the CI the docker image as described in `images/jsonnet`.

set -eu

# jsonnet_image_name is set in the variables of '.gitlab-ci.yml'
if [ -z "${jsonnet_image_name:-}" ]; then
  echo "Impossible to create jsonnet docker image. jsonnet_image_name is undefined: It should be defined in .gitlab-ci.yml."
  exit 1
else
  image_base="${jsonnet_image_name}"
fi

image_tag="${jsonnet_image_tag:-}"
if [ -z "$image_tag" ]; then
  # by default, tag with the hash of this image's input which is the set of paths
  # defined in images/rust-toolchain/inputs.
  image_tag=$(./images/image_tag.sh images/jsonnet)
fi
image_name="${image_base}:${image_tag}"

# shellcheck source=./scripts/ci/docker_registry.inc.sh
. ./scripts/ci/docker_registry.inc.sh

if [ -z "${CI_COMMIT_REF_NAME:-}" ]; then
  echo "Impossible to create jsonnet docker image. CI_COMMIT_REF_NAME is undefined."
  exit 1
else
  docker_image_ref_tag=$(echo "${CI_COMMIT_REF_NAME}" | sanitizeTag)
fi

# Store the image name for jobs that use it.
echo "jsonnet_image_tag=$image_tag" > jsonnet_image_tag.env

echo "Build ${image_name}"

# Build and push image
./images/create_image.sh \
  "jsonnet" \
  "${image_base}" \
  "${image_tag}" \
  --push \
  --build-arg=BUILDKIT_INLINE_CACHE=1 \
  --cache-from="${image_base}:${docker_image_ref_tag}" \
  --cache-from="${image_base}:${CI_DEFAULT_BRANCH}" \
  --label "com.tezos.build-pipeline-id"="${CI_PIPELINE_ID}" \
  --label "com.tezos.build-pipeline-url"="${CI_PIPELINE_URL}" \
  --label "com.tezos.build-job-id"="${CI_JOB_ID}" \
  --label "com.tezos.build-job-url"="${CI_JOB_URL}" \
  --label "com.tezos.build-tezos-revision"="${CI_COMMIT_SHA}" \
  -t "${image_base}:${docker_image_ref_tag}"

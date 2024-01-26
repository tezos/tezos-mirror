#!/bin/sh

set -eu

set -x

# rust_toolchain_image_name is set in the variables of '.gitlab-ci.yml'
# shellcheck disable=SC2154
image_base="${rust_toolchain_image_name}"
image_tag="${CI_COMMIT_SHA}"
image_name="${image_base}:${image_tag}"

# Get SHA of the latest merge parent (note the ^2 in the git refs
# below, signifying the second merge parent), to fetch cache from.
#
# We fetch caches from the image built on the merge parent (in
# addition to caches built on the merge commits of the master branch)
# to ensure that we have an up-to-date cache when marge-bot is merging
# a series of MRs. In that scenario, it is unlikely that the image
# built on the 'master_branch' by a previously merged MR pipeline is
# ready when the 'before_merging' pipeline of the next MR is executed.
if [ -n "${CI_MERGE_REQUEST_DIFF_BASE_SHA:-}" ]; then
  # This script is running in a MR before_merging pipeline.
  # Attempt to fetch cache from the predecessor of the base of this MR.
  git fetch origin "${CI_MERGE_REQUEST_DIFF_BASE_SHA}"
  merge_parent=$(git show -s --pretty=format:%H "${CI_MERGE_REQUEST_DIFF_BASE_SHA}^2" ||
    echo "merge_parent_not_found")
else
  # This script is running in a master_branch pipelines.
  # Attempt to fetch cache from the predecessor of this commit.
  git fetch origin "${CI_COMMIT_BRANCH}" --depth 2
  merge_parent=$(git show -s --pretty=format:%H HEAD^2 ||
    echo "merge_parent_not_found")
fi

# Build image
docker build images/rust-toolchain \
  --build-arg=BUILDKIT_INLINE_CACHE=1 \
  --cache-from="${image_base}:${CI_COMMIT_REF_SLUG}" \
  --cache-from="${image_base}:${merge_parent}" \
  --cache-from="${image_base}:${CI_DEFAULT_BRANCH}" \
  -t "${image_base}:${CI_COMMIT_REF_SLUG}" \
  -t "${image_name}"

# Push image
docker push --all-tags "${image_base}"

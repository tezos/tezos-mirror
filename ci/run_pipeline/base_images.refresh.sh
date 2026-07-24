#!/bin/sh

set -eu

# Trigger a base_images.refresh pipeline. Modelled on master_branch.sh: the
# pipeline's rule (Rules.base_images_refresh) requires the schedule source, the
# base_images.refresh schedule kind, and the master-ci-images branch, so we
# override CI_COMMIT_BRANCH here. This lets the pipeline be triggered from any
# branch that carries its definition -- in particular to test it before it
# lands on master-ci-images.
#
# Note: CI_COMMIT_SHA / CI_COMMIT_SHORT_SHA come from $BRANCH (the ref), not
# from the CI_COMMIT_BRANCH override, so the rebuilt images are tagged
# master-<$BRANCH short sha>. For a real refresh of the images the CI consumes,
# run this on the master-ci-images branch.
#
# DOCKER_FORCE_BUILD and the CI_COMMIT_REF_SLUG=master override are set by the
# pipeline's own workflow rule, so they are not passed here.

# Project ID defaults to tezos/tezos.
CI_PROJECT_NAMESPACE="${CI_PROJECT_NAMESPACE:-tezos}"
BRANCH="${BRANCH:-$(git rev-parse --abbrev-ref HEAD)}"

# Compile parameters into a JSON input for GitLab's API.
DATA="$(
  cat << EOF
{
  "ref": "$BRANCH",
  "variables": [
    {
      "key": "CI_PIPELINE_SOURCE",
      "value": "schedule"
    },
    {
      "key": "CI_PROJECT_NAMESPACE",
      "value": "$CI_PROJECT_NAMESPACE"
    },
    {
      "key": "CI_COMMIT_BRANCH",
      "value": "master-ci-images"
    },
    {
      "key": "TZ_SCHEDULE_KIND",
      "value": "base_images.refresh"
    }
  ]
}
EOF
)"

# Tell the user what is going to happen.
cat << EOF
Will run a base_images.refresh pipeline for:

               CI_PROJECT_NAMESPACE = $CI_PROJECT_NAMESPACE
                             BRANCH = $BRANCH
                   CI_COMMIT_BRANCH = master-ci-images (overridden)
EOF

SCRIPT_DIR="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
cd "$SCRIPT_DIR"
export DATA
./post_pipeline.sh

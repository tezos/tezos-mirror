#!/bin/sh

set -eu

# Project ID defaults to tezos/tezos.
CI_PROJECT_NAMESPACE="${CI_PROJECT_NAMESPACE:-tezos}"
DOCKER_FORCE_BUILD="${DOCKER_FORCE_BUILD:-false}"
BRANCH="${BRANCH:-$(git rev-parse --abbrev-ref HEAD)}"
CI_MERGE_REQUEST_TARGET_BRANCH_NAME="${CI_MERGE_REQUEST_TARGET_BRANCH_NAME:-master}"
CI_MERGE_REQUEST_DIFF_BASE_SHA="${CI_MERGE_REQUEST_DIFF_BASE_SHA:-$(git merge-base HEAD origin/master)}"

# Compile parameters into a JSON input for GitLab's API.
DATA="$(
  cat << EOF
{
  "ref": "$BRANCH",
  "variables": [
    {
      "key": "CI_PIPELINE_SOURCE",
      "value": "merge_request_event"
    },
    {
      "key": "CI_PROJECT_NAMESPACE",
      "value": "$CI_PROJECT_NAMESPACE"
    },
    {
      "key": "CI_MERGE_REQUEST_TARGET_BRANCH_NAME",
      "value": "$CI_MERGE_REQUEST_TARGET_BRANCH_NAME"
    },
    {
      "key": "CI_MERGE_REQUEST_DIFF_BASE_SHA",
      "value": "$CI_MERGE_REQUEST_DIFF_BASE_SHA"
    },
    {
      "key": "DOCKER_FORCE_BUILD",
      "value": "$DOCKER_FORCE_BUILD"
    }
  ]
}
EOF
)"

# Tell the user what is going to happen.
cat << EOF
Will run a before_merging pipeline for:

               CI_PROJECT_NAMESPACE = $CI_PROJECT_NAMESPACE
                 DOCKER_FORCE_BUILD = $DOCKER_FORCE_BUILD
                             BRANCH = $BRANCH
CI_MERGE_REQUEST_TARGET_BRANCH_NAME = $CI_MERGE_REQUEST_TARGET_BRANCH_NAME
     CI_MERGE_REQUEST_DIFF_BASE_SHA = $CI_MERGE_REQUEST_DIFF_BASE_SHA
EOF

SCRIPT_DIR="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
cd "$SCRIPT_DIR"
export DATA
./post_pipeline.sh

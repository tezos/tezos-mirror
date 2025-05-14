#!/bin/sh

set -eu

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
      "value": "push"
    },
    {
      "key": "CI_PROJECT_NAMESPACE",
      "value": "$CI_PROJECT_NAMESPACE"
    },
    {
      "key": "CI_COMMIT_BRANCH",
      "value": "master"
    }
  ]
}
EOF
)"

# Tell the user what is going to happen.
cat << EOF
Will run a master_branch pipeline for:

               CI_PROJECT_NAMESPACE = $CI_PROJECT_NAMESPACE
                             BRANCH = $BRANCH
EOF

SCRIPT_DIR="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
cd "$SCRIPT_DIR"
export DATA
./post_pipeline.sh

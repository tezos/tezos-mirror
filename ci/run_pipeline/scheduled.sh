#!/bin/sh

set -eu

# Project ID defaults to tezos/tezos.
CI_PROJECT_NAMESPACE="${CI_PROJECT_NAMESPACE:-tezos}"
BRANCH="${BRANCH:-$(git rev-parse --abbrev-ref HEAD)}"
TZ_SCHEDULE_KIND="${TZ_SCHEDULE_KIND:-EXTENDED_TESTS}"
CI_PIPELINE__TYPE="${CI_PIPELINE__TYPE:-schedule_extended_test}"

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
      "key": "TZ_SCHEDULE_KIND",
      "value": "$TZ_SCHEDULE_KIND"
    }
  ]
}
EOF
)"

# Tell the user what is going to happen.
cat << EOF
Will run a schedule_extended_test pipeline for:

               CI_PROJECT_NAMESPACE = $CI_PROJECT_NAMESPACE
                             BRANCH = $BRANCH
                   TZ_SCHEDULE_KIND = $TZ_SCHEDULE_KIND
EOF

SCRIPT_DIR="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
cd "$SCRIPT_DIR"
export DATA
./post_pipeline.sh

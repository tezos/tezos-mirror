#!/bin/sh

# Run an API triggered pipeline ( [TZ_API_KIND] )
# on namespace [CI_PROJECT_NAMESPACE] and branch [BRANCH]
# [NAMESPACE] depends on the argument [--namespace]

set -eu

usage="Usage: Usage: $0 --namespace tezos|nomadic-labs

Runs an API triggered pipeline of type TZ_API_KIND (which default is \"RELEASE_PAGE\").
The pipeline is triggered in <namespace>/tezos.
"

# Default values
TZ_API_KIND="${TZ_API_KIND:-RELEASE_PAGE}"
BRANCH="${BRANCH:-$(git rev-parse --abbrev-ref HEAD)}"

# Parse arguments
while [ $# -gt 0 ]; do
  case "$1" in
  --namespace)
    shift
    if [ $# -eq 0 ]; then
      echo "Missing value for --namespace"
      echo "$usage"
      exit 1
    fi
    if [ "$1" = "tezos" ]; then
      CI_PROJECT_NAMESPACE="tezos"
      export PROJECT_ID=3836952
    else
      if [ "$1" = "nomadic-labs" ]; then
        CI_PROJECT_NAMESPACE="nomadic-labs"
        export PROJECT_ID=9487506
      else
        echo "Invalid namespace: $1"
        echo "$usage"
        exit 1
      fi
    fi
    ;;
  *)
    echo "Unknown argument: $1"
    echo "$usage"
    exit 1
    ;;
  esac
  shift
done

if [ -z "${CI_PROJECT_NAMESPACE:-}" ]; then
  echo "Missing required --namespace"
  echo "$usage"
  exit 1
fi

# Compile parameters into a JSON input for GitLab's API.
DATA="$(
  cat << EOF
{
  "ref": "$BRANCH",
  "variables": [
    {
      "key": "CI_PIPELINE_SOURCE",
      "value": "api"
    },
    {
      "key": "CI_PROJECT_NAMESPACE",
      "value": "$CI_PROJECT_NAMESPACE"
    },
    {
      "key": "TZ_API_KIND",
      "value": "$TZ_API_KIND"
    }
  ]
}
EOF
)"

# Show info
cat << EOF
Will run an API triggered pipeline for:

               CI_PROJECT_NAMESPACE = $CI_PROJECT_NAMESPACE
                             BRANCH = $BRANCH
                        TZ_API_KIND = $TZ_API_KIND
EOF

# Run script from correct location
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd -P)"
cd "$SCRIPT_DIR"

export DATA
./post_pipeline.sh

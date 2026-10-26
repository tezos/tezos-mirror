#!/bin/sh

# Run an [octez_release] pipeline for an existing tag of nomadic-labs/tezos.
# Until this pipeline replaces the other Octez release pipelines, pushing a tag
# does not create it: this script creates it through the API instead.

set -eu

usage="Usage: $0 <tag>"

if [ $# -ne 1 ]; then
  echo "$usage"
  exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd -P)"
cd "$SCRIPT_DIR"

TZ_API_KIND=OCTEZ_RELEASE BRANCH="$1" ./api.sh --namespace nomadic-labs

#!/bin/bash

set -e

ORIGIN=${ORIGIN:-origin}

if [[ "$CI_MERGE_REQUEST_LABELS" =~ (^|,)ci--run-all-tezts($|,) ]]; then
  echo "CI_MERGE_REQUEST_LABELS contains ci--run-all-tezts, test selection is disabled."
  echo "true" > selected_tezts.tsl
  exit 0
fi

abort() {
  echo "true" > selected_tezts.tsl
  exit 17
}

if [ -z "$CI_MERGE_REQUEST_DIFF_BASE_SHA" ]; then
  echo "CI_MERGE_REQUEST_DIFF_BASE_SHA is unspecified or empty, test selection is disabled."
  abort
fi

# If set, diff base against CI_MERGE_REQUEST_SOURCE_BRANCH_SHA instead of HEAD.
# This variable contains the HEAD SHA of the merge request's source branch.
# On merged result pipelines, this is not equivalent on the local git's HEAD,
# as the merged result pipeline runs on an ephemeral, rebased branch.
head=${CI_MERGE_REQUEST_SOURCE_BRANCH_SHA:-HEAD}

echo "---- Fetching HEAD ($head) and base ($CI_MERGE_REQUEST_DIFF_BASE_SHA) from $ORIGIN..."
if ! git fetch "$ORIGIN" "$head" "$CI_MERGE_REQUEST_DIFF_BASE_SHA"; then
  # Example error that was seen in a job:
  # error: RPC failed; curl 92 HTTP/2 stream 5 was not closed cleanly: INTERNAL_ERROR (err 2)
  # error: 50483 bytes of body are still expected
  # fetch-pack: unexpected disconnect while reading sideband packet
  # fatal: protocol error: bad pack header
  echo "Failed to fetch, test selection is disabled."
  abort
fi

echo "---- Diffing..."
CHANGES="$(git diff --name-only "$CI_MERGE_REQUEST_DIFF_BASE_SHA" "$head")"
echo "$CHANGES"

echo "---- Compiling manifest/manifest..."
make --silent -C manifest manifest

echo "---- Selecting tests..."
# "--" ensures that if a filename starts with a dash, it is not interpreted as an option.
# Do not put quotes around $CHANGES since we need files to be separate arguments.
# shellcheck disable=SC2086
TSL="$(manifest/manifest --manifezt -- $CHANGES)"
echo "$TSL"
echo "$TSL" > selected_tezts.tsl
echo "---- Wrote: selected_tezts.tsl"

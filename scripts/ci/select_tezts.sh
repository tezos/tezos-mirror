#!/bin/bash

set -e

ORIGIN=${ORIGIN:-origin}

if [[ "$CI_MERGE_REQUEST_LABELS" =~ (^|,)ci--run-all-tezts($|,) ]]; then
  echo "CI_MERGE_REQUEST_LABELS contains ci--run-all-tezts, test selection is disabled."
  echo "true" > selected_tezts.tsl
  exit 0
fi

if [ -z "$CI_MERGE_REQUEST_DIFF_BASE_SHA" ]; then
  echo "CI_MERGE_REQUEST_DIFF_BASE_SHA is unspecified or empty, test selection is disabled."
  echo "true" > selected_tezts.tsl
  exit 0
fi

echo "---- Fetching HEAD and $CI_MERGE_REQUEST_DIFF_BASE_SHA..."
git fetch "$ORIGIN" HEAD "$CI_MERGE_REQUEST_DIFF_BASE_SHA"

echo "---- Diffing..."
CHANGES="$(git diff --name-only "$CI_MERGE_REQUEST_DIFF_BASE_SHA")"
echo "$CHANGES"

echo "---- Compiling manifest/manifest..."
make -C manifest manifest

echo "---- Selecting tests..."
# "--" ensures that if a filename starts with a dash, it is not interpreted as an option.
# Do not put quotes around $CHANGES since we need files to be separate arguments.
# shellcheck disable=SC2086
TSL="$(manifest/manifest --manifezt -- $CHANGES)"
echo "$TSL"
echo "$TSL" > selected_tezts.tsl
echo "---- Wrote: selected_tezts.tsl"

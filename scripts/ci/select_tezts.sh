#!/bin/sh

set -e

ORIGIN=${ORIGIN:-origin}

if [ -z "$CI_MERGE_REQUEST_DIFF_BASE_SHA" ]; then
  echo "CI_MERGE_REQUEST_DIFF_BASE_SHA is unspecified or empty, cannot continue."
  exit 1
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

#!/bin/bash

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$(dirname "$script_dir")")"

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

# shellcheck source=scripts/ci/gitlab_mr_environment.sh
. "$src_dir"/scripts/ci/gitlab_mr_environment.sh

merge_base=$("$src_dir"/scripts/ci/git_merge_base.sh "$TEZOS_CI_MR_TARGET" "$TEZOS_CI_MR_HEAD" || {
  # Print on stderr to make visible outside command substitution
  echo "Failed to get merge base, test selection is disabled." >&2
  abort
})
if [ -z "$merge_base" ]; then
  echo "Merge base is empty, test selection is disabled."
  abort
fi

echo "---- Fetching source branch HEAD ($TEZOS_CI_MR_HEAD) and base ($merge_base) from $ORIGIN..."
if ! git fetch "$ORIGIN" "$TEZOS_CI_MR_HEAD" "$merge_base"; then
  # Example error that was seen in a job:
  # error: RPC failed; curl 92 HTTP/2 stream 5 was not closed cleanly: INTERNAL_ERROR (err 2)
  # error: 50483 bytes of body are still expected
  # fetch-pack: unexpected disconnect while reading sideband packet
  # fatal: protocol error: bad pack header
  echo "Failed to fetch, test selection is disabled."
  abort
fi

echo "---- Diffing..."
CHANGES="$(git diff --name-only "$merge_base" "$TEZOS_CI_MR_HEAD")"
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

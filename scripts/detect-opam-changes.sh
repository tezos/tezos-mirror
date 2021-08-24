#! /bin/sh

# This script detects changes on files that could potentially affect the opam related jobs.
#
# The check is done as a whole for a merge request instead of separate commits.
#
# If one of the files was modified the `TZ_OPAM_FILES_MODIFIED` is set to `true` in order for the opam
# pipeline to be generated.

FILES="test"

if [ -n "$CI_MERGE_REQUEST_TARGET_BRANCH_NAME" ]; then
  git fetch origin "$CI_MERGE_REQUEST_TARGET_BRANCH_NAME"
  FILES=$(git diff-tree --no-commit-id --name-only -r HEAD..origin/"$CI_MERGE_REQUEST_TARGET_BRANCH_NAME")
else
  echo "Not part of a merge request. No commits to compare."
fi

echo Files changed: "$FILES"

MATCHES=$(echo "$FILES" | tr ' ' '\n' | grep -E ".*dune|.*dune.inc|.*.opam|scripts/version.sh|gitlab-ci.yml")

echo Matches found: "$MATCHES"

if [ -n "$MATCHES" ]; then
  echo "TZ_OPAM_FILES_MODIFIED=true" >> opam.env
else
  echo "No matches found"
fi

cat opam.env

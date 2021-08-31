#! /bin/sh

# This script detects changes on files that could potentially affect the opam related jobs.
#
# The check is done as a whole for a merge request instead of separate commits.
#
# If one of the files was modified the `TZ_OPAM_FILES_MODIFIED` is set to `true` in order for the opam
# pipeline to be generated.

set -e

if [ -n "$CI_MERGE_REQUEST_TARGET_BRANCH_NAME" ]; then
  git fetch origin "$CI_MERGE_REQUEST_TARGET_BRANCH_NAME"
  git fetch origin --depth 50 "$CI_MERGE_REQUEST_REF_PATH"

  FILES=$(git diff-tree --no-commit-id --name-only -r "$CI_MERGE_REQUEST_DIFF_BASE_SHA"..HEAD)

  echo Files changed:
  echo "$FILES"

  MATCHES=$(echo "$FILES" | tr ' ' '\n' | grep -E ".*dune|.*dune\.inc|.*\.opam|scripts/version\.sh|\.gitlab-ci\.yml" | cat)

  echo Matches found:
  echo "$MATCHES"

  if [ -n "$MATCHES" ]; then
    export TZ_OPAM_FILES_MODIFIED=true
  else
    echo "No matches found"
  fi
else
  echo "Not part of a merge request. No commits to compare."
fi


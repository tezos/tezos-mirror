#! /bin/sh

FILES="test"

if [ -n "$CI_MERGE_REQUEST_TARGET_BRANCH_NAME" ]; then
  FILES=$(git diff-tree --no-commit-id --name-only -r origin/"$CI_MERGE_REQUEST_TARGET_BRANCH_NAME" -r "$CI_COMMIT_SHA")
else
  echo "Not part of a merge request. No commits to compare."
fi

MATCHES=$($FILES | grep -q -E "dune|dune.inc|*.opam|scripts/version.sh|gitlab-ci.yml")

if [ -n "$MATCHES" ]; then
  echo "TZ_OPAM_FILES_MODIFIED=true" >> opam.env
else
  echo "No matches found"
fi

cat opam.env

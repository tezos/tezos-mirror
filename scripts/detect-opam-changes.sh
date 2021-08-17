#! /bin/sh

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

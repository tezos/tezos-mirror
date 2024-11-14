#!/bin/sh
set -eu

# test the version associated to a git tag. Here we use
# a random version and we check if it is correctly parsed
# The script tezos-version prints the
# same version displayed by octez-node --version

VERSION='10.94'
RANDOMTAG='testtesttest'
TESTBRANCH="$RANDOMTAG"
CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD)

test_version() {
  rm -f _build/default/src/lib_version/generated_git_info.ml
  res=$(dune exec octez-version || :)
  if [ "$res" != "$1" ]; then
    echo "Expected version '$1', got '$res' => FAIL"
    exit 1
  else
    echo "Tag '$2', expected version '$res' => PASS"
  fi
}

cleanup() {
  set +e
  git tag -d "octez-v$VERSION" > /dev/null 2>&1
  git tag -d "octez-v$VERSION"+rc1 > /dev/null 2>&1
  git tag -d "octez-v$VERSION"-rc1 > /dev/null 2>&1
  git checkout "$CURRENT_BRANCH"
  git branch -D "$TESTBRANCH"
  set -e
}

trap cleanup EXIT INT

cleanup

git checkout -b "$TESTBRANCH"

git tag "octez-v$VERSION" -m "test"
test_version "Octez $VERSION" "octez-v$VERSION"

git commit --allow-empty -m "test" > /dev/null 2>&1
test_version "Octez $VERSION+dev" "$(git describe --tags)"

git tag "octez-v$VERSION+rc1" -m "test"
test_version "Octez $VERSION+dev" "octez-v$VERSION+rc1"

git tag "octez-v$VERSION-rc1" -m "test"
test_version "Octez $VERSION~rc1" "octez-v$VERSION-rc1"

git commit --allow-empty -m "test" > /dev/null 2>&1
test_version "Octez $VERSION~rc1+dev" "$(git describe --tags)"

git checkout -

cleanup

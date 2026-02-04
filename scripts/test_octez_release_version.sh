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

# Expects one argument which is the expected output of octez-version.
test_version() {
  rm -f _build/default/src/lib_version/generated_git_info.ml
  git_describe="$(git describe --tags)"
  res=$(dune exec octez-version || :)
  if [ "$res" != "$1" ]; then
    echo "git describe = '$git_describe': expected version '$1', got '$res' => FAIL"
    exit 1
  else
    echo "git describe = '$git_describe': got the expected version '$res' => PASS"
  fi
}

cleanup() {
  set +e
  git tag -d "octez-v$VERSION" > /dev/null 2>&1
  git tag -d "octez-v$VERSION"+rc1 > /dev/null 2>&1
  git tag -d "octez-v$VERSION"-rc1 > /dev/null 2>&1
  git tag -d "octez-v$VERSION"-1 > /dev/null 2>&1
  git checkout "$CURRENT_BRANCH"
  git branch -D "$TESTBRANCH"
  set -e
}

trap cleanup EXIT INT

cleanup

git checkout -b "$TESTBRANCH"

echo "--- Test a simple version tag."
git tag "octez-v$VERSION" -m "test"
test_version "Octez $VERSION (build: 0)"

echo "--- Test a commit above a simple version tag."
git commit --allow-empty -m "test" > /dev/null 2>&1
test_version "Octez $VERSION+dev (build: 0)"

git tag "octez-v$VERSION-1" -m "test"
test_version "Octez $VERSION (build: 1)"

echo "--- Test an invalid version tag."
git tag "octez-v$VERSION+rc1" -m "test"
test_version "Octez 0.0+dev (build: 0)"

echo "--- Test a release candidate version tag."
git tag "octez-v$VERSION-rc1" -m "test"
test_version "Octez $VERSION~rc1 (build: 0)"

echo "--- Test a commit above a release candidate version tag."
git commit --allow-empty -m "test" > /dev/null 2>&1
test_version "Octez $VERSION~rc1+dev (build: 0)"

echo "--- Done testing, cleaning up."
git checkout -

cleanup

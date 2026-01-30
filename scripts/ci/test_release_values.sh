#!/bin/bash

# This script tests the value defined in scripts/ci/octez-packages-version.sh
# and used in ex: scripts/ci/create_debian_repo.sh

CI_COMMIT_SHORT_SHA="test"

# Array of test cases: Each test case is a set of values for CI_COMMIT_REF_PROTECTED, CI_COMMIT_TAG, CI_COMMIT_REF_NAME and expected RELEASETYPE value
tests=(
  "true octez-v1.1 xxx tezos Release 1.1"
  "true octez-v1.1-rc1 xxx tezos ReleaseCandidate 1.1-rc1"
  "true octez-v1.1-rc1 xxx nomadic TestReleaseCandidate 1.1-rc1"
  "true lalalalal xxx tezos SoftRelease"
  "false lalalalal xxx tezos TestBranch"
  "false  octez-v1.1 xxx tezos TestBranch 1.1"
  "true lalalalal xxx tezos TestProtectedBranch"
  "true  octez-v1.1 xxx tezos TestProtectedBranch 1.1"
  "true lalalalal master tezos Master"
  # Add more test cases here as needed
)

all_tests_passed=true

for test in "${tests[@]}"; do
  IFS=' ' read -r CI_COMMIT_REF_PROTECTED CI_COMMIT_TAG CI_COMMIT_REF_NAME CI_PROJECT_NAMESPACE expected_release expected_version <<< "$test"

  export CI_COMMIT_REF_PROTECTED CI_COMMIT_TAG CI_COMMIT_REF_NAME

  . scripts/ci/octez-packages-version.sh

  if [ "$RELEASETYPE" != "$expected_release" ] || [ "$VERSION" != "$expected_version" ]; then
    echo -e "Test failed:\n   CI_COMMIT_REF_PROTECTED=$CI_COMMIT_REF_PROTECTED\n" \
      "  CI_PROJECT_NAMESPACE=$CI_PROJECT_NAMESPACE\n" \
      "  CI_COMMIT_TAG=$CI_COMMIT_TAG\n" \
      "  CI_COMMIT_REF_NAME=$CI_COMMIT_REF_NAME\n" \
      "  Expected release=$expected_release, but got release=$RELEASETYPE\n" \
      "  Expected version=$expected_version, but got version=$VERSION"
    all_tests_passed=false
  else
    echo -e "Test passed:\n   CI_COMMIT_REF_PROTECTED=$CI_COMMIT_REF_PROTECTED\n" \
      "  CI_PROJECT_NAMESPACE=$CI_PROJECT_NAMESPACE\n" \
      "  CI_COMMIT_TAG=$CI_COMMIT_TAG\n" \
      "  CI_COMMIT_REF_NAME=$CI_COMMIT_REF_NAME\n" \
      "  Release=$RELEASETYPE\n" \
      "  Version=$VERSION"
  fi

done

if [ "$all_tests_passed" = false ]; then
  echo "One or more tests failed."
  exit 1
else
  echo "All tests passed."
fi

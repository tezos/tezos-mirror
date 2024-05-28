#!/usr/bin/env bash
#
# Check that shellcheck's version is the expected one. Returns 0
# in case of success, 1 otherwise.

declare -r github="https://github.com/koalaman/shellcheck"
declare -r expected_version="0.9.0" # This version is the one used
# by the CI's test Docker image. This version hence needs to be updated
# when the image is updated and pulls a newer version of shellcheck.
# When this happens, the CI of tezos/tezos will start failing; so we will
# be notified that a change here is required.

command -v shellcheck &> /dev/null || {
  echo "shellcheck $expected_version must be installed. See $github for instructions."
  exit 1
}

actual_version=$(shellcheck --version | grep version: | awk '{print $2}')

if [[ "$actual_version" != "$expected_version" ]]; then
  echo "shellcheck version must be $expected_version, but found version $actual_version."
  echo "Please install shellcheck version $expected_version. See $github for instructions."
  exit 1
fi

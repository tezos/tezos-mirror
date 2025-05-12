#!/bin/sh

## Sourceable file with common variables for other scripts related to Teztale release
# shellcheck disable=SC2034

architectures='x86_64 arm64'

binaries="$(cat "script-inputs/teztale-experimental-executables")"

# Full release tag
# teztale-vX.Y
release=$(echo "${CI_COMMIT_TAG}" | grep -oE '^teztale-v([0-9]+)\.([0-9]+))?$' || :)

# Strips the leading 'teztale-v'
# X.Y
release_no_v=$(echo "${release}" | sed -e 's/^teztale-v//g')

release_name="Teztale version ${release_no_v}"

gitlab_package_name="teztale-${release_no_v}"

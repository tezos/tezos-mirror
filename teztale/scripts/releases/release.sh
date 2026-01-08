#!/bin/sh

## Sourceable file with common variables for other scripts related to Teztale release
# shellcheck disable=SC2034

architectures='x86_64 arm64'

binaries="$(cat "script-inputs/teztale-experimental-executables")"

# Full release tag
# teztale-vX.Y
release=$(echo "${CI_COMMIT_TAG}" | grep -oE '^(teztale|octez)-v([0-9]+)\.([0-9]+)(-(rc|beta)([0-9]+))?$' || :)

# Strips the leading 'teztale-v'
# X.Y
release_no_v=$(echo "${release}" | sed -E 's/^(teztale|octez)-v//g')

# X
release_major_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^(teztale|octez)-v([0-9]+)\.([0-9]+)((-rc[0-9]+)?|(-beta[0-9]+)?)$/\2/p')
# Y
release_minor_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^(teztale|octez)-v([0-9]+)\.([0-9]+)((-rc[0-9]+)?|(-beta[0-9]+)?)$/\3/p')
# Z
release_rc_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^(teztale|octez)-v([0-9]+)\.([0-9]+)(-rc)?([0-9]+)?$/\5/p')

release_name="Teztale version ${release_no_v}"

gitlab_package_name="teztale-${release_no_v}"

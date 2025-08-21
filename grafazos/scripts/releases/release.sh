#!/bin/sh

## Sourceable file with common variables for other scripts related to Grafazos release
# shellcheck disable=SC2034

# Full release tag
# grafazos-vX.Y
release=$(echo "${CI_COMMIT_TAG}" | grep -oE '^(grafazos|octez)-v([0-9]+)\.([0-9]+)(-(rc|beta)([0-9]+))?$' || :)

# Strips the leading 'grafazos-v'
# X.Y
release_no_v=$(echo "${release}" | sed -E 's/^(grafazos|octez)-v//g')

# X
release_major_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^(grafazos|octez)-v([0-9]+)\.([0-9]+)((-rc[0-9]+)?|(-beta[0-9]+)?)$/\2/p')
# Y
release_minor_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^(grafazos|octez)-v([0-9]+)\.([0-9]+)((-rc[0-9]+)?|(-beta[0-9]+)?)$/\3/p')
# Z
release_rc_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^(grafazos|octez)-v([0-9]+)\.([0-9]+)(-rc)?([0-9]+)?$/\5/p')

release_name="Grafazos version ${release_no_v}"

gitlab_dashboards_package_name="grafazos-dashboards-${release_no_v}"
gitlab_source_package_name="grafazos-source-${release_no_v}"

source_tarball="grafazos-${release_no_v}.tar.gz"

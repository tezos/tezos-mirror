#!/bin/sh

## Sourceable file with common variables for other scripts related to Grafazos release
# shellcheck disable=SC2034

# Full release tag
# grafazos-vX.Y
release=$(echo "${CI_COMMIT_TAG}" | grep -oE '^grafazos-v([0-9]+)\.([0-9]+))?$' || :)

# Strips the leading 'grafazos-v'
# X.Y
release_no_v=$(echo "${release}" | sed -e 's/^grafazos-v//g')

release_name="Grafazos version ${release_no_v}"

gitlab_dashboards_package_name="grafazos-dashboards-${release_no_v}"
gitlab_source_package_name="grafazos-source-${release_no_v}"

source_tarball="grafazos-${release_no_v}.tar.gz"

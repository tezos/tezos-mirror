#!/bin/sh

## Sourceable file with common variables for other scripts related to release

# shellcheck disable=SC2034
architectures='x86_64 arm64'

# Full release tag. A release tag is octez-smart-rollup-node-vYYYYMMDD, with an
# optional ".N" (N has no leading zero) and an optional "-rcN" or "-betaN".
release=$(echo "${CI_COMMIT_TAG}" | grep -oE '^octez-smart-rollup-node-v[0-9]{8}(\.[1-9][0-9]*)?(-(rc|beta)[0-9]+)?$' || :)

# [docker_promote_to_latest.sh] reads this name.
# shellcheck disable=SC2034
gitlab_release="${release}"

# Strips the leading 'octez-smart-rollup-node-v'
# YYYYMMDD, YYYYMMDD.N, YYYYMMDD-rcZ or YYYYMMDD.N-rcZ
release_no_v=$(echo "${release}" | sed -E 's/^octez-smart-rollup-node-v//g')

# Replace '.' with '-'
# shellcheck disable=SC2034
release_no_dot=$(echo "${release_no_v}" | sed -e 's/\./-/g')

# shellcheck disable=SC2034
release_tilda=$(echo "${release_no_v}" | sed -e 's/-/~/g')

# The date, always present.
release_date=$(echo "${release_no_v}" | sed -nE 's/^([0-9]{8}).*/\1/p')
# The ".N" counter if present, empty otherwise.
release_counter=$(echo "${release_no_v}" | sed -nE 's/^[0-9]{8}\.([0-9]+).*/\1/p')
# RC
release_rc_version=$(echo "${release_no_v}" | sed -nE 's/.*-rc([0-9]+)$/\1/p')
# Beta
release_beta_version=$(echo "${release_no_v}" | sed -nE 's/.*-beta([0-9]+)$/\1/p')

# Is this a release candidate?
if [ -n "${release_rc_version}" ]; then
  # Yes, release name: YYYYMMDD~rcZ
  # shellcheck disable=SC2034
  gitlab_release_name="Octez Smart Rollup Node Release Candidate v${release_tilda}"
# Is this a beta ?
elif [ -n "${release_beta_version}" ]; then
  gitlab_release_name="Octez Smart Rollup Node Beta v${release_tilda}"
else
  # No, release name: Octez Smart Rollup Node Release YYYYMMDD
  # shellcheck disable=SC2034
  gitlab_release_name="Octez Smart Rollup Node Release v${release_tilda}"
fi

### Compute GitLab generic package names

suffix="${release_no_v}"
gitlab_octez_binaries_package_name="octez-smart-rollup-node-${suffix}"

# YYYYMMDD, YYYYMMDD.N, YYYYMMDD-rcZ, ...
gitlab_package_version="${suffix}"

#!/bin/sh

## Sourceable file with common variables for other scripts related to release

# shellcheck disable=SC2034
architectures='x86_64 arm64'

# Full release tag
# octez-smart-rollup-node-vX.Y, octez-smart-rollup-node-vX.Y-rcZ or octez-smart-rollup-node-vX.Y-betaZ
release=$(echo "${CI_COMMIT_TAG}" | grep -oE '^(octez-smart-rollup-node|octez)-v[0-9]+(\.[0-9]+)?(-(rc|beta)([0-9]+))?$' || :)

# Strips the leading 'octez-smart-rollup-node-v'
# X.Y, X.Y-rcZ or X.Y-betaZ
release_no_v=$(echo "${release}" | sed -E 's/^(octez-smart-rollup-node|octez)-v//g')

# Replace '.' with '-'
# X-Y or X-Y-rcZ
# shellcheck disable=SC2034
release_no_dot=$(echo "${release_no_v}" | sed -e 's/\./-/g')

# shellcheck disable=SC2034
release_tilda=$(echo "${release_no_v}" | sed -e 's/-/~/g')

# X
release_major_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^(octez-smart-rollup-node|octez)-v([0-9]+)\.([0-9]+)((-rc[0-9]+)?|(-beta[0-9]+)?)$/\2/p')
# Y
release_minor_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^(octez-smart-rollup-node|octez)-v([0-9]+)\.([0-9]+)((-rc[0-9]+)?|(-beta[0-9]+)?)$/\3/p')
# RC
release_rc_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^(octez-smart-rollup-node|octez)-v[0-9]+(\.[0-9]+)?(-rc)?([0-9]+)?$/\4/p')
# Beta
release_beta_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^(octez-smart-rollup-node|octez)-v[0-9]+(\.[0-9]+)?(-beta)?([0-9]+)?$/\4/p')

# Is this a release candidate?
if [ -n "${release_rc_version}" ]; then
  # Yes, release name: X.Y~rcZ
  # shellcheck disable=SC2034
  gitlab_release_name="Octez Smart Rollup Node Release Candidate v${release_tilda}"
# Is this a beta ?
elif [ -n "${release_beta_version}" ]; then
  gitlab_release_name="Octez Smart Rollup Node Beta v${release_tilda}"
else
  # No, release name: Octez SMART-ROLLUP Node Release X.Y
  # shellcheck disable=SC2034
  gitlab_release_name="Octez Smart Rollup Node Release v${release_tilda}"
fi

### Compute GitLab generic package names

suffix="${release_no_v}"
gitlab_octez_binaries_package_name="octez-smart-rollup-node-${suffix}"

# X.Y or X.Y-rcZ
gitlab_package_version="${suffix}"

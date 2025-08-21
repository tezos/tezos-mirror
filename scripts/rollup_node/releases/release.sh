#!/bin/sh

## Sourceable file with common variables for other scripts related to release

# shellcheck disable=SC2034
architectures='x86_64 arm64'

# Full octez release tag
# octez-smart-rollup-node-vX.Y, octez-smart-rollup-node-vX.Y-rcZ or octez-smart-rollup-node-vX.Y-betaZ
gitlab_release=$(echo "${CI_COMMIT_TAG}" | grep -oE '^octez-smart-rollup-node-v[0-9]+(\.[0-9]+)?(-(rc|beta)([0-9]+))?$' || :)

# Strips the leading 'octez-smart-rollup-node-v'
# X.Y, X.Y-rcZ or X.Y-betaZ
gitlab_release_no_v=$(echo "${gitlab_release}" | sed -e 's/^octez-smart-rollup-node-v//g')

# Replace '.' with '-'
# X-Y or X-Y-rcZ
# shellcheck disable=SC2034
gitlab_release_no_dot=$(echo "${gitlab_release_no_v}" | sed -e 's/\./-/g')

# shellcheck disable=SC2034
gitlab_release_tilda=$(echo "${gitlab_release_no_v}" | sed -e 's/-/~/g')

# RC
gitlab_release_rc_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^octez-smart-rollup-node-v[0-9]+(\.[0-9]+)?(-rc)?([0-9]+)?$/\3/p')
# Beta
gitlab_release_beta_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^octez-smart-rollup-node-v[0-9]+(\.[0-9]+)?(-beta)?([0-9]+)?$/\3/p')

# Is this a release candidate?
if [ -n "${gitlab_release_rc_version}" ]; then
  # Yes, release name: X.Y~rcZ
  # shellcheck disable=SC2034
  gitlab_release_name="Octez Smart Rollup Node Release Candidate v${gitlab_release_tilda}"
# Is this a beta ?
elif [ -n "${gitlab_release_beta_version}" ]; then
  gitlab_release_name="Octez Smart Rollup Node Beta v${gitlab_release_tilda}"
else
  # No, release name: Octez SMART-ROLLUP Node Release X.Y
  # shellcheck disable=SC2034
  gitlab_release_name="Octez Smart Rollup Node Release v${gitlab_release_tilda}"
fi

### Compute GitLab generic package names

suffix="${gitlab_release_no_v}"
gitlab_octez_binaries_package_name="octez-smart-rollup-node-${suffix}"

# X.Y or X.Y-rcZ
gitlab_package_version="${suffix}"

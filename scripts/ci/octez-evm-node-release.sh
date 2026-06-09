#!/bin/sh

## Sourceable file with common variables for other scripts related to release

# shellcheck disable=SC2034
architectures='x86_64 arm64'

# Full octez release tag
# octez-evm-node-vX.Y, octez-evm-node-vX.Y-rcZ, octez-evm-node-vX.Y-betaZ or octez-evm-node-vX.Y-preZ
gitlab_release=$(echo "${CI_COMMIT_TAG}" | grep -oE '^octez-evm-node-v([0-9]+)\.([0-9]+)(-(rc|beta|pre)?([0-9]+))?$' || :)

# Strips the leading 'octez-evm-node-v'
# X.Y, X.Y-rcZ, X.Y-betaZ or X.Y-preZ
gitlab_release_no_v=$(echo "${gitlab_release}" | sed -e 's/^octez-evm-node-v//g')

# Replace '.' with '-'
# X-Y or X-Y-rcZ
# shellcheck disable=SC2034
gitlab_release_no_dot=$(echo "${gitlab_release_no_v}" | sed -e 's/\./-/g')

# X
gitlab_release_major_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^octez-evm-node-v([0-9]+)\.([0-9]+)((-rc[0-9]+)?|(-beta[0-9]+)?|(-pre[0-9]+)?)$/\1/p')
# Y
gitlab_release_minor_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^octez-evm-node-v([0-9]+)\.([0-9]+)((-rc[0-9]+)?|(-beta[0-9]+)?|(-pre[0-9]+)?)$/\2/p')
# Z
gitlab_release_rc_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^octez-evm-node-v([0-9]+)\.([0-9]+)(-rc)?([0-9]+)?$/\4/p')
# Beta
gitlab_release_beta_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^octez-evm-node-v([0-9]+)\.([0-9]+)(-beta)?([0-9]+)?$/\4/p')
# Pre
gitlab_release_pre_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^octez-evm-node-v([0-9]+)\.([0-9]+)(-pre)?([0-9]+)?$/\4/p')

# Is this a release candidate?
if [ -n "${gitlab_release_rc_version}" ]; then
  # Yes, release name: X.Y~rcZ
  # shellcheck disable=SC2034
  gitlab_release_name="Octez EVM Node Release Candidate ${gitlab_release_major_version}.${gitlab_release_minor_version}~rc${gitlab_release_rc_version}"
# Is this a beta ?
elif [ -n "${gitlab_release_beta_version}" ]; then
  gitlab_release_name="Octez EVM Node Beta ${gitlab_release_major_version}.${gitlab_release_minor_version}~beta${gitlab_release_beta_version}"
# Is this a pre-release ?
elif [ -n "${gitlab_release_pre_version}" ]; then
  # shellcheck disable=SC2034
  gitlab_release_name="Octez EVM Node Pre-release ${gitlab_release_major_version}.${gitlab_release_minor_version}~pre${gitlab_release_pre_version}"
else
  # No, release name: Octez EVM Node Release X.Y
  # shellcheck disable=SC2034
  gitlab_release_name="Octez EVM Node Release ${gitlab_release_major_version}.${gitlab_release_minor_version}"
fi

### Compute GitLab generic package names

if [ -n "${gitlab_release_no_v}" ]; then
  suffix="${gitlab_release_no_v}"
  gitlab_octez_binaries_package_name="octez-evm-node-${suffix}"
else
  suffix=$(date +'%Y%m%d%H%M')+$CI_COMMIT_SHORT_SHA
  gitlab_octez_binaries_package_name="octez-evm-node-${suffix}"
fi

# X.Y or X.Y-rcZ
gitlab_package_version="${suffix}"

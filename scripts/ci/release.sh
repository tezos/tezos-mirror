#!/bin/sh
set -x

## Sourceable file with common variables for other scripts related to release

# shellcheck disable=SC2034
architectures='x86_64 arm64'

binaries='tezos-admin-client tezos-client tezos-node tezos-signer tezos-codec'

while read -r  proto
do
  if [ "${proto}" = '011-PtHangz2' ]
  then
    binaries="${binaries} tezos-accuser-${proto} tezos-baker-${proto} tezos-endorser-${proto}"
  else
    binaries="${binaries} tezos-accuser-${proto} tezos-baker-${proto}"
  fi
done < active_protocol_versions

### Compute GitLab release names

# Remove the 'v' in front
# X.Y or X.Y-rcZ
gitlab_release_no_v=$(echo "${CI_COMMIT_TAG}" | sed -e 's/^v//g')

# Replace '.' with '-'
# X-Y or X-Y-rcZ
# shellcheck disable=SC2034
gitlab_release_no_dot=$(echo "${gitlab_release_no_v}" | sed -e 's/\./-/g')

# X
gitlab_release_major_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^v([0-9]+)\.([0-9]+)(-rc[0-9]+)?$/\1/p')
# Y
gitlab_release_minor_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^v([0-9]+)\.([0-9]+)(-rc[0-9]+)?$/\2/p')
# Z
gitlab_release_rc_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^v([0-9]+)\.([0-9]+)(-rc)?([0-9]+)?$/\4/p')

# Is this a release candidate?
if [ -n "${gitlab_release_rc_version}" ]
then
  # Yes, release name: X.Y~rcZ
  # shellcheck disable=SC2034
  gitlab_release_name="${gitlab_release_major_version}.${gitlab_release_minor_version}~rc${gitlab_release_rc_version}"
else
  # No, release name: Release X.Y
  # shellcheck disable=SC2034
  gitlab_release_name="Release ${gitlab_release_major_version}.${gitlab_release_minor_version}"
fi

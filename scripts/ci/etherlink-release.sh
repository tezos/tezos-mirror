#!/bin/sh

### Compute GitLab release names from git tags

# Git tags for octez releases are on the form `etherlink-vX.Y`, `etherlink-vX.Y-rcZ` or `etherlink-vX.Y-betaZ`.

# Strips the leading 'etherlink-v'
# X.Y, X.Y-rcZ or  X.Y-betaZ
gitlab_release_no_v=$(echo "${CI_COMMIT_TAG}" | sed -e 's/^etherlink-v//g')

# X
gitlab_release_major_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^etherlink-v([0-9]+)\.([0-9]+)(-rc[0-9]+)?$/\1/p')
# Y
gitlab_release_minor_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^etherlink-v([0-9]+)\.([0-9]+)(-rc[0-9]+)?$/\2/p')
# Z
gitlab_release_rc_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^etherlink-v([0-9]+)\.([0-9]+)(-rc)?([0-9]+)?$/\4/p')


# Is this a release candidate?
if [ -n "${gitlab_release_rc_version}" ]; then
  # Yes, release name: X.Y~rcZ
  # shellcheck disable=SC2034
  gitlab_release_name="Etherlink Release Candidate ${gitlab_release_major_version}.${gitlab_release_minor_version}~rc${gitlab_release_rc_version}"
  opam_release_tag="${gitlab_release_major_version}.${gitlab_release_minor_version}~rc${gitlab_release_rc_version}"
else
  # No, release name: Etherlink Release X.Y
  # shellcheck disable=SC2034
  gitlab_release_name="Etherlink Release ${gitlab_release_major_version}.${gitlab_release_minor_version}"
  opam_release_tag="${gitlab_release_major_version}.${gitlab_release_minor_version}"
fi

# X.Y or X.Y-rcZ
gitlab_package_version="${gitlab_release_no_v}"

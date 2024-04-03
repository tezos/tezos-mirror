#!/bin/sh

## Sourceable file with common variables for other scripts related to release

# shellcheck disable=SC2034
architectures='x86_64 arm64'

current_dir=$(cd "$(dirname "${0}")" && pwd)
scripts_dir=$(dirname "$current_dir")
src_dir=$(dirname "$scripts_dir")
script_inputs_dir="$src_dir/script-inputs"

binaries="$(cat "$script_inputs_dir/etherlink-executables")"

# these variables are used in the script scripts/ci/create_gitlab_package.sh
debian_packages="$(find debian:bookworm/ -maxdepth 1 -name etherlink-\*.deb)"
ubuntu_packages="$(find ubuntu:focal/ -maxdepth 1 -name etherlink-\*.deb)"
fedora_packages="$(find fedora:39/ -maxdepth 1 -name etherlink-\*.rpm)"
rockylinux_packages="$(find rockylinux:9.3/ -maxdepth 1 -name etherlink-\*.rpm)"

etherlink_source_content="$script_inputs_dir/etherlink-source-content"

### Compute GitLab release names from git tags

# Strips the leading 'etherlink-v'
# X.Y, X.Y-rcZ or  X.Y-betaZ
gitlab_release_no_v=$(echo "${CI_COMMIT_TAG}" | sed -e 's/^etherlink-v//g')

# Git tags for Etherlink releases are on the form `etherlink-vX.Y`, `etherlink-vX.Y-rcZ` or `etherlink-vX.Y-betaZ`.

# X
gitlab_release_major_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^etherlink-v([0-9]+)\.([0-9]+)(-rc[0-9]+)?$/\1/p')
# Y
gitlab_release_minor_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^etherlink-v([0-9]+)\.([0-9]+)(-rc[0-9]+)?$/\2/p')
# Z
gitlab_release_rc_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^etherlink-v([0-9]+)\.([0-9]+)(-rc)?([0-9]+)?$/\4/p')

# Is this a release candidate?
if [ -n "${gitlab_release_rc_version}" ]; then
  # Yes, release name: Etherlink Release Candidate X.Y~rcZ
  # shellcheck disable=SC2034
  gitlab_release_name="Etherlink Release Candidate ${gitlab_release_major_version}.${gitlab_release_minor_version}~rc${gitlab_release_rc_version}"
else
  # No, release name: Etherlink Release X.Y
  # shellcheck disable=SC2034
  gitlab_release_name="Etherlink Release ${gitlab_release_major_version}.${gitlab_release_minor_version}"
fi

### Compute GitLab generic package names

gitlab_etherlink_binaries_package_name="etherlink-binaries-${gitlab_release_no_v}"
gitlab_etherlink_debian_package_name="etherlink-debian-${gitlab_release_no_v}"
gitlab_etherlink_ubuntu_package_name="etherlink-ubuntu-${gitlab_release_no_v}"
gitlab_etherlink_fedora_package_name="etherlink-fedora-${gitlab_release_no_v}"
gitlab_etherlink_rockylinux_package_name="etherlink-rockylinux-${gitlab_release_no_v}"
gitlab_etherlink_source_package_name="etherlink-source-${gitlab_release_no_v}"

# X.Y or X.Y-rcZ
gitlab_package_version="${gitlab_release_no_v}"

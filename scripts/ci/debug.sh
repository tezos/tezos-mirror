#!/bin/sh
set -eux

### DEBUG

# shellcheck source=./scripts/ci/release.sh
. ./scripts/ci/release.sh

# X.Y or X.Y-rcZ
gitlab_package_name="${gitlab_release_no_v}"

source_tarball="tezos-${gitlab_package_name}.tar.bz2"

# We are using the export-subst feature of git, configured in configured in .gitattributes
# Requires git version >= 2.32

# Create tarball
# https://git-scm.com/docs/git-archive
# https://git-scm.com/docs/gitattributes#_creating_an_archive
git --version
git describe --tags
git archive "${CI_COMMIT_TAG}" | bzip2 > "${source_tarball}"

# Check tarball is valid
tar -tjf "${source_tarball}" > /dev/null

# Checksums
sha256sum "${source_tarball}" > "${source_tarball}.sha256"
sha512sum "${source_tarball}" > "${source_tarball}.sha512"

# Debug gitattributes with export-subst
tar -Oxvf "${source_tarball}" src/lib_version/exe/get_git_info.ml
tar -Oxvf "${source_tarball}" src/lib_version/current_git_info.ml

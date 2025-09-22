#!/bin/sh
set -eu

if [ $# -eq 0 ]; then
  # No arguments are not provided. Assume we're running in a tag
  # pipeline and set package_name accordingly.

  # shellcheck source=./scripts/releases/octez-release.sh
  . ./scripts/releases/octez-release.sh
  package_name="${gitlab_octez_source_package_name}"
  commit="${CI_COMMIT_TAG}"
else
  package_name="$1"
  commit="HEAD"
fi

source_tarball="${package_name}.tar.bz2"

# Pass '--worktree-attributes' to ensure that ignores written by restrict_export_to_octez_source.sh
# are respected.
git archive "${commit}" --format=tar --worktree-attributes --prefix "${package_name}/" | bzip2 > "${source_tarball}"

# Check tarball is valid
tar -tjf "${source_tarball}" > /dev/null

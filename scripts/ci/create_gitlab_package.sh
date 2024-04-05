#!/bin/sh
set -eu

### Create a GitLab package with raw binaries and tarballs

## Testing
# In the GitLab namespace 'nomadic-labs', if you want to iterate using the same tag
# you should manually delete any previously created package, otherwise it will
# reupload the files inside the same package, creating duplicates

# shellcheck source=./scripts/ci/octez-release.sh
. ./scripts/ci/octez-release.sh

# shellcheck source=./scripts/ci/create_release.inc.sh
. ./scripts/ci/create_release.inc.sh

# https://docs.gitlab.com/ee/user/packages/generic_packages/index.html#download-package-file
# :gitlab_api_url/projects/:id/packages/generic/:package_name/:package_version/:file_name
gitlab_octez_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_octez_binaries_package_name}/${gitlab_package_version}"
gitlab_octez_debian_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_octez_debian_package_name}/${gitlab_package_version}"
gitlab_octez_ubuntu_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_octez_ubuntu_package_name}/${gitlab_package_version}"
gitlab_octez_fedora_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_octez_fedora_package_name}/${gitlab_package_version}"
gitlab_octez_rockylinux_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_octez_rockylinux_package_name}/${gitlab_package_version}"
gitlab_octez_source_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_octez_source_package_name}/${gitlab_package_version}"

# Loop over architectures
for architecture in ${architectures}; do
  echo "Upload raw binaries (${architecture})"

  # Loop over binaries
  for binary in ${binaries}; do
    gitlab_upload "octez-binaries/${architecture}/${binary}" "${architecture}-${binary}" "${gitlab_octez_package_url}"
  done

  echo "Upload tarball with all binaries (${architecture})"

  mkdir -pv "octez-binaries/octez-${architecture}"
  cp -a octez-binaries/"${architecture}"/* "octez-binaries/octez-${architecture}/"

  cd octez-binaries/
  tar -czf "octez-${architecture}.tar.gz" "octez-${architecture}/"
  gitlab_upload "octez-${architecture}.tar.gz" "${gitlab_octez_binaries_package_name}-linux-${architecture}.tar.gz" "${gitlab_octez_package_url}"
  cd ..
done

echo "Upload debian packages"
for package in ${debian_packages}; do
  package_name="$(basename "${package}")"
  gitlab_upload "./${package}" "${package_name}" "${gitlab_octez_debian_package_url}"
done

echo "Upload Ubuntu packages"
for package in ${ubuntu_packages}; do
  package_name="$(basename "${package}")"
  gitlab_upload "./${package}" "${package_name}" "${gitlab_octez_ubuntu_package_url}"
done

echo "Upload Fedora packages"
for package in ${fedora_packages}; do
  package_name="$(basename "${package}")"
  gitlab_upload "./${package}" "${package_name}" "${gitlab_octez_fedora_package_url}"
done

echo "Upload Rocky Linux packages"
for package in ${rockylinux_packages}; do
  package_name="$(basename "${package}")"
  gitlab_upload "./${package}" "${package_name}" "${gitlab_octez_rockylinux_package_url}"
done

# Source code archives automatically published in a GitLab release do not have a static checksum,
# which is mandatory for the opam repository, because they are dynamically generated
# => create and upload manually
echo 'Upload tarball of source code and its checksums'

source_tarball="${gitlab_octez_source_package_name}.tar.bz2"

# We are using the export-subst feature of git configured in .gitattributes, requires git version >= 2.35
# https://git-scm.com/docs/git-archive
# https://git-scm.com/docs/gitattributes#_creating_an_archive
git --version
# Verify the placeholder %(describe:tags) is available
git describe --tags
# Pass '--worktree-attributes' to ensure that ignores written by restrict_export_to_octez_source.sh
# are respected.
git archive "${CI_COMMIT_TAG}" --format=tar --worktree-attributes --prefix "${gitlab_octez_source_package_name}/" | bzip2 > "${source_tarball}"

# Check tarball is valid
tar -tjf "${source_tarball}" > /dev/null

# Verify git expanded placeholders in archive
tar -Oxf "${source_tarball}" "${gitlab_octez_source_package_name}/src/lib_version/exe/get_git_info.ml" | grep "let raw_current_version = \"${CI_COMMIT_TAG}\""

# Checksums
sha256sum "${source_tarball}" > "${source_tarball}.sha256"
sha512sum "${source_tarball}" > "${source_tarball}.sha512"

gitlab_upload "${source_tarball}" "${source_tarball}" "${gitlab_octez_source_package_url}"
gitlab_upload "${source_tarball}.sha256" "${source_tarball}.sha256" "${gitlab_octez_source_package_url}"
gitlab_upload "${source_tarball}.sha512" "${source_tarball}.sha512" "${gitlab_octez_source_package_url}"

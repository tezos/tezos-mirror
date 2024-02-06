#!/bin/sh
set -eu

### Create a GitLab package with raw binaries and tarballs

## Testing
# In the GitLab namespace 'nomadic-labs', if you want to iterate using the same tag
# you should manually delete any previously created package, otherwise it will
# reupload the files inside the same package, creating duplicates

# shellcheck source=./scripts/ci/release.sh
. ./scripts/ci/release.sh

# https://docs.gitlab.com/ee/user/packages/generic_packages/index.html#download-package-file
# :gitlab_api_url/projects/:id/packages/generic/:package_name/:package_version/:file_name
gitlab_octez_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_octez_package_name}/${gitlab_package_version}"
gitlab_octez_deb_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_octez_deb_package_name}/${gitlab_package_version}"
gitlab_octez_rpm_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_octez_rpm_package_name}/${gitlab_package_version}"
gitlab_octez_source_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_octez_source_package_name}/${gitlab_package_version}"

gitlab_upload() {
  local_path="${1}"
  remote_file="${2}"
  url="${3-${gitlab_octez_package_url}}"
  echo "Upload to ${gitlab_octez_package_url}/${remote_file}"

  i=0
  max_attempts=10

  # Retry because gitlab.com is flaky sometimes, curl upload fails with http status code 524 (timeout)
  while [ "${i}" != "${max_attempts}" ]; do
    i=$((i + 1))
    http_code=$(curl -fsSL -o /dev/null -w "%{http_code}" \
      -H "JOB-TOKEN: ${CI_JOB_TOKEN}" \
      -T "${local_path}" \
      "${url}/${remote_file}")

    # Success
    [ "${http_code}" = '201' ] && return
    # Failure
    echo "Error: HTTP response code ${http_code}, expected 201"
    # Do not backoff after last attempt
    [ "${i}" = "${max_attempts}" ] && break
    # Backoff
    echo "Retry (${i}) in one minute..."
    sleep 60s
  done

  echo "Error: maximum attempts exhausted (${max_attempts})"
  exit 1
}

# Loop over architectures
for architecture in ${architectures}; do
  echo "Upload raw binaries (${architecture})"

  # Loop over binaries
  for binary in ${binaries}; do
    gitlab_upload "octez-binaries/${architecture}/${binary}" "${architecture}-${binary}"
  done

  echo "Upload tarball with all binaries (${architecture})"

  mkdir -pv "octez-binaries/octez-${architecture}"
  cp -a octez-binaries/"${architecture}"/* "octez-binaries/octez-${architecture}/"

  cd octez-binaries/
  tar -czf "octez-${architecture}.tar.gz" "octez-${architecture}/"
  gitlab_upload "octez-${architecture}.tar.gz" "${gitlab_octez_package_name}-linux-${architecture}.tar.gz"
  cd ..
done

echo "Upload debian packages"

# Loop over debian packages
for package in ${deb_packages}; do
  gitlab_upload "${package}" "${package}" "${gitlab_octez_deb_package_url}"
done

echo "Upload rpm packages"

# Loop over rpm packages
for package in ${rpm_packages}; do
  gitlab_upload "./${package}" "${package}" "${gitlab_octez_rpm_package_url}"
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
# Pass '--worktree-attributes' to ensure that ignores written by <NAME OF THE create_octez_source.sh SCRIPT if you rename it>
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

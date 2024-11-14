#!/bin/sh
set -eu

### Create a GitLab package with raw binaries and tarballs

## Testing
# In the GitLab namespace 'nomadic-labs', if you want to iterate using the same tag
# you should manually delete any previously created package, otherwise it will
# reupload the files inside the same package, creating duplicates

# shellcheck source=./scripts/ci/octez-release.sh
. ./scripts/ci/octez-release.sh

debian_bookworm_packages="$(find packages/debian/bookworm/ -maxdepth 1 -name octez-\*.deb 2> /dev/null || printf '')"
ubuntu_noble_packages="$(find packages/ubuntu/noble/ -maxdepth 1 -name octez-\*.deb 2> /dev/null || printf '')"
ubuntu_jammy_packages="$(find packages/ubuntu/jammy/ -maxdepth 1 -name octez-\*.deb 2> /dev/null || printf '')"
fedora_packages="$(find packages/fedora/39/ -maxdepth 1 -name octez-\*.rpm 2> /dev/null || printf '')"
rockylinux_packages="$(find packages/rockylinux/9.3/ -maxdepth 1 -name octez-\*.rpm 2> /dev/null || printf '')"

# https://docs.gitlab.com/ee/user/packages/generic_packages/index.html#download-package-file
# :gitlab_api_url/projects/:id/packages/generic/:package_name/:package_version/:file_name
gitlab_octez_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_octez_binaries_package_name}/${gitlab_package_version}"

gitlab_octez_debian_bookworm_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_octez_debian_bookworm_package_name}/${gitlab_package_version}"

gitlab_octez_ubuntu_noble_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_octez_ubuntu_noble_package_name}/${gitlab_package_version}"
gitlab_octez_ubuntu_jammy_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_octez_ubuntu_jammy_package_name}/${gitlab_package_version}"

gitlab_octez_fedora_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_octez_fedora_package_name}/${gitlab_package_version}"
gitlab_octez_rockylinux_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_octez_rockylinux_package_name}/${gitlab_package_version}"
gitlab_octez_source_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_octez_source_package_name}/${gitlab_package_version}"

gitlab_upload() {
  local_path="${1}"
  remote_file="${2}"
  url="${3-${gitlab_octez_package_url}}"
  echo "Upload to ${url}/${remote_file}"

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
  gitlab_upload "octez-${architecture}.tar.gz" "${gitlab_octez_binaries_package_name}-linux-${architecture}.tar.gz"
  cd ..
done

echo "Upload debian bookworm packages"
for package in ${debian_bookworm_packages}; do
  package_name="$(basename "${package}")"
  gitlab_upload "./${package}" "${package_name}" "${gitlab_octez_debian_bookworm_package_url}"
done

echo "Upload Ubuntu noble packages"
for package in ${ubuntu_noble_packages}; do
  package_name="$(basename "${package}")"
  gitlab_upload "./${package}" "${package_name}" "${gitlab_octez_ubuntu_noble_package_url}"
done

echo "Upload Ubuntu jammy packages"
for package in ${ubuntu_jammy_packages}; do
  package_name="$(basename "${package}")"
  gitlab_upload "./${package}" "${package_name}" "${gitlab_octez_ubuntu_jammy_package_url}"
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

# shellcheck source=./scripts/ci/create_octez_tarball.sh
. ./scripts/ci/create_octez_tarball.sh

# Verify git expanded placeholders in archive
tar -Oxf "${source_tarball}" "${gitlab_octez_source_package_name}/src/lib_version/exe/get_git_info.ml" | grep "let raw_current_version = \"${CI_COMMIT_TAG}\""

# Checksums
sha256sum "${source_tarball}" > "${source_tarball}.sha256"
sha512sum "${source_tarball}" > "${source_tarball}.sha512"

gitlab_upload "${source_tarball}" "${source_tarball}" "${gitlab_octez_source_package_url}"
gitlab_upload "${source_tarball}.sha256" "${source_tarball}.sha256" "${gitlab_octez_source_package_url}"
gitlab_upload "${source_tarball}.sha512" "${source_tarball}.sha512" "${gitlab_octez_source_package_url}"

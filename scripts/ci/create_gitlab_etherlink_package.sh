#!/bin/sh
set -eu

### Create a GitLab package with raw binaries and tarballs

## Testing
# In the GitLab namespace 'nomadic-labs', if you want to iterate using the same tag
# you should manually delete any previously created package, otherwise it will
# reupload the files inside the same package, creating duplicates

# shellcheck source=./scripts/ci/etherlink-release.sh
. ./scripts/ci/etherlink-release.sh

# https://docs.gitlab.com/ee/user/packages/generic_packages/index.html#download-package-file
# :gitlab_api_url/projects/:id/packages/generic/:package_name/:package_version/:file_name
gitlab_etherlink_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_etherlink_binaries_package_name}/${gitlab_package_version}"
gitlab_etherlink_debian_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_etherlink_debian_package_name}/${gitlab_package_version}"
gitlab_etherlink_ubuntu_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_etherlink_ubuntu_package_name}/${gitlab_package_version}"
gitlab_etherlink_fedora_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_etherlink_fedora_package_name}/${gitlab_package_version}"
gitlab_etherlink_rockylinux_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_etherlink_rockylinux_package_name}/${gitlab_package_version}"

gitlab_upload() {
  local_path="${1}"
  remote_file="${2}"
  url="${3-${gitlab_etherlink_package_url}}"
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

  mkdir -pv "octez-binaries/etherlink-${architecture}"
  cp -a octez-binaries/"${architecture}"/* "octez-binaries/etherlink-${architecture}/"

  cd octez-binaries/
  tar -czf "etherlink-${architecture}.tar.gz" "etherlink-${architecture}/"
  gitlab_upload "etherlink-${architecture}.tar.gz" "${gitlab_etherlink_binaries_package_name}-linux-${architecture}.tar.gz"
  cd ..
done

echo "Upload debian packages"
for package in ${debian_packages}; do
  package_name="$(basename "${package}")"
  gitlab_upload "./${package}" "${package_name}" "${gitlab_etherlink_debian_package_url}"
done

echo "Upload Ubuntu packages"
for package in ${ubuntu_packages}; do
  package_name="$(basename "${package}")"
  gitlab_upload "./${package}" "${package_name}" "${gitlab_etherlink_ubuntu_package_url}"
done

echo "Upload Fedora packages"
for package in ${fedora_packages}; do
  package_name="$(basename "${package}")"
  gitlab_upload "./${package}" "${package_name}" "${gitlab_etherlink_fedora_package_url}"
done

echo "Upload Rocky Linux packages"
for package in ${rockylinux_packages}; do
  package_name="$(basename "${package}")"
  gitlab_upload "./${package}" "${package_name}" "${gitlab_etherlink_rockylinux_package_url}"
done

# TODO (maybe): Source code archives automatically published in a GitLab release
# do not have a static checksum, which is mandatory for the opam repository,
# because they are dynamically generated
# => create and upload manually

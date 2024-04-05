#!/bin/sh
set -eu

### Create a GitLab Etherlink release with links to all the related resources

# shellcheck source=./scripts/ci/etherlink-release.sh
. ./scripts/ci/etherlink-release.sh

# shellcheck source=./scripts/ci/create_release.inc.sh
. ./scripts/ci/create_release.inc.sh

kernels_artifact_url="$CI_JOB_URL/artifacts/raw/kernels.tar.gz"

echo "Query GitLab to get generic package URL"

gitlab_binaries_url=$(package_web_path "${gitlab_package_version}" "${gitlab_etherlink_binaries_package_name}")
gitlab_debian_bookworm_packages_url=$(package_web_path "${gitlab_package_version}" "${gitlab_etherlink_debian_bookworm_package_name}")
gitlab_ubuntu_focal_packages_url=$(package_web_path "${gitlab_package_version}" "${gitlab_etherlink_ubuntu_focal_package_name}")
gitlab_ubuntu_jammy_packages_url=$(package_web_path "${gitlab_package_version}" "${gitlab_etherlink_ubuntu_jammy_package_name}")
gitlab_fedora_packages_url=$(package_web_path "${gitlab_package_version}" "${gitlab_etherlink_fedora_package_name}")
gitlab_rockylinux_packages_url=$(package_web_path "${gitlab_package_version}" "${gitlab_etherlink_rockylinux_package_name}")

# Enable release-cli verbose mode
export DEBUG='true'

release-cli create \
  --name="${gitlab_release_name}" \
  --tag-name="${CI_COMMIT_TAG}" \
  --assets-link="{\"name\":\"Kernels\",\"url\":\"${kernels_artifact_url}\"}" \
  --assets-link="{\"name\":\"Static binaries\",\"url\":\"${gitlab_binaries_url}\",\"link_type\":\"package\"}" \
  --assets-link="{\"name\":\"Debian Bookworm packages\",\"url\":\"${gitlab_debian_bookworm_packages_url}\",\"link_type\":\"package\"}" \
  --assets-link="{\"name\":\"Ubuntu Focal packages\",\"url\":\"${gitlab_ubuntu_focal_packages_url}\",\"link_type\":\"package\"}" \
  --assets-link="{\"name\":\"Ubuntu Jammy packages\",\"url\":\"${gitlab_ubuntu_jammy_packages_url}\",\"link_type\":\"package\"}" \
  --assets-link="{\"name\":\"Fedora packages\",\"url\":\"${gitlab_fedora_packages_url}\",\"link_type\":\"package\"}" \
  --assets-link="{\"name\":\"Rocky Linux packages\",\"url\":\"${gitlab_rockylinux_packages_url}\",\"link_type\":\"package\"}"

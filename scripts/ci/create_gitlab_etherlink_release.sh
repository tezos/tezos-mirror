#!/bin/sh
set -eu

### Create a GitLab Etherlink release with links to all the related resources

# shellcheck source=./scripts/ci/etherlink-release.sh
. ./scripts/ci/etherlink-release.sh

echo "Query GitLab to get generic package URL"

# https://docs.gitlab.com/ee/api/packages.html#within-a-project
# :gitlab_api_url/projects/:id/packages
package_web_path() {
  f_package_name="$1"
  ret=$(curl -fsSL -X GET \
    -H "JOB-TOKEN: ${CI_JOB_TOKEN}" \
    "${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages?sort=desc&package_name=${f_package_name}" |
    jq -r ".[] | select(.version==\"${gitlab_package_version}\") | ._links.web_path")

  if [ -z "${ret}" ]; then
    echo "Error: ${f_package_name} could not find package matching version ${gitlab_package_version}"
    exit 1
  else
    echo "https://${CI_SERVER_HOST}${ret}"
  fi
}

gitlab_binaries_url=$(package_web_path "${gitlab_etherlink_binaries_package_name}")
gitlab_etherlink_source_url=$(package_web_path "${gitlab_etherlink_source_package_name}")
gitlab_debian_packages_url=$(package_web_path "${gitlab_etherlink_debian_package_name}")
gitlab_ubuntu_packages_url=$(package_web_path "${gitlab_etherlink_ubuntu_package_name}")
gitlab_fedora_packages_url=$(package_web_path "${gitlab_etherlink_fedora_package_name}")
gitlab_rockylinux_packages_url=$(package_web_path "${gitlab_etherlink_rockylinux_package_name}")

# Enable release-cli verbose mode
export DEBUG='true'

release-cli create \
  --name="${gitlab_release_name}" \
  --tag-name="${CI_COMMIT_TAG}" \
  --assets-link="{\"name\":\"Static binaries\",\"url\":\"${gitlab_binaries_url}\",\"link_type\":\"package\"}" \
  --assets-link="{\"name\":\"Etherlink source\",\"url\":\"${gitlab_etherlink_source_url}\",\"link_type\":\"other\"}" \
  --assets-link="{\"name\":\"Debian packages\",\"url\":\"${gitlab_debian_packages_url}\",\"link_type\":\"package\"}" \
  --assets-link="{\"name\":\"Ubuntu packages\",\"url\":\"${gitlab_ubuntu_packages_url}\",\"link_type\":\"package\"}" \
  --assets-link="{\"name\":\"Fedora packages\",\"url\":\"${gitlab_fedora_packages_url}\",\"link_type\":\"package\"}" \
  --assets-link="{\"name\":\"Rocky Linux packages\",\"url\":\"${gitlab_rockylinux_packages_url}\",\"link_type\":\"package\"}"

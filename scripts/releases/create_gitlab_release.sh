#!/bin/sh
set -eu

### Create a GitLab release with links to all the related resources

## Testing
# In the GitLab namespace 'nomadic-labs', if you want to iterate using the same tag
# you should manually delete any previously created release, otherwise it will error

# shellcheck source=./scripts/releases/octez-release.sh
. ./scripts/releases/octez-release.sh

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

gitlab_binaries_url=$(package_web_path "${gitlab_octez_binaries_package_name}")
gitlab_octez_source_url=$(package_web_path "${gitlab_octez_source_package_name}")

if [ "${CI_PROJECT_NAMESPACE}" = "tezos" ]; then
  ## Production => Docker Hub
  docker_hub_path='tezos/tezos'
  echo "Query Docker Hub repository to get image URL at https://hub.docker.com/r/${docker_hub_path}"

  token=$(curl -fsSL "https://auth.docker.io/token?scope=repository:${docker_hub_path}:pull&service=registry.docker.io" | jq -r '.token')
  docker_image_digest=$(curl -fsSL -X GET \
    -H 'Accept: application/vnd.docker.distribution.manifest.list.v2+json' \
    -H "Authorization: Bearer ${token}" \
    "https://registry-1.docker.io/v2/${docker_hub_path}/manifests/${CI_COMMIT_TAG}" |
    jq -r '.manifests | .[0] | .digest | split(":")[1]')

  if [ -z "${docker_image_digest}" ]; then
    echo "Error: could not find Docker Hub image matching tag ${CI_COMMIT_TAG}"
    exit 1
  else
    docker_image_url="https://hub.docker.com/layers/${docker_hub_path}/${CI_COMMIT_TAG}/images/sha256-${docker_image_digest}"
  fi
else
  ## Testing in namespace 'nomadic-labs' => GitLab container registry
  echo "Testing: link to GitLab container registry search"
  docker_image_url="https://gitlab.com/nomadic-labs/tezos/container_registry/421631?search[]=${CI_COMMIT_TAG}"
fi

# GitLab Release command-line tool
# https://gitlab.com/gitlab-org/release-cli

# Enable release-cli verbose mode
export DEBUG='true'

deprecated_release_name="${gitlab_release_name} - GitLab Releases Deprecated, See octez.tezos.com/releases/"

release-cli create \
  --name="${deprecated_release_name}" \
  --tag-name="${CI_COMMIT_TAG}" \
  --description="⚠️ **DEPRECATION NOTICE** ⚠️

This GitLab release page is deprecated. Please use our new dedicated release page for the latest releases and information.

🔗 **New Release Page: https://octez.tezos.com/releases/**

This page will no longer be updated. All future releases and updates will be available on the new release page." \
  --assets-link="{\"name\":\"NEW RELEASE PAGE (Use This Instead)\",\"url\":\"https://octez.tezos.com/releases/\",\"link_type\":\"other\"}" \
  --assets-link="{\"name\":\"Changelog\",\"url\":\"https://octez.tezos.com/docs/CHANGES.html#version-${gitlab_release_no_dot}\",\"link_type\":\"other\"}" \
  --assets-link="{\"name\":\"Announcement\",\"url\":\"https://octez.tezos.com/docs/releases/version-${gitlab_release_major_version}.html\",\"link_type\":\"other\"}" \
  --assets-link="{\"name\":\"Docker image\",\"url\":\"${docker_image_url}\",\"link_type\":\"image\"}" \
  --assets-link="{\"name\":\"Static binaries\",\"url\":\"${gitlab_binaries_url}\",\"link_type\":\"package\"}" \
  --assets-link="{\"name\":\"Octez source\",\"url\":\"${gitlab_octez_source_url}\",\"link_type\":\"other\"}"

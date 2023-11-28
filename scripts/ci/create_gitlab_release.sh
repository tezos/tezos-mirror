#!/bin/sh
set -eu

### Create a GitLab release with links to all the related resources

## Testing
# In the GitLab namespace 'nomadic-labs', if you want to iterate using the same tag
# you should manually delete any previously created release, otherwise it will error

# shellcheck source=./scripts/ci/release.sh
. ./scripts/ci/release.sh

echo "Query GitLab to get generic package URL"

# https://docs.gitlab.com/ee/api/packages.html#within-a-project
# :gitlab_api_url/projects/:id/packages
web_path=$(curl -fsSL -X GET \
                -H "JOB-TOKEN: ${CI_JOB_TOKEN}" \
                "${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages?sort=desc&package_name=${gitlab_octez_package_name}" \
           | jq -r ".[] | select(.version==\"${gitlab_package_version}\") | ._links.web_path")

deb_web_path=$(curl -fsSL -X GET \
                    -H "JOB-TOKEN: ${CI_JOB_TOKEN}" \
                    "${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages?sort=desc&package_name=${gitlab_octez_deb_package_name}" \
           | jq -r ".[] | select(.version==\"${gitlab_package_version}\") | ._links.web_path")

rpm_web_path=$(curl -fsSL -X GET \
                    -H "JOB-TOKEN: ${CI_JOB_TOKEN}" \
                    "${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages?sort=desc&package_name=${gitlab_octez_rpm_package_name}" \
           | jq -r ".[] | select(.version==\"${gitlab_package_version}\") | ._links.web_path")

if [ -z "${web_path}" ]
then
  echo "Error: could not find package matching version ${gitlab_package_version}"
  exit 1
else
  gitlab_binaries_url="https://${CI_SERVER_HOST}${web_path}"
fi

if [ -z "${deb_web_path}" ]
then
  echo "Error: could not find debian package matching version ${gitlab_package_version}"
  exit 1
else
  gitlab_deb_packages_url="https://${CI_SERVER_HOST}${deb_web_path}"
fi

if [ -z "${rpm_web_path}" ]
then
  echo "Error: could not find rpm package matching version ${gitlab_package_version}"
  exit 1
else
  gitlab_rpm_packages_url="https://${CI_SERVER_HOST}${rpm_web_path}"
fi

if [ "${CI_PROJECT_NAMESPACE}" = "tezos" ]
then
  ## Production => Docker Hub
  docker_hub_path='tezos/tezos'
  echo "Query Docker Hub repository to get image URL at https://hub.docker.com/r/${docker_hub_path}"

  token=$(curl -fsSL "https://auth.docker.io/token?scope=repository:${docker_hub_path}:pull&service=registry.docker.io"  | jq -r '.token')
  docker_image_digest=$(curl -fsSL -X GET \
                             -H 'Accept: application/vnd.docker.distribution.manifest.list.v2+json' \
                             -H "Authorization: Bearer ${token}" \
                             "https://registry-1.docker.io/v2/${docker_hub_path}/manifests/${CI_COMMIT_TAG}" \
                        | jq -r '.manifests | .[0] | .digest | split(":")[1]')

  if [ -z "${docker_image_digest}" ]
  then
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

release-cli create \
  --name="${gitlab_release_name}" \
  --tag-name="${CI_COMMIT_TAG}" \
  --assets-link="{\"name\":\"Changelog\",\"url\":\"https://tezos.gitlab.io/CHANGES.html#version-${gitlab_release_no_dot}\",\"link_type\":\"other\"}" \
  --assets-link="{\"name\":\"Announcement\",\"url\":\"https://tezos.gitlab.io/releases/version-${gitlab_release_major_version}.html\",\"link_type\":\"other\"}" \
  --assets-link="{\"name\":\"Docker image\",\"url\":\"${docker_image_url}\",\"link_type\":\"image\"}" \
  --assets-link="{\"name\":\"Static binaries\",\"url\":\"${gitlab_binaries_url}\",\"link_type\":\"package\"}" \
  --assets-link="{\"name\":\"Debian packages\",\"url\":\"${gitlab_deb_packages_url}\",\"link_type\":\"package\"}" \
  --assets-link="{\"name\":\"Red Hat packages\",\"url\":\"${gitlab_rpm_packages_url}\",\"link_type\":\"package\"}"

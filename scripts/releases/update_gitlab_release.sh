#!/bin/sh
set -eu

### Update an existing GitLab release with new static binaries from packaging revision
### e.g., update release v24.0 with binaries from v24.0-1

# shellcheck source=./scripts/releases/octez-release.sh
. ./scripts/releases/octez-release.sh

# Validate we have a packaging revision (not a regular release)
if [ -z "${gitlab_packaging_revision_version}" ]; then
  echo "Error: Not a packaging revision. This script is for updating releases with packaging revisions (octez-vX.Y-N)"
  exit 1
fi

# Build the release version tag from major.minor without revision
release_version="octez-v${gitlab_release_major_version}.${gitlab_release_minor_version}"

echo "### Updating GitLab release '${release_version}' with binaries from '${CI_COMMIT_TAG}'"
echo "### Packaging revision: ${gitlab_packaging_revision_version}"

echo "Query GitLab to get generic package URL for packaging revision"
# Function to get package web path (copied from scripts/releases/create_gitlab_release.sh)
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

echo "New static binaries URL: ${gitlab_binaries_url}"

echo "Checking if release '${release_version}' exists..."
existing_release=$(curl -fsSL -X GET \
  -H "JOB-TOKEN: ${CI_JOB_TOKEN}" \
  "${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/releases/${release_version}" ||
  echo "null")

if [ "${existing_release}" = "null" ]; then
  echo "Error: Release '${release_version}' does not exist. Cannot update non-existing release."
  echo "Please create the release first using the standard release pipeline."
  exit 1
fi

echo "Release '${release_version}' exists. Updating with new static binaries..."

current_assets=$(echo "${existing_release}" | jq -c '.assets.links')

echo "Current assets: ${current_assets}"

# Remove old static binaries link and add new one
updated_assets=$(echo "${current_assets}" | jq --arg new_url "${gitlab_binaries_url}" '
  map(select(.name != "Static binaries")) +
  [{"name": "Static binaries", "url": $new_url, "link_type": "package"}]
')

echo "Updated assets: ${updated_assets}"

# Update the release with new assets
echo "Updating release assets..."
curl -fsSL -X PUT \
  -H "JOB-TOKEN: ${CI_JOB_TOKEN}" \
  -H "Content-Type: application/json" \
  "${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/releases/${release_version}" \
  --data "$(jq -n --argjson assets "$updated_assets" '{
    "assets": {
      "links": $assets
    }
  }')"

echo "### Successfully updated release '${release_version}' with new static binaries from '${CI_COMMIT_TAG}'"

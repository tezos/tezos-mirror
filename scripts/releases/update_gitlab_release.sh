#!/bin/sh
set -eu

### Update an existing GitLab release with new static binaries from packaging revision
### e.g., update release v24.0 with binaries from v24.0-1

# shellcheck source=./scripts/releases/octez-release.sh
. ./scripts/releases/octez-release.sh

# Validate we are not on an RC version (RC versions cannot have packaging revisions)
if [ -n "${gitlab_release_rc_version}" ]; then
  echo "Error: RC versions cannot have packaging revisions (octez-vX.Y-rcZ)."
  exit 1
fi

# Validate we are not on a beta version (beta versions cannot have packaging revisions)
if [ -n "${gitlab_release_beta_version}" ]; then
  echo "Error: Beta versions cannot have packaging revisions (octez-vX.Y-betaZ)."
  exit 1
fi

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
gitlab_binaries_url=$(package_web_path "${gitlab_octez_binaries_package_name}")

echo "New static binaries URL (from gitlab:create_package): ${gitlab_binaries_url}"

echo "Checking if release '${release_version}' exists..."
gitlab_release=$(curl -fsSL -X GET \
  -H "JOB-TOKEN: ${CI_JOB_TOKEN}" \
  "${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/releases/${release_version}" ||
  echo "null")

if [ "${gitlab_release}" = "null" ]; then
  echo "Error: Release '${release_version}' does not exist. Cannot update non-existing release."
  echo "Please create the release first using the standard release pipeline."
  exit 1
fi

echo "Release '${release_version}' exists. Updating with new static binaries..."

# Get the current release links to find the old static binaries link ID
current_assets=$(echo "${gitlab_release}" | jq '.assets.links')
echo "Current assets: ${current_assets}"

# Find the ID of the existing "Static binaries" link
old_static_binaries_id=$(echo "${current_assets}" | jq -r '.[] | select(.name == "Static binaries") | .id')

if [ -n "${old_static_binaries_id}" ] && [ "${old_static_binaries_id}" != "null" ]; then
  echo "Removing old static binaries link (ID: ${old_static_binaries_id})..."
  curl -fsSL -X DELETE \
    -H "JOB-TOKEN: ${CI_JOB_TOKEN}" \
    "${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/releases/${release_version}/assets/links/${old_static_binaries_id}"
  echo ''
fi

echo "Adding new static binaries link: ${gitlab_binaries_url}"
curl -fsSL -X POST \
  -H "JOB-TOKEN: ${CI_JOB_TOKEN}" \
  -H "Content-Type: application/json" \
  "${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/releases/${release_version}/assets/links" \
  --data "{\"name\":\"Static binaries\",\"url\":\"${gitlab_binaries_url}\",\"link_type\":\"package\"}"
echo ''

echo "### Successfully updated release '${release_version}' with new static binaries from '${CI_COMMIT_TAG}'"

#!/bin/sh

### Create a GitLab release with links to all the related resources

if [ -z "${CI_COMMIT_TAG:-}" ]; then
  echo "CI_COMMIT_TAG is not set, impossible to create a gitlab release page."
  exit 1
fi

# shellcheck source=./grafazos/scripts/releases/release.sh
. ./grafazos/scripts/releases/release.sh

set -eu

# https://docs.gitlab.com/ee/user/packages/generic_packages/index.html#download-package-file
# :gitlab_api_url/projects/:id/packages/generic/:package_name/:package_version/:file_name
gitlab_dashboards_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_dashboards_package_name}/${release_no_v}"
gitlab_source_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_source_package_name}/${release_no_v}"
# Copied from `scripts/ci/create_gitlab_package.sh`.
gitlab_upload() {
  local_path="${1}"
  remote_file="${2-${local_path}}"
  url="${3:-${gitlab_source_url}}"

  # Upload only if not running in dry-run
  if [ -z "${dry_run:-}" ]; then

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
  else
    echo "The following file would be uploaded if not running in dry-run mode: ${url}/${remote_file}"
  fi
}

dashboards="$(find grafazos/output/ -type f -name "*.json")"

echo "Upload Grafazos Dashboards"
for dashboard in ${dashboards}; do
  gitlab_upload "${dashboard}" "$(basename "${dashboard}")" "${gitlab_dashboards_url}"
done

# Create and upload a tarball for Grafazos source
echo "Upload Grafazos source"
git archive HEAD --format=tar "grafazos/" | bzip2 > "${source_tarball}"

# Checksums
sha256sum "${source_tarball}" > "${source_tarball}.sha256"
sha512sum "${source_tarball}" > "${source_tarball}.sha512"

gitlab_upload "${source_tarball}" "${source_tarball}"
gitlab_upload "${source_tarball}.sha256" "${source_tarball}.sha256"
gitlab_upload "${source_tarball}.sha512" "${source_tarball}.sha512"

# Get gitlab packages URL
ret_dashboards=$(curl -fsSL -X GET \
  -H "JOB-TOKEN: ${CI_JOB_TOKEN}" \
  "${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages?sort=desc&package_name=${gitlab_dashboards_package_name}" |
  jq -r ".[] | select(.version==\"${release_no_v}\") | ._links.web_path")
ret_source=$(curl -fsSL -X GET \
  -H "JOB-TOKEN: ${CI_JOB_TOKEN}" \
  "${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages?sort=desc&package_name=${gitlab_source_package_name}" |
  jq -r ".[] | select(.version==\"${release_no_v}\") | ._links.web_path")

if [ -z "${ret_dashboards}" ]; then
  echo "Error: ${gitlab_dashboards_package_name} could not find package matching version ${release_no_v}"
  exit 1
elif [ -z "${ret_source}" ]; then
  echo "Error: ${gitlab_source_package_name} could not find package matching version ${release_no_v}"
  exit 1
else
  dashboards_url="https://${CI_SERVER_HOST}${ret_dashboards}"
  source_url="https://${CI_SERVER_HOST}${ret_source}"
fi

# GitLab Release command-line tool
# https://gitlab.com/gitlab-org/release-cli

# Enable release-cli verbose mode
export DEBUG='true'

deprecated_release_name="${release_name} - GitLab Releases Deprecated, See octez.tezos.com/releases/grafazos/"

release-cli create \
  --name="${deprecated_release_name}" \
  --tag-name="${CI_COMMIT_TAG}" \
  --description="⚠️ **DEPRECATION NOTICE** ⚠️

This GitLab release page is deprecated. Please use our new dedicated release page for the latest releases and information.

🔗 **New Release Page: https://octez.tezos.com/releases/grafazos/**

This page will no longer be updated. All future releases and updates will be available on the new release page." \
  --assets-link="{\"name\":\"NEW RELEASE PAGE (Use This Instead)\",\"url\":\"https://octez.tezos.com/releases/\",\"link_type\":\"other\"}" \
  --assets-link="{\"name\":\"Dashboards\",\"url\":\"${dashboards_url}\",\"link_type\":\"package\"}" \
  --assets-link="{\"name\":\"Grafazos source\",\"url\":\"${source_url}\",\"link_type\":\"other\"}"

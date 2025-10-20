#!/bin/sh

### Create a GitLab release with links to all the related resources

if [ -z "${CI_COMMIT_TAG:-}" ]; then
  echo "CI_COMMIT_TAG is not set, impossible to create a gitlab release page."
  exit 1
fi

# shellcheck source=./teztale/scripts/releases/release.sh
. ./teztale/scripts/releases/release.sh

set -eu

# https://docs.gitlab.com/ee/user/packages/generic_packages/index.html#download-package-file
# :gitlab_api_url/projects/:id/packages/generic/:package_name/:package_version/:file_name
url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_package_name}/${release_no_v}"

# Copied from `scripts/ci/create_gitlab_package.sh`.
gitlab_upload() {
  local_path="${1}"
  remote_file="${2}"

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

# Loop over architectures
for architecture in ${architectures}; do
  echo "Upload raw binaries (${architecture})"

  # Loop over binaries
  for binary in ${binaries}; do
    gitlab_upload "teztale-binaries/${architecture}/${binary}" "${architecture}-${binary}"
  done

  echo "Upload tarball with all binaries (${architecture})"

  mkdir -pv "teztale-binaries/teztale-${architecture}"
  cp -a teztale-binaries/"${architecture}"/* "teztale-binaries/teztale-${architecture}/"

  cd teztale-binaries/
  tar -czf "teztale-${architecture}.tar.gz" "teztale-${architecture}/"
  gitlab_upload "teztale-${architecture}.tar.gz" "${gitlab_package_name}-linux-${architecture}.tar.gz"
  cd ..
done

ret=$(curl -fsSL -X GET \
  -H "JOB-TOKEN: ${CI_JOB_TOKEN}" \
  "${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages?sort=desc&package_name=${gitlab_package_name}" |
  jq -r ".[] | select(.version==\"${release_no_v}\") | ._links.web_path")

if [ -z "${ret}" ]; then
  echo "Error: ${gitlab_package_name} could not find package matching version ${release_no_v}"
  exit 1
else
  binaries_url="https://${CI_SERVER_HOST}${ret}"
fi

# GitLab Release command-line tool
# https://gitlab.com/gitlab-org/release-cli

# Enable release-cli verbose mode
export DEBUG='true'

deprecated_release_name="${release_name} - GitLab Releases Deprecated, See octez.tezos.com/releases/teaztale/"

release-cli create \
  --name="${deprecated_release_name}" \
  --tag-name="${CI_COMMIT_TAG}" \
  --description="⚠️ **DEPRECATION NOTICE** ⚠️

This GitLab release page is deprecated. Please use our new dedicated release page for the latest releases and information.

🔗 **New Release Page: https://octez.tezos.com/releases/teztale/**

This page will no longer be updated. All future releases and updates will be available on the new release page." \
  --assets-link="{\"name\":\"NEW RELEASE PAGE (Use This Instead)\",\"url\":\"https://octez.tezos.com/releases/\",\"link_type\":\"other\"}" \
  --assets-link="{\"name\":\"Static binaries\",\"url\":\"${binaries_url}\",\"link_type\":\"package\"}"

#!/bin/ash
set -eux

### Create a GitLab package with the generated JSON files

gitlab_package_name="${CI_PROJECT_NAME}-${CI_COMMIT_TAG}"
gitlab_package_version="${CI_COMMIT_TAG}"

# https://docs.gitlab.com/ee/user/packages/generic_packages/index.html#download-package-file
# :gitlab_api_url/projects/:id/packages/generic/:package_name/:package_version/:file_name
gitlab_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_package_name}/${gitlab_package_version}"

gitlab_upload() {
  local_path="${1}"
  remote_file="${2}"
  echo "Upload to ${gitlab_package_url}/${remote_file}"

  i=0
  max_attempts=10

  # Retry because gitlab.com is flaky sometimes, curl upload fails with http status code 524 (timeout)
  while [ "${i}" != "${max_attempts}" ]
  do
    i=$((i + 1))
    http_code=$(curl -fsSL -o /dev/null -w "%{http_code}" \
                     -H "JOB-TOKEN: ${CI_JOB_TOKEN}" \
                     -T "${local_path}" \
                     "${gitlab_package_url}/${remote_file}")

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

# Target directory (checkout Makefile)
cd output/

# Upload all JSON files
for file in *.json
do
  gitlab_upload "${file}" "${file}"
done

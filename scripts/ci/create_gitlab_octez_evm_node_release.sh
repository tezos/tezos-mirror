#!/bin/sh
set -eu

### Create a GitLab Octez EVM Node release with links to static binaries

# In case of failures with this pipeline, contact the maintainer.
# Maintainer: Thomas Letan <lthms@nomadic-labs.com>

# shellcheck source=./scripts/ci/octez-evm-node-release.sh
. ./scripts/ci/octez-evm-node-release.sh
gitlab_octez_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_octez_binaries_package_name}/${gitlab_package_version}"

# Copied from `create_gitlab_release.sh`.
# If the pipeline is broken because of uploads, a safe bet is to update this
# function with its latest version in the original file.
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

# Copied from `create_gitlab_package.sh`.
# If the pipeline is broken because of uploads, a safe bet is to update this
# function with its latest version in the original file.
gitlab_upload() {
  local_path="${1}"
  remote_file="${2}"
  url="${3-${gitlab_octez_package_url}}"
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

gen_sha512sum() {
  sha512sum "$1" | cut -d " " -f 1 > "$1".sha512
}

echo "Create GitLab package"

# We only support one OS for now
os="linux"

# Loop over architectures
for architecture in ${architectures}; do
  echo "Upload raw binariy (${architecture})"

  gitlab_upload \
    "octez-binaries/${architecture}/octez-evm-node" \
    "$os-${architecture}-octez-evm-node"

  echo "Upload tarball (${architecture})"

  mkdir -pv "octez-binaries/octez-evm-node-$os-${architecture}"
  cp -a \
    octez-binaries/"${architecture}"/* \
    "octez-binaries/octez-evm-node-$os-${architecture}/"

  cd octez-binaries/
  tar -czf \
    "octez-evm-node-$os-${architecture}.tar.gz" \
    "octez-evm-node-$os-${architecture}/"
  gitlab_upload \
    "octez-evm-node-$os-${architecture}.tar.gz" \
    "${gitlab_octez_binaries_package_name}-$os-${architecture}.tar.gz"
  cd ..

  echo "Upload checksum"

  gen_sha512sum "octez-binaries/${architecture}/octez-evm-node"
  gitlab_upload \
    "octez-binaries/${architecture}/octez-evm-node.sha512" \
    "$os-${architecture}-octez-evm-node.sha512"
done

echo "Query GitLab to get generic package URL"

gitlab_binaries_url=$(package_web_path "${gitlab_octez_binaries_package_name}")

echo "Release"

release-cli create \
  --name="${gitlab_release_name}" \
  --tag-name="${CI_COMMIT_TAG}" \
  --assets-link="{\"name\":\"Static binaries\",\"url\":\"${gitlab_binaries_url}\",\"link_type\":\"package\"}"

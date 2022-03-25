#!/bin/sh
set -eu

### Create a GitLab package with raw binaries and tarballs

## Testing
# In the GitLab namespace 'nomadic-labs', if you want to iterate using the same tag
# you should manually delete any previously created package, otherwise it will
# reupload the files inside the same package, creating duplicates

# shellcheck source=./scripts/ci/release.sh
. ./scripts/ci/release.sh

# X.Y or X.Y-rcZ
gitlab_package_name="${gitlab_release_no_v}"

# https://docs.gitlab.com/ee/user/packages/generic_packages/index.html#download-package-file
# :gitlab_api_url/projects/:id/packages/generic/:package_name/:package_version/:file_name
gitlab_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${CI_PROJECT_NAME}/${gitlab_package_name}"

gitlab_upload() {
  local_path="${1}"
  remote_file="${2}"
  echo "Upload to ${gitlab_package_url}/${remote_file}"

  http_code=$(curl -fsSL -o /dev/null -w "%{http_code}" \
                   -H "JOB-TOKEN: ${CI_JOB_TOKEN}" \
                   -T "${local_path}" \
                   "${gitlab_package_url}/${remote_file}")

  if [ "${http_code}" != '201' ]
  then
    echo "Error: HTTP response code ${http_code}, expected 201"
    exit 1
  fi
}

# Loop over architectures
for architecture in ${architectures}
do
  echo "Upload raw binaries (${architecture})"

  # Loop over binaries
  for binary in ${binaries}
  do
    gitlab_upload "tezos-binaries/${architecture}/${binary}" "${architecture}-${binary}"
  done

  echo "Upload tarball with all binaries (${architecture})"

  mkdir -pv "tezos-binaries/tezos-${architecture}"
  cp -a tezos-binaries/"${architecture}"/* "tezos-binaries/tezos-${architecture}/"

  cd tezos-binaries/
  tar -czf "tezos-${architecture}.tar.gz" "tezos-${architecture}/"
  gitlab_upload "tezos-${architecture}.tar.gz" "tezos-${gitlab_package_name}-linux-${architecture}.tar.gz"
  cd ..
done

# Source code archives automatically published in a GitLab release do not have a static checksum,
# which is mandatory for the opam repository, because they are dynamically generated
# => create and upload manually
echo 'Upload tarball of source code and its checksums'

source_tarball="tezos-${gitlab_package_name}.tar.bz2"

# We are using the export-subst feature of git onfigured in .gitattributes, requires git version >= 2.35
# https://git-scm.com/docs/git-archive
# https://git-scm.com/docs/gitattributes#_creating_an_archive
git --version
# Verify the placeholder %(describe:tags) is available
git describe --tags
# Create tarball
git archive "${CI_COMMIT_TAG}" --format=tar | bzip2 > "${source_tarball}"

# Check tarball is valid
tar -tjf "${source_tarball}" > /dev/null

# Verify git expanded placeholders in archive
tar -Oxf "${source_tarball}" src/lib_version/exe/get_git_info.ml | grep "let raw_current_version = \"${CI_COMMIT_TAG}\""

# Checksums
sha256sum "${source_tarball}" > "${source_tarball}.sha256"
sha512sum "${source_tarball}" > "${source_tarball}.sha512"

gitlab_upload "${source_tarball}" "${source_tarball}"
gitlab_upload "${source_tarball}.sha256" "${source_tarball}.sha256"
gitlab_upload "${source_tarball}.sha512" "${source_tarball}.sha512"

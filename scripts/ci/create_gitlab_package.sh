#!/bin/sh

### Create a GitLab package with raw binaries and tarballs

## Testing
# In the GitLab namespace 'nomadic-labs', if you want to iterate using the same tag
# you should manually delete any previously created package, otherwise it will
# reupload the files inside the same package, creating duplicates

# expected env vars
# - architectures
# - GCP_LINUX_PACKAGES_BUCKET

# Env vars set by scripts/ci/repository-keys.sh
# - GPG_DUAL_SIGNING
# - GPG_KEY_ID
# - GPG_KEY_ID_BIS
# - GPG_PASSPHRASE
# - GPG_PASSPHRASE_BIS
# - GPG_PUBLIC_KEY

set -eu

ROOT_DIR="$(pwd)"

# shellcheck source=./scripts/releases/octez-release.sh
. ./scripts/releases/octez-release.sh

# Checks if running in dry-mode
for arg in "$@"; do
  case $arg in
  "--dry-run")
    dry_run="--dry-run"
    echo "Running in dry-run mode. Nothing will be uploaded."
    ;;
  esac
done

# Retrieve and import GPG keys (keyring is ready after this)
. ./scripts/ci/repository-keys.sh

# https://docs.gitlab.com/ee/user/packages/generic_packages/index.html#download-package-file
# :gitlab_api_url/projects/:id/packages/generic/:package_name/:package_version/:file_name
gitlab_octez_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_octez_binaries_package_name}/${gitlab_package_version}"
gitlab_octez_source_package_url="${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/packages/generic/${gitlab_octez_source_package_name}/${gitlab_package_version}"

gitlab_upload() {
  local_path="${1}"
  remote_file="${2}"
  url="${3-${gitlab_octez_package_url}}"

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

# Signs a file with a detached GPG signature, optionally appending a second
# signature if GPG_DUAL_SIGNING is true, then verifies the result.
# The signature is written to <input_file>.sig.
# File paths are relative to the caller's current directory.
# Usage: gpg_detach_sign_and_verify <input_file> <label>
gpg_detach_sign_and_verify() {
  input_file="$1"
  label="$2"
  output_sig="${input_file}.sig"

  echo "Signing: ${input_file}"
  echo "Using key: $GPG_KEY_ID"
  echo "Output: ${output_sig}"
  echo "$GPG_PASSPHRASE" |
    gpg --batch --passphrase-fd 0 --pinentry-mode loopback \
      -u "$GPG_KEY_ID" \
      -o "${output_sig}" \
      --detach-sign "${input_file}"

  echo "GPG dual signing: $GPG_DUAL_SIGNING"
  if [ "$GPG_DUAL_SIGNING" = "true" ]; then
    echo "Signing: ${input_file}"
    echo "Using key: $GPG_KEY_ID_BIS"
    echo "Output: ${output_sig}.bis"
    echo "$GPG_PASSPHRASE_BIS" |
      gpg --batch --passphrase-fd 0 --pinentry-mode loopback \
        -u "$GPG_KEY_ID_BIS" \
        -o "${output_sig}.bis" \
        --detach-sign "${input_file}"

    echo "Appending ${output_sig}.bis to ${output_sig}"
    cat "${output_sig}.bis" >> "${output_sig}"
    rm "${output_sig}.bis"
  fi

  # Verify GPG signatures against the exported public key
  "${ROOT_DIR}/scripts/ci/verify_gpg_signature.sh" "${output_sig}" "${input_file}" "${ROOT_DIR}/public/octez.asc" "${label}"
}

# Create the repository root directory and copy the public key
mkdir -p public
echo "$GPG_PUBLIC_KEY" > "public/octez.asc"

# If it's a protected branch the value of $BUCKET will
# be set accordingly by the CI.
BUCKET="$GCP_LINUX_PACKAGES_BUCKET"

./scripts/ci/gcp_auth.sh
GOOGLE_OAUTH_ACCESS_TOKEN=$(gcloud auth print-access-token)
export GOOGLE_OAUTH_ACCESS_TOKEN

echo "Push to $BUCKET"
# Upload only if not running in dry-run
if [ -z "${dry_run:-}" ]; then
  gsutil -m cp -r public/octez.asc gs://"${BUCKET}"
fi

# Loop over architectures
for architecture in ${architectures}; do
  echo "Upload raw binaries (${architecture})"

  # Loop over binaries
  for binary in ${binaries}; do

    gpg_detach_sign_and_verify "octez-binaries/${architecture}/${binary}" "Octez-binary signature"

    gitlab_upload "octez-binaries/${architecture}/${binary}" "${architecture}-${binary}"
    gitlab_upload "octez-binaries/${architecture}/${binary}.sig" "${architecture}-${binary}.sig"

  done

  echo "Upload tarball with all binaries (${architecture})"

  mkdir -pv "octez-binaries/octez-${architecture}"
  cp -a octez-binaries/"${architecture}"/* "octez-binaries/octez-${architecture}/"

  cd octez-binaries/
  tar -czf "octez-${architecture}.tar.gz" "octez-${architecture}/"

  gpg_detach_sign_and_verify "octez-${architecture}.tar.gz" "Octez-tarball signature"

  gitlab_upload "octez-${architecture}.tar.gz" "${gitlab_octez_binaries_package_name}-linux-${architecture}.tar.gz"
  gitlab_upload "octez-${architecture}.tar.gz.sig" "${gitlab_octez_binaries_package_name}-linux-${architecture}.tar.gz.sig"

  cd ..
done

# Source code archives automatically published in a GitLab release do not have a static checksum,
# which is mandatory for the opam repository, because they are dynamically generated
# => create and upload manually
echo 'Upload tarball of source code and its checksums'

source_tarball="${gitlab_octez_source_package_name}.tar.bz2"

# We are using the export-subst feature of git configured in .gitattributes, requires git version >= 2.35
# https://git-scm.com/docs/git-archive
# https://git-scm.com/docs/gitattributes#_creating_an_archive
git --version
# Verify the placeholder %(describe:tags) is available
git describe --tags

# shellcheck source=./scripts/ci/create_octez_tarball.sh
./scripts/ci/create_octez_tarball.sh

# Verify git expanded placeholders in archive
tar -Oxf "${source_tarball}" "${gitlab_octez_source_package_name}/src/lib_version/exe/get_git_info.ml" | grep "let raw_current_version = \"${CI_COMMIT_TAG}\""

# Checksums
sha256sum "${source_tarball}" > "${source_tarball}.sha256"
sha512sum "${source_tarball}" > "${source_tarball}.sha512"

gpg_detach_sign_and_verify "${source_tarball}" "Octez-source-tarball signature"

gitlab_upload "${source_tarball}" "${source_tarball}" "${gitlab_octez_source_package_url}"
gitlab_upload "${source_tarball}.sha256" "${source_tarball}.sha256" "${gitlab_octez_source_package_url}"
gitlab_upload "${source_tarball}.sha512" "${source_tarball}.sha512" "${gitlab_octez_source_package_url}"
gitlab_upload "${source_tarball}.sig" "${source_tarball}.sig" "${gitlab_octez_source_package_url}"

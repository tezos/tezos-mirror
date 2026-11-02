#!/bin/sh
set -eu

### Create or update a GitLab release with links to all the related resources
###
### Usage: create_gitlab_release.sh [--dry-run]
###
### The links point to resources published by other jobs (GitLab generic
### packages and Docker images), which must thus have been published first.
### If the release already exists, its links are replaced, so this script
### can be run again, for instance after the missing resources are published.
###
### For packaging revisions (octez-vX.Y-N), no new release is created:
### the static binaries link of the release octez-vX.Y is updated instead.
###
### With --dry-run, print the release that would be created or updated,
### without creating or updating anything.

dry_run=false
for arg in "$@"; do
  case "${arg}" in
  --dry-run)
    dry_run=true
    echo "Running in dry-run mode. No release will be created or updated."
    ;;
  *)
    echo "Unknown argument: ${arg}" >&2
    exit 1
    ;;
  esac
done

# shellcheck source=./scripts/releases/octez-release.sh
. ./scripts/releases/octez-release.sh

if [ -z "${gitlab_release}" ]; then
  echo "Error: '${CI_COMMIT_TAG:-}' is not an Octez release tag."
  exit 1
fi

# Print the URL computed by a command.
# If the command fails, typically because the resource has not been published,
# fail as well, or print a placeholder in dry-run mode.
# Use it in assignments, so that failures stop the script.
url_of() {
  if url=$("$@"); then
    echo "${url}"
  else
    if [ -n "${url}" ]; then
      echo "${url}" >&2
    fi
    if [ "${dry_run}" = true ]; then
      echo "(not found: $*)"
    else
      exit 1
    fi
  fi
}

docker_image_url() {
  if [ "${CI_PROJECT_NAMESPACE}" = "tezos" ]; then
    ## Production => Docker Hub
    docker_hub_path='tezos/tezos'
    echo "Query Docker Hub repository to get image URL at https://hub.docker.com/r/${docker_hub_path}" >&2

    token=$(curl -fsSL "https://auth.docker.io/token?scope=repository:${docker_hub_path}:pull&service=registry.docker.io" | jq -r '.token')
    docker_image_digest=$(curl -fsSL -X GET \
      -H 'Accept: application/vnd.docker.distribution.manifest.list.v2+json' \
      -H "Authorization: Bearer ${token}" \
      "https://registry-1.docker.io/v2/${docker_hub_path}/manifests/${CI_COMMIT_TAG}" |
      jq -r '.manifests | .[0] | .digest | split(":")[1]') || docker_image_digest=''

    if [ -z "${docker_image_digest}" ]; then
      echo "Error: could not find Docker Hub image matching tag ${CI_COMMIT_TAG}" >&2
      return 1
    fi
    echo "https://hub.docker.com/layers/${docker_hub_path}/${CI_COMMIT_TAG}/images/sha256-${docker_image_digest}"
  else
    ## Other namespaces => GitLab container registry of the project
    echo "${CI_PROJECT_URL}/container_registry?search[]=${CI_COMMIT_TAG}"
  fi
}

# Links of the release, one JSON object per line.
links=''
add_link() {
  link=$(jq -nc --arg name "$1" --arg url "$2" --arg link_type "$3" \
    '{name: $name, url: $url, link_type: $link_type}')
  links="${links}${link}
"
}

echo "Query GitLab to get generic package URL"

if [ -n "${gitlab_packaging_revision_version}" ]; then
  # The release of the version that this packaging revision revises.
  release_tag="octez-v${gitlab_release_major_version}.${gitlab_release_minor_version}"
  echo "### Packaging revision ${gitlab_packaging_revision_version}: updating the static binaries of release '${release_tag}'"
  binaries_url=$(url_of package_web_path "${gitlab_octez_binaries_package_name}")
  add_link "Static binaries" "${binaries_url}" package
else
  release_tag="${CI_COMMIT_TAG}"
  add_link "NEW RELEASE PAGE (Use This Instead)" "https://octez.tezos.com/releases/" other
  add_link "Changelog" "https://octez.tezos.com/docs/CHANGES.html#version-${gitlab_release_no_dot}" other
  add_link "Announcement" "https://octez.tezos.com/docs/releases/version-${gitlab_release_major_version}.html" other
  docker_url=$(url_of docker_image_url)
  binaries_url=$(url_of package_web_path "${gitlab_octez_binaries_package_name}")
  source_url=$(url_of package_web_path "${gitlab_octez_source_package_name}")
  add_link "Docker image" "${docker_url}" image
  add_link "Static binaries" "${binaries_url}" package
  add_link "Octez source" "${source_url}" other
fi

echo "### Links of release '${release_tag}':"
printf '%s' "${links}"

if [ "${dry_run}" = true ]; then
  echo "### Dry-run mode: release '${release_tag}' was not created or updated."
  exit 0
fi

echo "Checking if release '${release_tag}' exists..."
gitlab_release_json=$(curl -fsSL -X GET \
  -H "JOB-TOKEN: ${CI_JOB_TOKEN}" \
  "${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/releases/${release_tag}" ||
  echo "null")

if [ "${gitlab_release_json}" = "null" ]; then
  if [ -n "${gitlab_packaging_revision_version}" ]; then
    echo "Error: Release '${release_tag}' does not exist. Cannot update non-existing release."
    echo "Please create the release first using the standard release pipeline."
    exit 1
  fi

  echo "Creating release '${release_tag}'..."

  # Pass each link as an [--assets-link] argument.
  set --
  while IFS= read -r link; do
    if [ -n "${link}" ]; then
      set -- "$@" "--assets-link=${link}"
    fi
  done << EOF
${links}
EOF

  # GitLab Release command-line tool
  # https://gitlab.com/gitlab-org/release-cli

  # Enable release-cli verbose mode
  export DEBUG='true'

  release-cli create \
    --name="${gitlab_release_name}" \
    --tag-name="${release_tag}" \
    --description="Download Octez, a reference implementation for the Tezos blockchain.

🔗 New Release Page: https://octez.tezos.com/releases/" \
    "$@"
else
  echo "Release '${release_tag}' exists, updating its links..."

  # Not [echo]: in dash, it would turn the [\n] of JSON strings into newlines,
  # which jq rejects.
  current_links=$(printf '%s\n' "${gitlab_release_json}" | jq '.assets.links')

  while IFS= read -r link; do
    if [ -z "${link}" ]; then
      continue
    fi
    name=$(printf '%s\n' "${link}" | jq -r '.name')

    # Remove the existing links with the same name, if any.
    for old_link_id in $(printf '%s\n' "${current_links}" | jq -r --arg name "${name}" '.[] | select(.name == $name) | .id'); do
      echo "Removing old link '${name}' (ID: ${old_link_id})..."
      curl -fsSL -X DELETE \
        -H "JOB-TOKEN: ${CI_JOB_TOKEN}" \
        "${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/releases/${release_tag}/assets/links/${old_link_id}"
      echo ''
    done

    echo "Adding link: ${link}"
    curl -fsSL -X POST \
      -H "JOB-TOKEN: ${CI_JOB_TOKEN}" \
      -H "Content-Type: application/json" \
      "${CI_API_V4_URL}/projects/${CI_PROJECT_ID}/releases/${release_tag}/assets/links" \
      --data "${link}"
    echo ''
  done << EOF
${links}
EOF
fi

echo "### Successfully created or updated release '${release_tag}'"

#!/bin/sh
set -eu

### Copy the already-published Docker distribution tags to another registry.
###
### Usage: docker_copy_images.sh --to gitlab-registry|docker-hub [--dry-run]
###
###   source: wherever the distribution was published, which
###           docker_image_names.sh picked earlier in the pipeline -- Docker Hub
###           if CI_DOCKER_HUB is true on tezos/tezos, the GCP registry otherwise.
###   destination:
###     - gitlab-registry: this repository's GitLab container registry
###       ($CI_REGISTRY_IMAGE);
###     - docker-hub: the Docker Hub repositories of this project
###       (docker.io/$CI_PROJECT_PATH). Requires CI_DOCKER_AUTH.
###
### With --dry-run, only print what would be copied, without logging into
### the destination nor copying anything.
###
### This script only reads from the source; it never pushes there. It copies
### what is already present, so it must run after docker_merge_manifests.sh has
### assembled the multi-architecture tags.
###
### Everything is done with regctl, registry to registry: no Docker daemon is
### involved. regctl preserves the manifest list, the attestation manifests and
### every digest, which is what keeps the copied images bit-identical to the
### published ones -- and therefore keeps the cosign signatures, stored as
### separate sha256-<digest>.sig tags, verifying against them. Those tags are
### copied alongside.

current_dir=$(cd "$(dirname "${0}")" && pwd)

usage() {
  echo "Usage: $0 --to gitlab-registry|docker-hub [--dry-run]" >&2
  exit 1
}

destination=''
dry_run=false
while [ $# -gt 0 ]; do
  case "$1" in
  --to)
    if [ $# -lt 2 ]; then
      usage
    fi
    destination="$2"
    shift 2
    ;;
  --dry-run)
    dry_run=true
    shift
    ;;
  *)
    echo "Unknown argument: $1" >&2
    exit 1
    ;;
  esac
done

# Read DOCKER_IMAGE_NAME / DOCKER_IMAGE_TAG, written by docker_image_names.sh.
. scripts/ci/docker.env

# shellcheck source=./scripts/ci/docker.sh
. "${current_dir}/docker.sh"

# Source: whichever registry docker_image_names.sh selected. Derived from the
# image name rather than re-deriving the branch conditions it already applied.
source_registry="${DOCKER_IMAGE_NAME%%/*}"

# Login to Docker Hub with CI_DOCKER_AUTH, the base64 of "user:password".
docker_hub_login() {
  : "${CI_DOCKER_AUTH:?CI_DOCKER_AUTH is required to log into Docker Hub}"
  hub_user=$(printf '%s' "${CI_DOCKER_AUTH}" | base64 -d | cut -d: -f1)
  hub_pass=$(printf '%s' "${CI_DOCKER_AUTH}" | base64 -d | cut -d: -f2-)
  regctl registry login docker.io --user "${hub_user}" --pass "${hub_pass}"
}

# [destination_prefix] is the destination of the [minimal] variant.
# Other variants go to [<destination_prefix>-<variant>].
case "${destination}" in
gitlab-registry)
  : "${CI_REGISTRY:?CI_REGISTRY is required}"
  : "${CI_REGISTRY_IMAGE:?CI_REGISTRY_IMAGE is required}"
  : "${CI_REGISTRY_USER:?CI_REGISTRY_USER is required}"
  : "${CI_REGISTRY_PASSWORD:?CI_REGISTRY_PASSWORD is required}"
  # The destination repositories are named after the Docker Hub ones --
  # [tezos], [tezos-bare] and [tezos-debug] -- nested under the project path
  # rather than under a namespace. Appending the variant to $CI_REGISTRY_IMAGE
  # directly ([<project>-debug]) is not an option: that names a different
  # GitLab project, not a repository inside this one.
  destination_prefix="${CI_REGISTRY_IMAGE}/tezos"
  ;;
docker-hub)
  if [ "${source_registry}" = "docker.io" ]; then
    echo "Error: the images are already on Docker Hub" >&2
    exit 1
  fi
  destination_prefix="docker.io/${CI_PROJECT_PATH}"
  ;;
*)
  usage
  ;;
esac

##############################################################################
# Authentication                                                             #
##############################################################################

# The credentials are passed with --pass: regctl v0.4.3, the version pinned in
# images/base-images (REGCTL_VERSION), offers only --user and --pass for
# [registry login] -- there is no --pass-stdin. They are therefore briefly
# visible in the container's process list, which is acceptable here: the
# container is single-tenant and ephemeral, and GitLab masks the values in the
# job log.

if [ "${dry_run}" = true ]; then
  echo "### Dry-run mode: not logging into the destination"
else
  case "${destination}" in
  gitlab-registry)
    # This project's registry, via the predefined job-token credentials.
    echo "### Logging into the GitLab container registry ${CI_REGISTRY}"
    regctl registry login "${CI_REGISTRY}" \
      --user "${CI_REGISTRY_USER}" --pass "${CI_REGISTRY_PASSWORD}"
    ;;
  docker-hub)
    echo "### Logging into Docker Hub"
    docker_hub_login
    ;;
  esac
fi

echo "### Logging into the source registry ${source_registry}"
if [ "${source_registry}" = "docker.io" ]; then
  docker_hub_login
else
  # GCP Artifact Registry: a short-lived OAuth access token, obtained with the
  # protected service account on protected refs.
  # shellcheck source=./scripts/ci/gcp_auth.sh
  . "${current_dir}/gcp_auth.sh"
  gcp_token=$(gcloud auth print-access-token)
  regctl registry login "${source_registry}" \
    --user oauth2accesstoken --pass "${gcp_token}"
fi

##############################################################################
# Copy                                                                       #
##############################################################################

# The two tags docker_merge_manifests.sh creates, recomputed identically,
# and the per-architecture tags docker_release.sh creates.
short_tag="${DOCKER_IMAGE_TAG}"
last_commit_date_time=$(git log --pretty=format:"%cd" -1 --date="format:%Y%m%d%H%M%S")
long_tag="${short_tag}_${CI_COMMIT_SHORT_SHA}_${last_commit_date_time}"
docker_tags="${short_tag} ${long_tag}"
for docker_architecture in ${docker_architectures}; do
  docker_tags="${docker_tags} ${docker_architecture}_${short_tag}"
done

# Copy one image's tags, plus the cosign signature of each.
# Args: $1=source image (no tag), $2=destination image (no tag)
copy_image() {
  src_image="${1}"
  dst_image="${2}"

  for docker_tag in ${docker_tags}; do
    # cosign stores the signature under a tag derived from the image digest.
    # The digest survives the copy, so the same tag name applies on both sides.
    digest=$(regctl image digest "${src_image}:${docker_tag}")
    sig_tag="$(echo "${digest}" | tr ':' '-').sig"

    if [ "${dry_run}" = true ]; then
      echo "### Would copy ${src_image}:${docker_tag} (${digest}) -> ${dst_image}:${docker_tag}"
      continue
    fi

    echo "### Copying ${src_image}:${docker_tag} -> ${dst_image}:${docker_tag}"
    regctl image copy "${src_image}:${docker_tag}" "${dst_image}:${docker_tag}"

    # [manifest digest] doubles as an existence probe: regctl v0.4.3 has no
    # [manifest head], and this fails when the tag is absent.
    if regctl manifest digest "${src_image}:${sig_tag}" > /dev/null 2>&1; then
      echo "### Copying ${src_image}:${sig_tag} -> ${dst_image}:${sig_tag}"
      regctl image copy "${src_image}:${sig_tag}" "${dst_image}:${sig_tag}"
    elif [ "${destination}" = docker-hub ]; then
      # Images published on Docker Hub must be signed.
      echo "### No cosign signature for ${src_image}:${docker_tag}" >&2
      return 1
    else
      # A missing signature is not an error: this may run against tags that
      # were not signed.
      echo "### No cosign signature for ${src_image}:${docker_tag}, skipping"
    fi
  done
}

# $docker_images and $docker_targets are index-aligned (see docker.sh), so the
# positional parameters pair each published image with its variant name, from
# which the destination follows.
#
# The variants are independent, so they are copied concurrently: each one is
# a long series of small requests against two registries, and the job is bound
# by round-trips rather than by bandwidth or CPU. Output is buffered per
# variant and replayed in order, so concurrency does not interleave the logs.
# shellcheck disable=SC2086
set -- ${docker_images}
log_dir=$(mktemp -d)
pids=""
targets=""

for docker_target in ${docker_targets}; do
  src_image="${1}"
  shift

  case "${docker_target}" in
  minimal) dst_image="${destination_prefix}" ;;
  *) dst_image="${destination_prefix}-${docker_target}" ;;
  esac

  copy_image "${src_image}" "${dst_image}" > "${log_dir}/${docker_target}" 2>&1 &
  pids="${pids} $!"
  targets="${targets} ${docker_target}"
done

status=0
for pid in ${pids}; do
  wait "${pid}" || status=1
done

for docker_target in ${targets}; do
  echo "----- ${docker_target} -----"
  cat "${log_dir}/${docker_target}"
done
rm -rf "${log_dir}"

if [ "${status}" -ne 0 ]; then
  echo "### At least one variant failed to copy" >&2
  exit 1
fi

if [ "${dry_run}" = true ]; then
  echo "### Dry-run mode: nothing was copied to ${destination_prefix}*"
else
  echo "### Copied ${DOCKER_IMAGE_NAME}* to ${destination_prefix}* (tags: ${docker_tags})"
fi

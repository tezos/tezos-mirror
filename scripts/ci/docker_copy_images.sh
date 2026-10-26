#!/bin/sh
set -eu

### Copy the already-published Docker distribution tags into this repository's
### GitLab container registry.
###
###   source: wherever the distribution was published, which
###           docker_image_names.sh picked earlier in the pipeline -- Docker Hub
###           on protected refs of tezos/tezos, the GCP registry otherwise.
###   destination: always $CI_REGISTRY_IMAGE.
###
### This script only reads from the source; it never pushes there. It mirrors
### what is already present, so it must run after docker_merge_manifests.sh has
### assembled the multi-architecture tags.
###
### Everything is done with regctl, registry to registry: no Docker daemon is
### involved, so the job needs no dind service. regctl preserves the manifest
### list and every digest, which is what keeps the mirrored images
### bit-identical to the published ones -- and therefore keeps the cosign
### signatures, stored as separate sha256-<digest>.sig tags, verifying against
### them. Those tags are copied alongside.

current_dir=$(cd "$(dirname "${0}")" && pwd)

# Read DOCKER_IMAGE_NAME / DOCKER_IMAGE_TAG, written by docker_image_names.sh.
. scripts/ci/docker.env

# shellcheck source=./scripts/ci/docker.sh
. "${current_dir}/docker.sh"

: "${CI_REGISTRY:?CI_REGISTRY is required}"
: "${CI_REGISTRY_IMAGE:?CI_REGISTRY_IMAGE is required}"
: "${CI_REGISTRY_USER:?CI_REGISTRY_USER is required}"
: "${CI_REGISTRY_PASSWORD:?CI_REGISTRY_PASSWORD is required}"

##############################################################################
# Authentication                                                             #
##############################################################################

# Destination: this project's registry, via the predefined job-token
# credentials.
#
# The credentials are passed with --pass: regctl v0.4.3, the version pinned in
# images/base-images (REGCTL_VERSION), offers only --user and --pass for
# [registry login] -- there is no --pass-stdin. They are therefore briefly
# visible in the container's process list, which is acceptable here: the
# container is single-tenant and ephemeral, and GitLab masks the values in the
# job log.
echo "### Logging into the GitLab container registry ${CI_REGISTRY}"
regctl registry login "${CI_REGISTRY}" \
  --user "${CI_REGISTRY_USER}" --pass "${CI_REGISTRY_PASSWORD}"

# Source: whichever registry docker_image_names.sh selected. Derived from the
# image name rather than re-deriving the branch conditions it already applied.
source_registry="${DOCKER_IMAGE_NAME%%/*}"
echo "### Logging into the source registry ${source_registry}"
if [ "${source_registry}" = "docker.io" ]; then
  # CI_DOCKER_AUTH is the base64 of "user:password".
  hub_user=$(printf '%s' "${CI_DOCKER_AUTH}" | base64 -d | cut -d: -f1)
  hub_pass=$(printf '%s' "${CI_DOCKER_AUTH}" | base64 -d | cut -d: -f2-)
  regctl registry login "${source_registry}" --user "${hub_user}" --pass "${hub_pass}"
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
# Mirroring                                                                  #
##############################################################################

# The two tags docker_merge_manifests.sh creates, recomputed identically.
short_tag="${DOCKER_IMAGE_TAG}"
last_commit_date_time=$(git log --pretty=format:"%cd" -1 --date="format:%Y%m%d%H%M%S")
long_tag="${short_tag}_${CI_COMMIT_SHORT_SHA}_${last_commit_date_time}"
docker_tags="${short_tag} ${long_tag}"

# Copy one image's tags, plus the cosign signature of each.
# Args: $1=source image (no tag), $2=destination image (no tag)
mirror_image() {
  src_image="${1}"
  dst_image="${2}"

  for docker_tag in ${docker_tags}; do
    echo "### Mirroring ${src_image}:${docker_tag} -> ${dst_image}:${docker_tag}"
    regctl image copy "${src_image}:${docker_tag}" "${dst_image}:${docker_tag}"

    # cosign stores the signature under a tag derived from the image digest.
    # The digest survives the copy, so the same tag name applies on both sides.
    # A missing signature is not an error: this may run against tags that were
    # not signed.
    digest=$(regctl image digest "${src_image}:${docker_tag}")
    sig_tag="$(echo "${digest}" | tr ':' '-').sig"
    # [manifest digest] doubles as an existence probe: regctl v0.4.3 has no
    # [manifest head], and this fails when the tag is absent.
    if regctl manifest digest "${src_image}:${sig_tag}" > /dev/null 2>&1; then
      echo "### Mirroring ${src_image}:${sig_tag} -> ${dst_image}:${sig_tag}"
      regctl image copy "${src_image}:${sig_tag}" "${dst_image}:${sig_tag}"
    else
      echo "### No cosign signature for ${src_image}:${docker_tag}, skipping"
    fi
  done
}

# $docker_images and $docker_targets are index-aligned (see docker.sh), so the
# positional parameters pair each published image with its variant name, from
# which the destination under $CI_REGISTRY_IMAGE follows.
#
# The destination repositories are named after the Docker Hub ones -- [tezos],
# [tezos-bare] and [tezos-debug] -- nested under the project path rather than
# under a namespace. Appending the variant to $CI_REGISTRY_IMAGE directly
# ([<project>-debug]) is not an option: that names a different GitLab project,
# not a repository inside this one.
#
# The variants are independent, so they are mirrored concurrently: each one is
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
  minimal) dst_image="${CI_REGISTRY_IMAGE}/tezos" ;;
  *) dst_image="${CI_REGISTRY_IMAGE}/tezos-${docker_target}" ;;
  esac

  mirror_image "${src_image}" "${dst_image}" > "${log_dir}/${docker_target}" 2>&1 &
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
  echo "### At least one variant failed to mirror" >&2
  exit 1
fi

echo "### Mirrored ${DOCKER_IMAGE_NAME}* to ${CI_REGISTRY_IMAGE} (tags: ${docker_tags})"

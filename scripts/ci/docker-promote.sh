#!/bin/sh
set -eu

### Promote Docker images from a source tag to one or more target tags.
### Re-tags per-arch images and creates multi-arch manifests for each target.
###
### Usage: docker-promote.sh [--source-tag SOURCE] --target-tag TARGET [--target-tag TARGET ...] [--skip-per-arch-tags]
###
### If --source-tag is not provided, DOCKER_IMAGE_TAG from docker.env is used.
### If --skip-per-arch-tags is set, the manifest is built from the source per-arch
### tags directly, without pushing separate target per-arch tags.

current_dir=$(cd "$(dirname "${0}")" && pwd)

. scripts/ci/docker.env

# shellcheck source=./scripts/ci/docker.sh
. "${current_dir}/docker.sh"

source_tag="${DOCKER_IMAGE_TAG}"
target_tags=""
skip_per_arch_tags=false

while [ $# -gt 0 ]; do
  case "$1" in
  --source-tag)
    source_tag="$2"
    shift 2
    ;;
  --target-tag)
    target_tags="${target_tags:+${target_tags} }$2"
    shift 2
    ;;
  --skip-per-arch-tags)
    skip_per_arch_tags=true
    shift
    ;;
  *)
    echo "Unknown argument: $1" >&2
    exit 1
    ;;
  esac
done

if [ -z "${target_tags}" ]; then
  echo "Error: at least one --target-tag is required" >&2
  exit 1
fi

echo "### Promoting docker images from '${source_tag}' to:${target_tags}"

for target_tag in ${target_tags}; do
  for docker_image in ${docker_images}; do
    echo "### Promoting ${docker_image}: ${source_tag} -> ${target_tag}"

    # Promote per-arch images
    amends=''
    for docker_architecture in ${docker_architectures}; do
      source_arch_tag="${docker_architecture}_${source_tag}"
      target_arch_tag="${docker_architecture}_${target_tag}"

      echo "Pulling ${docker_image}:${source_arch_tag}"
      docker pull "${docker_image}:${source_arch_tag}"

      # Verify signature before promoting
      ./scripts/ci/docker_verify_signature.sh "${docker_image}:${source_arch_tag}"

      if [ "${skip_per_arch_tags}" = false ]; then
        echo "Tagging ${docker_image}:${source_arch_tag} -> ${docker_image}:${target_arch_tag}"
        docker tag "${docker_image}:${source_arch_tag}" "${docker_image}:${target_arch_tag}"
        docker push "${docker_image}:${target_arch_tag}"

        # Sign individual architecture image
        ./scripts/ci/docker_sign.sh "${docker_image}:${target_arch_tag}"

        amends="${amends} --amend ${docker_image}:${target_arch_tag}"
      else
        amends="${amends} --amend ${docker_image}:${source_arch_tag}"
      fi
    done

    # Create and push multi-arch manifest
    echo "Creating manifest for ${docker_image}:${target_tag}"
    eval "docker manifest create ${docker_image}:${target_tag}${amends}"
    docker manifest push "${docker_image}:${target_tag}"

    # Sign multi-arch manifest
    ./scripts/ci/docker_sign.sh "${docker_image}:${target_tag}"
  done
done

# Signature verification
for target_tag in ${target_tags}; do
  for docker_image in ${docker_images}; do
    echo "### Verifying signatures for ${docker_image}:${target_tag}"
    ./scripts/ci/docker_verify_signature.sh "${docker_image}:${target_tag}"
    if [ "${skip_per_arch_tags}" = false ]; then
      for docker_architecture in ${docker_architectures}; do
        target_arch_tag="${docker_architecture}_${target_tag}"
        ./scripts/ci/docker_verify_signature.sh "${docker_image}:${target_arch_tag}"
      done
    fi
  done
done

# Vulnerability attestation: attach a signed in-toto vuln predicate
# produced by the container_scanning job (passed via VULN_PREDICATE artifact).
if [ -n "${VULN_PREDICATE:-}" ] && [ -f "${VULN_PREDICATE}" ]; then
  echo "### Attaching vulnerability attestations"

  # Re-create GCP credentials for cosign attest
  # (cosign is already installed by docker_sign.sh above)
  echo "${GCP_SIGNER_SERVICE_ACCOUNT}" | base64 -d > signer_sa.json
  trap 'rm -f signer_sa.json' EXIT
  export GOOGLE_APPLICATION_CREDENTIALS=signer_sa.json

  for target_tag in ${target_tags}; do
    for docker_image in ${docker_images}; do
      IMAGE_DIGEST="${docker_image}@$(docker buildx imagetools inspect "${docker_image}:${target_tag}" --format '{{json .Manifest}}' | jq -r '.digest')"
      echo "==> Attesting ${IMAGE_DIGEST}"
      cosign attest --predicate "${VULN_PREDICATE}" --type vuln \
        --key "${GCP_SIGN_KEY}" "${IMAGE_DIGEST}" -y
    done
  done
fi

echo "### Successfully promoted '${source_tag}' to:${target_tags}"

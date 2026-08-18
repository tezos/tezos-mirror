#!/bin/sh
#
# Verify that Octez Docker image(s) carry buildx-attached SLSA
# provenance and CycloneDX/SPDX SBOM attestations.
#
# Usage:
# - "$0 <IMAGE:TAG>" : verify the provided image reference
# - "$0"            : verify every image listed in $docker_images at
#                     the tag $DOCKER_IMAGE_TAG (loaded via docker.sh)
#
# Three layers of checks are performed:
#
#   1. Structural: the OCI index contains at least one attestation
#      manifest entry (vnd.docker.reference.type=attestation-manifest,
#      vnd.in-toto+json payload) per image manifest.
#
#   2. Provenance content: [docker buildx imagetools inspect --format
#      '{{json .Provenance}}'] returns the buildx provenance wrapper. We
#      walk the JSON looking for a [buildType] field (root for SLSA v0.2,
#      under [buildDefinition] for SLSA v1) and reject anything that
#      does not start with [https://].
#
#   3. SBOM content: [docker buildx imagetools inspect --format
#      '{{json .SBOM}}'] returns the buildx SBOM wrapper. We walk it for a
#      [spdxVersion] (SPDX) or [bomFormat] (CycloneDX) field.
#
# We inspect the [.Provenance] / [.SBOM] wrappers here (rather than the
# qualified [.Provenance.SLSA] / [.SBOM.SPDX] selectors) so this script
# works whether it is given a single-platform per-arch tag -- where the
# wrapper is [{"SLSA": {..}}] -- or a merged multi-platform index -- where
# buildx returns a per-platform map [{"linux/amd64": {"SLSA": {..}}, ..}]
# and the qualified selector resolves to [null]. The recursive [..] jq
# walk below extracts the predicate from either shape, so the same check
# holds for the CI per-arch tags and for manual multi-arch verification.
# docs/introduction/cosign-verify.rst shows consumers how to unwrap the
# bare predicate for a chosen platform.
#
# buildkit produces these attestations when [--provenance=mode=max] and
# [--sbom=true] are passed to [docker buildx build --push].
#
# Note: these are OCI-standard in-toto referrer attestations, not cosign
# DSSE attestations created by [cosign attest]. [cosign verify-attestation]
# therefore does not find them ("no matching attestations"); that is by
# design. Integrity is covered by the image signature: the OCI index is
# cosign-signed and commits to the digest of every referrer, so
# [cosign verify] (recursive) transitively covers the attestation manifests.
# We inspect the predicate content with [imagetools inspect] here.

set -eu

echo "Starting Docker image attestation verification..."

if [ $# -gt 1 ]; then
  echo "Usage: $0 [<IMAGE:TAG>]" >&2
  exit 1
elif [ $# -eq 1 ]; then
  IMAGE="${1%:*}"
  TAG="${1##*:}"
  # Reject a reference with no tag: a registry-port host such as
  # 'reg:5000/img' otherwise splits into IMAGE=reg / TAG=5000/img and only
  # fails later with a confusing inspect error. A tag never contains '/'.
  case "${TAG}" in */*) TAG="" ;; esac
  if [ "$1" = "${IMAGE}" ] || [ -z "${IMAGE}" ] || [ -z "${TAG}" ]; then
    echo "Format error: argument must respect <IMAGE:TAG> format" >&2
    exit 1
  fi
fi

verify_attestation() {
  echo "==> Inspecting OCI index for $1:$2"

  INDEX=$(docker buildx imagetools inspect "$1:$2" --raw)

  IMAGE_COUNT=$(printf '%s' "${INDEX}" |
    jq '[.manifests[]? | select(.platform.os != "unknown")] | length')
  ATTESTATION_COUNT=$(printf '%s' "${INDEX}" |
    jq '[.manifests[]?
         | select(.annotations."vnd.docker.reference.type" == "attestation-manifest")
        ] | length')

  echo "    image manifests:       ${IMAGE_COUNT}"
  echo "    attestation manifests: ${ATTESTATION_COUNT}"

  if [ "${ATTESTATION_COUNT}" -lt "${IMAGE_COUNT}" ]; then
    echo "ERROR: $1:$2 is missing attestation manifest(s)." >&2
    echo "       Expected at least one attestation per image manifest." >&2
    echo "       Did [create_docker_image.sh --push] run with" >&2
    echo "       provenance+sbom attestations (docker-bake.hcl, PUSH=true)?" >&2
    exit 1
  fi

  # Provenance content check: parse the buildx provenance wrapper and
  # confirm it has a SLSA-shaped [buildType] field. Using the [.Provenance]
  # wrapper (not [.Provenance.SLSA]) covers both a single-platform manifest
  # ([{"SLSA": {..}}]) and a merged multi-platform index (a per-platform map
  # ([{"linux/amd64": {"SLSA": {..}}, ..}]) where the qualified selector
  # would resolve to [null]); the walking [..] jq extracts either shape.
  PROVENANCE=$(docker buildx imagetools inspect "$1:$2" \
    --format '{{json .Provenance}}')

  if [ "${PROVENANCE}" = "null" ] || [ -z "${PROVENANCE}" ] ||
    [ "${PROVENANCE}" = "{}" ]; then
    echo "ERROR: buildx returned no provenance content for $1:$2" >&2
    exit 1
  fi

  BUILD_TYPES=$(printf '%s' "${PROVENANCE}" |
    jq -r '[.. | objects | (.buildType // empty)] | unique | .[]')

  if [ -z "${BUILD_TYPES}" ]; then
    echo "ERROR: provenance for $1:$2 has no [buildType] field." >&2
    echo "       Predicate payload was: ${PROVENANCE}" >&2
    exit 1
  fi

  for bt in ${BUILD_TYPES}; do
    case "${bt}" in
    https://*)
      echo "    buildType: ${bt}"
      ;;
    *)
      echo "ERROR: unexpected buildType for $1:$2: ${bt}" >&2
      exit 1
      ;;
    esac
  done

  # SBOM content check: the [.SBOM] wrapper surfaces the syft-produced SPDX
  # document (again per-platform-keyed on a merged multi-platform index). We
  # look for [spdxVersion] (SPDX) or [bomFormat] (CycloneDX) via the same
  # [..] walk so the check still holds if the SBOM generator is switched.
  SBOM=$(docker buildx imagetools inspect "$1:$2" \
    --format '{{json .SBOM}}')

  if [ "${SBOM}" = "null" ] || [ -z "${SBOM}" ] || [ "${SBOM}" = "{}" ]; then
    echo "ERROR: buildx returned no SBOM content for $1:$2" >&2
    exit 1
  fi

  SBOM_FORMATS=$(printf '%s' "${SBOM}" |
    jq -r '[.. | objects
            | (.spdxVersion // .bomFormat // empty)
           ] | unique | .[]')

  if [ -z "${SBOM_FORMATS}" ]; then
    echo "ERROR: SBOM for $1:$2 lacks [spdxVersion] or [bomFormat]." >&2
    exit 1
  fi

  for fmt in ${SBOM_FORMATS}; do
    echo "    SBOM format: ${fmt}"
  done

  echo "==> SLSA provenance and SBOM present and parseable on $1:$2"
}

# Verify the provided image or verify all images provided in $docker_images
if [ $# -eq 1 ]; then
  verify_attestation "${IMAGE}" "${TAG}"
else
  # Get context
  current_dir=$(cd "$(dirname "${0}")" && pwd)

  # Read environment variables written by 'docker_image_names.sh'
  # (via docker_initialize.sh --image-names) in 'before_script'. Only the
  # no-arg (CI) mode needs $docker_images / $DOCKER_IMAGE_TAG, so keep this
  # out of the single-image path -- that path must run against an arbitrary
  # image outside a CI job, where docker.env does not exist.
  # shellcheck source=./scripts/ci/docker.env
  . "${current_dir}/docker.env"

  # shellcheck source=./scripts/ci/docker.sh
  . "${current_dir}/docker.sh"

  # Loop over images in $docker_images and use $DOCKER_IMAGE_TAG as tag
  for docker_image in ${docker_images}; do
    verify_attestation "${docker_image}" "${DOCKER_IMAGE_TAG}"
  done
fi

echo "Docker image attestation verification done."

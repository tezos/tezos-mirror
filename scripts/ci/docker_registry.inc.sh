#!/usr/bin/env bash

# A Docker tag name must be valid ASCII and may contain lowercase and
# uppercase letters, digits, underscores, periods and dashes. A tag
# name may not start with a period or a dash and may contain a maximum
# of 128 characters.
sanitizeTag() {
  tr -c -- '-._\n[:alnum:]' '_'
}

# Returns the image name with the correct registry applied based on image type.
# Takes an image name as argument and returns it with the appropriate registry.
# See: https://gitlab.com/tezos/tezos/-/merge_requests/20341
ensure_correct_image_registry() {
  local image_name="${1:?"[ensure_correct_image_registry] mandatory image_name argument is not set"}"

  # Registry migration for FinOps optimization (https://gitlab.com/tezos/tezos/-/merge_requests/20341)
  #
  # We now use 4 registries:
  # - GCP_REGISTRY and GCP_PROTECTED_REGISTRY: For final release images (tezos/tezos),
  #   ci-docker, and ci-release images that must be preserved and excluded from
  #   aggressive cleanup policies
  # - GCP_CI_REGISTRY and GCP_PROTECTED_CI_REGISTRY: For all other CI tooling and
  #   intermediate build images, where cleanup policies can be applied to reduce
  #   storage costs
  #
  # The separation enables FinOps-compliant cleanup policies on intermediate images
  # without affecting production artifacts. Most CI images are moved to the dedicated
  # CI registries based on whether the branch is protected or not.
  #
  # However, specific images must remain on the original registries and are excluded
  # from this migration, for these images, we override the registry in the final
  # image_name if applicable:
  case "$image_name" in
  *"tezos/tezos:"* | *"tezos/tezos/bare"* | *"tezos/tezos/debug"* | *"tezos/docker-images/ci-docker"* | *"tezos/docker-images/ci-release"*)
    # These images stay on GCP_REGISTRY/GCP_PROTECTED_REGISTRY
    image_name="${image_name//${GCP_PROTECTED_CI_REGISTRY}/${GCP_PROTECTED_REGISTRY}}"
    image_name="${image_name//${GCP_CI_REGISTRY}/${GCP_REGISTRY}}"
    ;;
  *)
    # All other images: migrate from legacy registries to dedicated CI registries
    image_name="${image_name//${GCP_PROTECTED_REGISTRY}/${GCP_PROTECTED_CI_REGISTRY}}"
    image_name="${image_name//${GCP_REGISTRY}/${GCP_CI_REGISTRY}}"
    ;;
  esac

  echo "$image_name"
}

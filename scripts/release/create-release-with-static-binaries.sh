#! /usr/bin/env bash

set -euo pipefail

. scripts/release/binaries.sh

assets=()
for binary in "${binaries[@]}"; do
    asset_json="$(jq -n --arg name "$binary" \
                        --arg url "$PACKAGE_REGISTRY_URL/$binary" \
                        '{name: $name, url: $url}')"
    assets+=("--assets-link=$asset_json")
done

release-cli create --name "Release $CI_COMMIT_TAG" --tag-name "$CI_COMMIT_TAG" "${assets[@]}"

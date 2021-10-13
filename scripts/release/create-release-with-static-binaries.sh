#!/bin/sh

set -eu

binaries=
. scripts/release/binaries.sh

assets=
for binary in $binaries; do
    asset_json="$(jq -n --arg name "$binary (x86_64 Linux)" \
                        --arg url "$PACKAGE_REGISTRY_URL/x86_64-$binary" \
                        '{name: $name, url: $url}')"
    assets="$assets --assets-link=$asset_json"
    asset_json="$(jq -n --arg name "$binary (arm64 Linux)" \
                        --arg url "$PACKAGE_REGISTRY_URL/arm64-$binary" \
                        '{name: $name, url: $url}')"
    assets="$assets --assets-link=$asset_json"
done

archive_url="$(jq -n --arg name "x86_64-linux-tezos-binaries.tar.gz" \
                     --arg url "$PACKAGE_REGISTRY_URL/x86_64-tezos-binaries.tar.gz" \
                     '{name: $name, url: $url}')"
assets="$assets --assets-link=$archive_url"
archive_url="$(jq -n --arg name "arm64-linux-tezos-binaries.tar.gz" \
                     --arg url "$PACKAGE_REGISTRY_URL/arm64-tezos-binaries.tar.gz" \
                     '{name: $name, url: $url}')"
assets="$assets --assets-link=$archive_url"

#shellcheck disable=SC2086
release-cli create --name "Release $CI_COMMIT_TAG" --tag-name "$CI_COMMIT_TAG" $assets

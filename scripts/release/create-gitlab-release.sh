#!/bin/sh

# release the source tarball and static binaries on the gitlab registry

set -eu

# binaries=
# . scripts/release/binaries.sh

assets=

# source="tezos-source-$CI_COMMIT_SHORT_SHA.tgz"
# asset_json="$(jq -n --arg name "$source (source)" \
#                     --arg url "$PACKAGE_REGISTRY_URL/$source" \
#                         '{name: $name, url: $url}')"
# assets="$assets --assets-link='$asset_json'"

# for binary in $binaries; do
#     asset_json="$(jq -n --arg name "$binary (x86_64 Linux)" \
#                         --arg url "$PACKAGE_REGISTRY_URL/x86_64-$binary" \
#                         '{name: $name, url: $url}')"
#     assets="$assets --assets-link='$asset_json'"
#     asset_json="$(jq -n --arg name "$binary (arm64 Linux)" \
#                         --arg url "$PACKAGE_REGISTRY_URL/arm64-$binary" \
#                         '{name: $name, url: $url}')"
#     assets="$assets --assets-link='$asset_json'"
# done

# archive_json="$(jq -n --arg name "x86_64-linux-tezos-binaries.tar.gz" \
#                       --arg url "$PACKAGE_REGISTRY_URL/x86_64-tezos-binaries.tar.gz" \
#                         '{name: $name, url: $url}')"
# assets="$assets --assets-link='$archive_json'"
# archive_json="$(jq -n --arg name "arm64-linux-tezos-binaries.tar.gz" \
#                       --arg url "$PACKAGE_REGISTRY_URL/arm64-tezos-binaries.tar.gz" \
#                         '{name: $name, url: $url}')"
# assets="$assets --assets-link='$archive_json'"

CI_COMMIT_TAG=12.0-rc2

# Add links for the docker images & documentation announcement.
CI_COMMIT_TAG_NORMALIZED="$(echo "$CI_COMMIT_TAG" | sed -e "s/\./-/g" -e "s/\~/-/g" -e "s/v//g")"
docs_json="$(jq -n --arg url "https://tezos.gitlab.io/CHANGES.html#version-$CI_COMMIT_TAG_NORMALIZED" \
                     '{name: "Announcement", link_type: "other", url: $url}')"
assets="$assets --assets-link='$docs_json'"

CI_COMMIT_TAG_WITHOUT_TILDE="$(echo "$CI_COMMIT_TAG" | sed -e "s/\~/-/g" -e "s/v//g")"

# Authenticate with dockerhub to be able to fetch the image digest.
DOCKER_HUB_TOKEN=$(curl --silent "https://auth.docker.io/token?scope=repository:tezos/tezos:pull&service=registry.docker.io"  | jq -r '.token')
IMAGE_DIGEST="$(curl -s --header "Accept: application/vnd.docker.distribution.manifest.list.v2+json" \
                        --header "Authorization: Bearer ${DOCKER_HUB_TOKEN}" "https://registry-1.docker.io/v2/tezos/tezos/manifests/v$CI_COMMIT_TAG_WITHOUT_TILDE" \
                        | jq -r '.manifests | .[0] | .digest | split(":")[1]')"

docker_images_json="$(jq -n --arg url "https://hub.docker.com/layers/tezos/tezos/v$CI_COMMIT_TAG_WITHOUT_TILDE/images/sha256-$IMAGE_DIGEST" \
                              '{name: "Docker images", link_type: "image", url: $url}')"
assets="$assets --assets-link='$docker_images_json'"

# this is for testing if the tag is not set, only on forks
# if [ -z "$MASTER_OR_RELEASE" ] && ! [ "$TEZOS_DEFAULT_NAMESPACE/$TEZOS_DEFAULT_BRANCH" = "tezos/tezos" ]; then
# CI_COMMIT_TAG=${CI_COMMIT_TAG:-"v0.0.1"}
#   echo "Setting fake \$CI_COMMIT_TAG for testing $CI_COMMIT_TAG"
#   curl --request DELETE --header "PRIVATE-TOKEN: $CI_JOB_TOKEN" "$PACKAGE_REGISTRY_URL/releases/$CI_COMMIT_TAG"
#   echo "Remove test release $CI_COMMIT_TAG"
# fi

#shellcheck disable=SC2086
test=$(echo release-cli create --name \"Release $CI_COMMIT_TAG\" --tag-name \"$CI_COMMIT_TAG\" $assets)
echo $test

eval $test

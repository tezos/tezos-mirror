#! /usr/bin/env bash

set -euo pipefail

binaries=("tezos-admin-client" "tezos-client" "tezos-node" "tezos-signer" "tezos-codec" "tezos-sandbox")
protocols=("006-PsCARTHA" "007-PsDELPH1" "alpha")

for proto in "${protocols[@]}"; do
    binaries+=("tezos-accuser-$proto" "tezos-baker-$proto" "tezos-endorser-$proto")
done

assets=()
for binary in "${binaries[@]}"; do
    asset_link="--assets-link='{\"name\": \"$binary\", \"url\":\"$CI_PROJECT_URL/-/jobs/$CI_JOB_ID/artifacts/raw/install_root/bin/$binary\"}'"
    assets+=("$asset_link")
done

release-cli create --name "Release $VERSION" --tag-name "v$VERSION" "${assets[@]}"

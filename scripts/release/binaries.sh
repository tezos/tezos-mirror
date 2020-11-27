#! /usr/bin/env bash

binaries=("tezos-admin-client" "tezos-client" "tezos-node" "tezos-signer" "tezos-codec" "tezos-sandbox")
protocols=("006-PsCARTHA" "007-PsDELPH1" "alpha")

for proto in "${protocols[@]}"; do
    binaries+=("tezos-accuser-$proto" "tezos-baker-$proto" "tezos-endorser-$proto")
done

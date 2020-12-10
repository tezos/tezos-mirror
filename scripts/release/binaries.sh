#! /usr/bin/env bash

binaries=("tezos-admin-client" "tezos-client" "tezos-node" "tezos-signer" "tezos-codec")

for proto in $(cat active_protocol_versions); do
    binaries+=("tezos-accuser-$proto" "tezos-baker-$proto" "tezos-endorser-$proto")
done

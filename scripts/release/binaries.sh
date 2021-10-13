#!/bin/sh

binaries="tezos-admin-client tezos-client tezos-node tezos-signer tezos-codec"

while read -r  proto; do
    binaries="$binaries tezos-accuser-$proto tezos-baker-$proto tezos-endorser-$proto"
done < active_protocol_versions

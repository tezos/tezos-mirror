#!/bin/bash

binaries=("tezos-admin-client" "tezos-client" "tezos-node" "tezos-signer" "tezos-codec")

for proto in $(cat active_protocol_versions); do
  if [[ "$proto" == "alpha" ]] || [[ "$proto" == "012-Psithaca" ]]
  then
    binaries+=("tezos-accuser-$proto" "tezos-baker-$proto")
  else
    binaries+=("tezos-accuser-$proto" "tezos-baker-$proto" "tezos-endorser-$proto")
  fi
done

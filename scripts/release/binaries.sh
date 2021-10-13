#!/bin/sh

binaries="tezos-admin-client tezos-client tezos-node tezos-signer tezos-codec"

while read -r  proto; do
  if [ "$proto" = "alpha" ] || [ "$proto" = "012-PsiThaCa" ]; then
    binaries="$binaries tezos-accuser-$proto tezos-baker-$proto"
  else
    binaries="$binaries tezos-accuser-$proto tezos-baker-$proto tezos-endorser-$proto"
  fi
done < active_protocol_versions

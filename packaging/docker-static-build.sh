#!/bin/sh

# SPDX-FileCopyrightText: 2020 TQ Tezos <https://tqtezos.com/>
#
# SPDX-License-Identifier: MPL-2.0

# This script builds static tezos-binaries using custom alpine image.
# It expects docker or podman to be installed and configured.

set -euo pipefail

binaries="tezos-admin-client tezos-client tezos-node tezos-signer "
protocols="006-PsCARTHA 007-PsDELPH1"

set -- $protocols

for proto in "$@"; do
    binaries="$binaries tezos-accuser-$proto tezos-baker-$proto tezos-endorser-$proto "
done

docker_file=build/Dockerfile

docker build -t alpine-tezos-"$TEZOS_VERSION" -f build/Dockerfile --build-arg TEZOS_VERSION="$TEZOS_VERSION" \
       --build-arg TEZOS_REPO="$TEZOS_REPO" .
container_id="$(docker create alpine-tezos-"$TEZOS_VERSION")"
set -- $binaries
for b in "$@"; do
    docker cp "$container_id:/tezos/$b" "$b"
done
docker rm -v "$container_id"

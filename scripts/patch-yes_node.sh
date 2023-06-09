#!/bin/sh

# Exit on non-zero status
set -e

# Patch the node sources:
# - the cryptographic library to use fake secret keys,
# - the stresstest command of each protocol that is not frozen to make it work
#   on Tezos Mainnet

for arg in "$@"
do
    case $arg in
        "--dry-run" )
           dry_run='--dry-run'
   esac
done

patch $dry_run -p 1 < scripts/yes-node.patch

for f in src/proto_*/lib_client_commands/client_proto_stresstest_commands.ml
do
  patch $dry_run -p 1 "$f" < scripts/yes-stresstest.patch
done

#!/bin/sh

# Patch the node sources:
# - the cryptographic library to use fake secret keys,
# - the stresstest command of each protocol that is not frozen to make it works
#   on mainnet

patch -p 1 < scripts/yes-node.patch

for f in src/proto_*/lib_client_commands/client_proto_stresstest_commands.ml
do
  patch -p 1 "$f" < scripts/yes-stresstest.patch
done

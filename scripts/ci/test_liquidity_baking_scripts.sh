#!/bin/sh
set -eu

# The first argument of the script is the commit hash used to
# fetch the reference scripts. It cannot be changed for injected
# protocols.
./scripts/check-liquidity-baking-scripts.sh d98643881fe14996803997f1283e84ebd2067e35 src/proto_010_PtGRANAD

# However, for the alpha protocol, it is possible to modify the
# scripts, and therefore to update the hash.
./scripts/check-liquidity-baking-scripts.sh d98643881fe14996803997f1283e84ebd2067e35 src/proto_alpha

#!/usr/bin/env bash

set -e

#
# This script checks that the serialized versions of the Liquidity Baking
# LIGO scripts
#
#   https://gitlab.com/dexter2tz/dexter2tz/-/blob/d98643881fe14996803997f1283e84ebd2067e35/dexter.liquidity_baking.mligo
#   https://gitlab.com/dexter2tz/dexter2tz/-/blob/d98643881fe14996803997f1283e84ebd2067e35/lqt_fa12.mligo
#
#   are correctly embedded in the source tree in
#
#   src/proto_alpha/lib_protocol/liquidity_baking_cpmm.ml
#   src/proto_alpha/lib_protocol/liquidity_baking_lqt.ml
#

# --------------------
echo "* Configuration"
# --------------------

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd)"
TOP_DIR="$SCRIPT_DIR"/..
cd "$TOP_DIR" || exit
"$TOP_DIR"/tezos-client --mode mockup --base-dir /tmp/mockup create mockup \
                      1> /tmp/create-mockup.log 2>&1

TEZOS_CLIENT="$TOP_DIR/tezos-client --base-dir /tmp/mockup \
                    --protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK \
                    --mode mockup"

cd /tmp || exit

# --------------------------------------
echo "* Step 1: Setup the LIGO compiler"
# --------------------------------------

# This is the exact version of the compiler used to compile the scripts:
#
#   https://gitlab.com/ligolang/ligo/-/releases/0.9.0-liquidity-baking
#
rm -fr ligo
wget --quiet https://gitlab.com/ligolang/ligo/-/jobs/1291756399/artifacts/raw/ligo -O ligo
chmod a+rx ligo
LIGO=$(pwd)/ligo

# ----------------------------------------------------
echo "* Step 2: Retrieve and compile the LIGO scripts"
# ----------------------------------------------------

COMMIT_HASH="d98643881fe14996803997f1283e84ebd2067e35"

retrieve () {
  wget --quiet https://gitlab.com/dexter2tz/dexter2tz/-/raw/$COMMIT_HASH/"$1" -O "$2"
}

retrieve dexter.liquidity_baking.mligo cpmm.mligo
retrieve lqt_fa12.mligo lqt.mligo

"$LIGO" compile-contract cpmm.mligo main > cpmm.tz
"$LIGO" compile-contract lqt.mligo main > lqt.tz

# ------------------------------------------------------------------------------
echo "* Step 3: Compute the binary representations of the two Michelson scripts"
# ------------------------------------------------------------------------------

serialize () {
  CONTRACT="$1"
  $TEZOS_CLIENT convert script "$CONTRACT.tz" from michelson to binary > "$CONTRACT.bin"
}

serialize cpmm
serialize lqt

# -----------------------------------------------------------------------------------------
echo "* Step 4: Compare each binary representations with the ones found in the source code"
# ----------------------------------------------------------------------------------------

source_hex () {
    file=$1
    line=$2
    output=$3
    echo -n '0x' > "$output"
    sed -n "${line}"p "$file" | tr -d '"' | tr -d ' ' >> "$output"
}

compare () {
  WHAT="$1"
  SOURCE="$2"
  BIN="$3"
  if diff -q "$SOURCE" "$BIN"; then
      echo "  - $WHAT is correctly embedded in the source."
  else
      echo "  - $WHAT is not correctly embedded in the source."
      exit 1
  fi
}

source_hex "$TOP_DIR"/src/proto_alpha/lib_protocol/liquidity_baking_lqt.ml 3 source.lqt.bin
compare lqt source.lqt.bin lqt.bin

source_hex "$TOP_DIR"/src/proto_alpha/lib_protocol/liquidity_baking_cpmm.ml 3 source.cpmm.bin
compare cpmm source.cpmm.bin cpmm.bin

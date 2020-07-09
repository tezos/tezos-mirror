#! /bin/bash

set -e

usage="Usage:
$ ./scripts/user_activate_update.sh src/proto_007_* 3
Inserts a user-activated upgrade for protocol 007 at level 3.

When passing a low level it will make the sandbox activate protocol
006 so that in a few levels you can test the migration.

When passing a high level (greater than 28082), it assumes that you are using a mainnet
snapshot and it creates a yes-node and yes-wallet. If a yes-wallet is
already present it is preserved."

script_dir="$(cd "$(dirname "$0")" && pwd -P)"
cd "$script_dir"/..

if [ ! -d "$1" ] || [ -z "$2" ]; then
    echo "$usage"
    exit 1
fi

version=$(echo "$1" | sed 's/.*proto_\([0-9]\{3\}\)_.*/\1/')
pred=$(printf "%03d" $(($version -1)))
pred_full_hash=$(jq -r .hash < src/proto_${pred}_*/lib_protocol/TEZOS_PROTOCOL)
pred_short_hash=$(echo $pred_full_hash | head -c 8)

full_hash=$(jq -r .hash < $1/lib_protocol/TEZOS_PROTOCOL)
level=$2

if (( $level > 28082 )); then
# we are on a real network and we need a yes-node and yes-wallet to bake

    # replace existing upgrades
    awk -i inplace -v level=$level -v full_hash=$full_hash '
BEGIN{found=0}{
if (!found && $0 ~ "~user_activated_upgrades")
  {found=1; printf "    ~user_activated_upgrades:\n      [ (%dl, \"%s\") ]\n", level, full_hash}
else {
  if (found && $0 ~ "~user_activated_protocol_overrides")
    {found=0; print }
  else
    { if (!found){print}}
}}' src/bin_node/node_config_file.ml

    patch -p1 < scripts/yes-node.patch

    if ! [ -d yes-wallet ]; then
        dune exec scripts/yes-wallet/yes_wallet.exe
        echo 'Created `yes-wallet` directory.'
    fi
    echo "You can now bake for foundation{1..8}"
else # we are in sandbox

    # add upgrade to the sandbox
    awk -i inplace -v level=$level -v full_hash=$full_hash '
{ print
  if ($0 ~ "~alias:\"sandbox\"")
  { printf "    ~user_activated_upgrades:\n      [ (%dl, \"%s\") ]\n", level, full_hash }
}' src/bin_node/node_config_file.ml

    sed -i.old "s/\$bin_dir\/..\/proto_alpha\/parameters\/sandbox-parameters.json/\$bin_dir\/..\/proto_${pred}_${pred_short_hash}\/parameters\/sandbox-parameters.json/" src/bin_client/tezos-init-sandboxed-client.sh
    sed -i.old "s/activate_alpha()/activate_${pred}_${pred_short_hash}()/" src/bin_client/tezos-init-sandboxed-client.sh
    sed -i.old "s/tezos-activate-alpha/tezos-activate-${pred}-${pred_short_hash}/" src/bin_client/tezos-init-sandboxed-client.sh
    sed -i.old "s/activate protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK/activate protocol $pred_full_hash/" src/bin_client/tezos-init-sandboxed-client.sh
    rm src/bin_client/tezos-init-sandboxed-client.sh.old
    echo "The sandbox will now activate $pred_full_hash and switch to $full_hash at level $level"
fi

make

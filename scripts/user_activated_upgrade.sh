#!/usr/bin/env bash

set -e

usage="Usage:
$ ./scripts/user_activate_update.sh src/proto_<version_number|alpha>* <level>

Inserts a user-activated upgrade for the snapshot protocol with the given
version number at the given level.

When passing a low level (less or equal than 28082) it assumes that migration is
on the sandbox, and it renames the sandbox command octez-activate-alpha that
activates the Alpha protocol to the command

  octez-activate-<predecessor_version>_<predecessor_short_hash>

which activates the predecessor of the Alpha protocol. The <predecessor_version>
coincides with <protocol_version> minus one, and the <predecessor_short_hash>
coincides with the short hash in the name of the folder that contains the
predecessor of the Alpha protocol in the ./src directory, i.e., the folder

  ./src/proto_<predecessor_version>_<predecessor_short_hash>

When passing a high level (greater than 28082), it assumes that
migration is on a context imported from Mainnet."

script_dir="$(cd "$(dirname "$0")" && pwd -P)"
cd "$script_dir"/..

if [ ! -d "$1" ] || [ -z "$2" ]; then
  echo "$usage"
  exit 1
fi

if [[ $1 =~ ^.*/proto_[0-9]{3}_.*$ ]]; then
  #shellcheck disable=SC2001
  version=$(echo "$1" | sed 's/.*proto_\([0-9]\{3\}\)_.*/\1/')
  pred=$(printf "%03d" $((10#$version - 1)))
  pred_full_hash=$(jq -r .hash < src/proto_"${pred}"_*/lib_protocol/TEZOS_PROTOCOL)
  pred_short_hash=$(echo "$pred_full_hash" | head -c 8)

  full_hash=$(jq -r .hash < "$1/lib_protocol/TEZOS_PROTOCOL")
else
  pred_version_dir=$(find src -regex "src/proto_[0-9][0-9][0-9]_[^/]*" -printf '%P\n' | sort -r | head -1)
  pred=$(echo "$pred_version_dir" | cut -d'_' -f2)
  pred_full_hash=$(jq -r .hash < src/proto_"${pred}"_*/lib_protocol/TEZOS_PROTOCOL)
  pred_short_hash=$(echo "$pred_full_hash" | head -c 8)

  version=$((10#$pred + 1))

  full_hash=$(jq -r .hash < src/proto_alpha/lib_protocol/TEZOS_PROTOCOL)
fi
level=$2

if ((level > 28082)); then
  # we are on a real network and we need a yes-node and yes-wallet to bake

  # replace existing upgrades
  awk -v level="$level" -v full_hash="$full_hash" '
BEGIN{found=0}{
if (!found && $0 ~ "BEGIN_PATCHING_ZONE_FOR_MAINNET_USER_ACTIVATED_UPGRADES")
  {found=1; printf "(* BEGIN_PATCHING_ZONE_FOR_MAINNET_USER_ACTIVATED_UPGRADES *)\n";
   printf "let mainnet_user_activated_upgrades = [ (%dl, \"%s\") ]\n", level, full_hash}
else {
  if (found && ($0 ~ "END_PATCHING_ZONE_FOR_MAINNET_USER_ACTIVATED_UPGRADES"))
    {found=0; print }
  else
    { if (!found){print}}
}}' src/lib_node_config/config_file.ml > tmp_file
  mv tmp_file src/lib_node_config/config_file.ml

  echo "The sandbox will now switch to $full_hash at level $level."
else # we are in sandbox

  # add upgrade to the sandbox (same awk script as for mainnet but with
  # "SANDBOX" instead of "MAINNET")
  awk -v level="$level" -v full_hash="$full_hash" '
BEGIN{found=0}{
if (!found && $0 ~ "BEGIN_PATCHING_ZONE_FOR_SANDBOX_USER_ACTIVATED_UPGRADES")
  {found=1; printf "(* BEGIN_PATCHING_ZONE_FOR_SANDBOX_USER_ACTIVATED_UPGRADES *)\n";
   printf "let sandbox_user_activated_upgrades = [ (%dl, \"%s\") ]\n", level, full_hash}
else {
  if (found && ($0 ~ "END_PATCHING_ZONE_FOR_SANDBOX_USER_ACTIVATED_UPGRADES"))
    {found=0; print }
  else
    { if (!found){print}}
}}' src/lib_node_config/config_file.ml > tmp_file
  mv tmp_file src/lib_node_config/config_file.ml

  sed -i.old "s/\$bin_dir\/..\/..\/_build\/default\/src\/proto_alpha\/lib_parameters\/sandbox-parameters.json/\$bin_dir\/..\/..\/_build\/default\/src\/proto_${pred}_${pred_short_hash}\/lib_parameters\/sandbox-parameters.json/" src/bin_client/octez-init-sandboxed-client.sh
  sed -i.old "s/activate_alpha()/activate_${pred}_${pred_short_hash}()/" src/bin_client/octez-init-sandboxed-client.sh
  sed -i.old "s/octez-activate-alpha/octez-activate-${pred}-${pred_short_hash}/" src/bin_client/octez-init-sandboxed-client.sh
  sed -i.old "s/activate protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK/activate protocol $pred_full_hash/" src/bin_client/octez-init-sandboxed-client.sh
  rm src/bin_client/octez-init-sandboxed-client.sh.old
  echo "The sandbox will now switch to $full_hash at level $level."
fi

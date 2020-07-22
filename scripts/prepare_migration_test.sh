#! /usr/bin/env bash

set -e

#setting tmp dir
if ! [[ -z "$TMP" ]]
then
    tmp_dir="$TMP"
elif ! [[ -z "$TMPDIR" ]]
then
    tmp_dir="$TMPDIR"
else
    tmp_dir="/tmp"
fi

usage="Usage:

This script prepares the environment to perform migration tests.

When passing a low level (less or equal than 28082) the script assumes that
migration is run on the sandbox:

$ ./scripts/prepare_migration_test.sh [<protocol_name>_<protocol_version>] \\
  <low_level>

When the optionnal parameter <protocol_name>_<protocol_version> is provided,
then the script snapshots the Alpha protocol and renames the sandbox command
tezos-activate-alpha that activates the Alpha protocol to the command

  tezos-activate-<predecessor_version>_<predecessor_short_hash>

which activates the predecessor of the Alpha protocol. The <predecessor_version>
coincides with <protocol_verison> minus one, and the <predecessor_short_hash>
coincides the the short hash in the name of the folder that contains the
predecessor of the Alpha protocol in the ./src directory, i.e., the folder

  ./src/proto_<predecessor_version>_<predecessor_short_hash>

When passing a high level (greater than 28082) the script assumes that migration
is run on a context imported from Mainnet:

$ ./scripts/prepare_migration_test.sh <protocol_name>_<protocol_version> \\
  <high_level> [<snapshot_file>]

The script patches the code of the Alpha protocol to produce a yes-node, and
creates a yes-wallet in the folder ~/yes-wallet when such folder does not exist
already. If the optional parameter <snapshot_file> is provided, then the script
imports a context from the file specified by the parameter and places it in the
folder ~/tezos-node-orig"

if [ $# -lt 1 ]
then
    echo "$usage"
    exit 1
fi


#set variable related to protocol, in particular \$proto_version and \$pred_proto_version
if [[ $1 =~ ^[a-z]+_[0-9][0-9][0-9]$ ]]
then
    proto_name=$1
    proto_version=$(echo $proto_name | cut -d'_' -f2)

    proto_dir=src/proto_${proto_version}_*/

    pred_proto_version=$(printf "%03d" $(($proto_version -1)))
else
    pred_proto_version_dir=$(find src -name "proto_00*" -printf '%P\n' | sort -r | head -1)
    pred_proto_version=$(echo $pred_proto_version_dir | cut -d'_' -f2)

    proto_version=($pred_proto_version +1)

    proto_dir=src/proto_alpha/
fi

if [ -z "${proto_name}" ]
then
    mig_level=$1
else
    mig_level=$2
fi

if ! [[ "$mig_level" =~ [0-9]+ ]]
then
    echo "$usage"
    exit 1
fi

if ! [ -z "${proto_name}" ]
then
    # snapshot protocol alpha into new directory
    echo "
Calling: ./scripts/snapshot_alpha $proto_name"
    ./scripts/snapshot_alpha.sh $proto_name

    proto_dir=src/proto_${proto_version}_*/

    # link new protocol for the shell and client
    echo "
Calling: ./scripts/link_protocol.sh $proto_dir"
    ./scripts/link_protocol.sh $proto_dir
fi

# activate the migration at a specific level
echo "
Calling: ./scripts/user_activated_upgrade.sh $proto_dir $mig_level"
./scripts/user_activated_upgrade.sh $proto_dir $mig_level

if (( $mig_level <= 28082 ))
then
    echo "
The script detected that you will do a migration on a fresh context."

    # Compile the snapshot Alpha protocol
    make

    pred_full_hash=$(jq -r .hash < src/proto_${pred_proto_version}_*/lib_protocol/TEZOS_PROTOCOL)
    pred_short_hash=$(echo $pred_full_hash | head -c 8)
    echo "
Use the following commands to start the sandboxed node:
$ ./src/bin_node/tezos-sandboxed-node.sh 1 --connections 0 &
$ eval \`./src/bin_client/tezos-init-sandboxed-client.sh 1\`
$ tezos-activate-${pred_proto_version}-${pred_short_hash}

Then bake blocks until the chain reaches level $level with:
$ tezos-client bake for bootstrap1 --minimal-timestamp

In order to re-run the migration test, kill the sandboxed node and run the
commands above (the script needs not to be run again)."
else
    echo "
The script detected that you will do a migration on a context imported from
Mainnet."

    echo "
Patching the code to obtain a yes-node."
    patch -p1 < scripts/yes-node.patch

    yes_wallet="$tmp_dir/yes-wallet"
    if [ -d "$yes_wallet" ]
    then
        echo "
Found existing yes-wallet in directory ${yes_wallet}
The yes-wallet in ${yes_wallet} will be used for the test
after deleting spurious files ${yes_wallet}/{blocks,wallet_locks}"
        [ -f "${yes_wallet}/blocks" ]      && rm "${yes_wallet}/blocks"
        [ -f "${yes_wallet}/wallet_lock" ] && rm "${yes_wallet}/wallet_lock"
    else
        echo "
Creating a yes-wallet in directory ${yes_wallet}"
        dune exec scripts/yes-wallet/yes_wallet.exe "${yes_wallet}"
    fi
    echo "You can now bake for foundation{1..8}.
"

    # Compile the snapshot Alpha protocol
    make

    if ! [ -z "${proto_name}" ]
    then
        snapshot="$3"
    else
        snapshot="$2"
    fi

    # The command ${snapshot%.(rolling|full|archive} did not worked for no
    # found reason
    if [ -f "$snapshot" ]
    then
        directory="${snapshot%.rolling}"
        directory="${directory%.full}"
        directory="${directory%.archive}"
        directory="$tmp_dir/tezos-node-${directory##*/}"
        if [ -d "$directory" ]
        then
            echo "
Found existing context in directory ${directory}
If you want to re-import the context from ${snapshot}
please delete directory ${directory}

The context in ${directory} will be used for the test.
Make sure that this directory contains a valid identity.
You can generate an identity manually by using the command:

$ ./tezos-node identity generate --data-dir \"\$directory\""

        else
            echo "
Importing context from $snapshot into $directory"
            ./tezos-node snapshot import "$snapshot" --data-dir "$directory"
            ./tezos-node identity generate --data-dir "$directory"
        fi

    else
        directory="path/to/snapshot"
        echo "
No snapshot file provided. Please use the following commands to import a context
manually:

$ ./tezos-node snapshot import <snapshot_file> --data-dir <new/context/dir>
$ ./tezos-node identity generate --data-dir <new/context/dir>"
    fi

    echo "
Use the following commands to start the node with the imported context:
$ test_directory=\$(mktemp -d -t \"${directory##*/}-XXXX\") && cp -r \"$directory/.\" \"\$test_directory\"
$ ./tezos-node run --connections 0 --data-dir \"\$test_directory\" --rpc-addr localhost &

Then bake blocks until the chain reaches level $level with:
$ ./tezos-client -d $yes_wallet bake for foundation1 --minimal-timestamp

In order to re-run the migration test, kill the node and delete spurious files
by using:
$ rm -rf \"\$test_directory\" && rm -f $yes_wallet/{blocks,wallet_lock}

Then run the commands to start the node and to bake blocks above (the script
needs not to be run again).

Please adapt these commands to your case if you are using different folders for
the yes-wallet and/or the imported context."

fi

#!/usr/bin/env sh

set -e

snapshot_protocol () {
    f_proto_name="$1"
    f_proto_dir="$2"

    # snapshot protocol alpha into new directory
    echo "
Calling: ./scripts/snapshot_alpha $f_proto_name."
    ./scripts/snapshot_alpha.sh "$f_proto_name"

    # link new protocol for the shell and client
    echo "
Calling: ./scripts/link_protocol.sh $f_proto_dir."

    # $f_proto_dir might contain a wildcard
    # shellcheck disable=SC2086
    ./scripts/link_protocol.sh $f_proto_dir
}


user_activated_upgrade () {
    f_proto_dir="$1"
    f_mig_level="$2"
    # activate the migration at a specific level
    echo "
Calling: ./scripts/user_activated_upgrade.sh $f_proto_dir $f_mig_level."
    ./scripts/user_activated_upgrade.sh "$f_proto_dir" "$f_mig_level"
}

import_snapshot () {
    f_snapshot_path="$1"
    f_blockhash="$2"

    [ -n "$f_blockhash" ] && f_blockhash_opt="--block $f_blockhash"

    # The command ${snapshot%.(rolling|full|archive} did not worked for no
    # found reason
    context_dir="${f_snapshot_path%.rolling}"
    context_dir="${context_dir%.full}"
    context_dir="${context_dir%.archive}"
    context_dir="$tmp_dir/tezos-node-${context_dir##*/}"
    if [ -d "$context_dir" ]
    then
        echo "
Found existing context in directory ${context_dir}
If you want to re-import the context from ${f_snapshot_path}
please delete directory ${context_dir}

The context in ${context_dir} will be used for the test."
    else
        echo "
Importing context from $f_snapshot_path into $context_dir."
        # $f_snapshot_path might be empty
        # shellcheck disable=SC2086
        ./octez-node snapshot import "$f_snapshot_path" --data-dir "$context_dir" $f_blockhash_opt
    fi
}

generate_identities (){
    f_context_dir="$1"
    ./octez-node identity generate --data-dir "$f_context_dir"
}

patch_yes_node () {
    echo "
Patching the code to obtain a yes-node."
    patch -p1 < ./scripts/yes-node.patch
}

create_yes_wallet () {
    yes_wallet="$tmp_dir/yes-wallet"
    if [ -d "$yes_wallet" ]
    then
        echo "
Found existing yes-wallet in directory ${yes_wallet}.
The yes-wallet in ${yes_wallet} will be used for the test
after deleting spurious files ${yes_wallet}/{blocks,wallet_locks}."
        [ -f "${yes_wallet}/blocks" ] && rm "${yes_wallet}/blocks"
        [ -f "${yes_wallet}/wallet_lock" ] && rm "${yes_wallet}/wallet_lock"
    else
        echo "
Creating a yes-wallet in ${yes_wallet} with active delegates from ${context_dir}."
       dune exec devtools/yes_wallet/yes_wallet.exe -- create from context "$context_dir" in "$yes_wallet" --active-bakers-only
    fi
    echo "You can now bake for all active delegates"
}

first_hash() {
    jq -r .hash < "$1"
}

#setting tmp dir
if [ -n "$TMP" ]
then
    tmp_dir="$TMP"
elif [ -n "$TMPDIR" ]
then
    tmp_dir="$TMPDIR"
else
    tmp_dir="/tmp"
fi

usage="Usage:

This script prepares the environment to perform migration tests.

#### Migration on the sandbox ####

$ ./scripts/prepare_migration_test.sh \\
   [<protocol_name>_<protocol_version>] <low_level>

When passing a low level (less or equal than 28082) the script assumes that
migration is run on the sandbox:

When the optional parameter <protocol_name>_<protocol_version> is provided, then
the script snapshots the Alpha protocol and renames the sandbox command
octez-activate-alpha that activates the Alpha protocol to the command

  octez-activate-<predecessor_version>_<predecessor_short_hash>

which activates the predecessor of the Alpha protocol. The <predecessor_version>
coincides with <protocol_version> minus one, and the <predecessor_short_hash>
coincides the the short hash in the name of the folder that contains the
predecessor of the Alpha protocol in the src directory, i.e., the folder

  ./src/proto_<predecessor_version>_<predecessor_short_hash>

#### Migration on a context imported from Mainnet ####

$ ./scripts/prepare_migration_test.sh \\
  [<protocol_name>_<protocol_version>] <high_level> [</path/to/snapshot.rolling>] \\
  [<block_hash>]

When passing a high level (greater than 28082) the script assumes that migration
is run on a context imported from Mainnet:

The script patches the code to produce a yes-node, and creates a yes-wallet
folder in the system's temp folder when such folder does not exist already. If
the optional parameter </path/to/snapshot.rolling> is provided, then the script
imports a context from the file specified by the parameter and places it in a
folder with the same name than the snapshot file (without the extension) under
the system's temp directory. If the optional parameter <block_hash> is provided,
then the script checks that the hash of the last block in the imported chain is
<block_hash>.

## AUTOMATIC PROCEDURE ##

The automatic procedure has been disabled. See the issue:

https://gitlab.com/tezos/tezos/-/issues/5924

for context."

#set variable related to protocol, in particular \$proto_version and \$pred_proto_version
if echo "$1" | grep -q '^[a-z]\+_[0-9][0-9][0-9]$';
then
    proto_name="$1"
    proto_version=$(echo "$proto_name" | cut -d'_' -f2)

    proto_dir="src/proto_${proto_version}_*/"

    # strip leading zeros to prevent version being treated as octal
    proto_version=$(echo "$proto_version" | sed 's/^0*//')
    pred_proto_version=$(printf "%03d" $((proto_version - 1)))
else
    pred_proto_name=$(find src -name "proto_[0-9][0-9][0-9]_*" | awk -F'/' '{print $NF}' | sort -r | head -1)
    pred_proto_version=$(echo "$pred_proto_name" | cut -d'_' -f2)

    proto_dir="src/proto_alpha/"
fi

# now calls correct scripts and renaming
if [ -n "$proto_name" ]
then
    snapshot_protocol "$proto_name" $proto_dir
fi

echo "
Setting environment for test"

if [ -n "$proto_name" ]
then
    mig_level=$2
    snapshot_path=$3
    blockhash=$4
else
    mig_level=$1
    snapshot_path=$2
    blockhash=$3
fi

# check if \$mig_level is set
if [ -z "$mig_level" ]
then
    echo "$usage"
    exit 1
fi

user_activated_upgrade $proto_dir "$mig_level"

pred_full_hash=$(first_hash src/proto_"${pred_proto_version}"_*/lib_protocol/TEZOS_PROTOCOL)
pred_short_hash=$(echo "$pred_full_hash" | head -c 8)

# now calls correct scripts and renaming

if [ "$mig_level" -le 28082 ];
then
    # Env test use a fresh context and no yes-node/wallet
    echo "
The script detected that you will do a migration on the sandbox."

    make

    echo "
Use the following commands to start the sandboxed node:
$ ./src/bin_node/octez-sandboxed-node.sh 1 --connections 0 &
$ eval \`./src/bin_client/octez-init-sandboxed-client.sh 1\`
$ octez-activate-${pred_proto_version}-${pred_short_hash}

Then bake blocks until the chain reaches level $mig_level with:
$ octez-client bake for bootstrap1 --minimal-timestamp

In order to re-run the migration test, kill the sandboxed node and run the
commands above (the script needs not to be run again)."

else # \$mig_level > 28082

    patch_yes_node

    make

    if [ -n "$snapshot_path" ] && [ -f "$snapshot_path" ]
    then
        import_snapshot "$snapshot_path" "$blockhash"
        ! [ -f "$context_dir/identity.json" ] && generate_identities "$context_dir"
        create_yes_wallet
    else
        context_dir="path/to/tezos-node-context"
        echo "
No snapshot file provided. Please use the following commands to import a context
manually:

$ ./octez-node snapshot import <snapshot_file> --data-dir <new/context/dir>
$ ./octez-node identity generate --data-dir <new/context/dir>"
    fi


    echo "
Use the following commands to start the node with the imported context:
$ test_directory=\$(mktemp -d -t \"${context_dir##*/}-XXXX\") && cp -r \"$context_dir/.\" \"\$test_directory\"
$ ./octez-node run --connections 0 --data-dir \"\$test_directory\" --rpc-addr localhost &

Then bake blocks until the chain reaches level $mig_level with:
$ ./octez-client -d $yes_wallet bake for --minimal-timestamp

In order to re-run the migration test, kill the node and delete spurious fil
by using:
$ rm -rf \"\$test_directory\" && rm -f $yes_wallet/{blocks,wallet_lock}

Then run the commands to start the node and to bake blocks above (the script
needs not to be run again).

Please adapt these commands to your case if you are using different folders for
the yes-wallet and/or the imported context."

fi

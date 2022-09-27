#!/bin/sh

set -e

bin_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"

: "${BIN_DIR:="/usr/local/bin"}"
: "${DATA_DIR:="/var/run/tezos"}"

: "${NODE_HOST:="node"}"
: "${NODE_RPC_PORT:="8732"}"

: "${PROTOCOL:="unspecified-PROTOCOL-variable"}"

# export all these variables to be used in the inc script
export node="$BIN_DIR/octez-node"
export client="$BIN_DIR/octez-client"
export admin_client="$BIN_DIR/octez-admin-client"
export baker="$BIN_DIR/octez-baker-$PROTOCOL"
export endorser="$BIN_DIR/tezos-endorser-$PROTOCOL"
export accuser="$BIN_DIR/tezos-accuser-$PROTOCOL"
export signer="$BIN_DIR/octez-signer"

export client_dir="$DATA_DIR/client"
export node_dir="$DATA_DIR/node"
export node_data_dir="$node_dir/data"

# shellcheck source=./scripts/docker/entrypoint.inc.sh
. "$bin_dir/entrypoint.inc.sh"

command=${1:-octez-node}
shift 1

case $command in
    tezos-*)
        >&2 echo "Warning: The executable with name $command has been renamed to $(echo "$command" | sed 's/^tezos-/octez-/'). The name $command is now deprecated, and it will be removed in a future release. Please update your scripts to use the new name."
        ;;
esac

case $command in
    octez-node|tezos-node)
        launch_node "$@"
        ;;
    octez-upgrade-storage|tezos-upgrade-storage)
        upgrade_node_storage
        ;;
    octez-snapshot-import|tezos-snapshot-import)
        snapshot_import "$@"
        ;;
    octez-baker|tezos-baker)
        launch_baker "$@"
        ;;
    octez-baker-test|tezos-baker-test)
        launch_baker_test "$@"
        ;;
    tezos-endorser)
        launch_endorser "$@"
        ;;
    octez-endorser-test|tezos-endorser-test)
        launch_endorser_test "$@"
        ;;
    tezos-accuser)
        launch_accuser "$@"
        ;;
    tezos-accuser-test)
        launch_accuser_test "$@"
        ;;
    octez-client|tezos-client)
        configure_client
        exec "$client" "$@"
        ;;
    octez-admin-client|tezos-admin-client)
        configure_client
        exec "$admin_client" "$@"
        ;;
    octez-signer|tezos-signer)
        exec "$signer" "$@"
        ;;
    *)
        cat <<EOF
Available commands:

The following are wrappers around the tezos binaries.
To call the tezos binaries directly you must override the
entrypoint using --entrypoint . All binaries are in
$BIN_DIR and the tezos data in $DATA_DIR

You can specify the network with argument --network, for instance:
  --network carthagenet
(default is mainnet).

Daemons:
- octez-node [args]
  Initialize a new identity and run the tezos node.

- octez-baker [keys]
- octez-baker-test [keys]
- tezos-endorser [keys]
- octez-endorser-test [keys]

Clients:
- octez-client [args]
- octez-signer [args]
- octez-admin-client

Commands:
  - octez-upgrade-storage
  - octez-snapshot-import [args]
    Import a snapshot. The snapshot must be available in the file /snapshot
    Using docker run, you can make it available using the command :
       docker run -v <yourfilename>:/snapshot tezos/tezos octez-snapshot-import
    <yourfilename> must be an absolute path.
EOF
        ;;
esac

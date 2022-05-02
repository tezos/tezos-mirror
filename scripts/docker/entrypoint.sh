#!/bin/sh

set -e

bin_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"

: "${BIN_DIR:="/usr/local/bin"}"
: "${DATA_DIR:="/var/run/tezos"}"

: "${NODE_HOST:="node"}"
: "${NODE_RPC_PORT:="8732"}"

: "${PROTOCOL:="unspecified-PROTOCOL-variable"}"

# export all these variables to be used in the inc script
export node="$BIN_DIR/tezos-node"
export client="$BIN_DIR/tezos-client"
export admin_client="$BIN_DIR/tezos-admin-client"
export baker="$BIN_DIR/tezos-baker-$PROTOCOL"
export endorser="$BIN_DIR/tezos-endorser-$PROTOCOL"
export accuser="$BIN_DIR/tezos-accuser-$PROTOCOL"
export signer="$BIN_DIR/tezos-signer"

export client_dir="$DATA_DIR/client"
export node_dir="$DATA_DIR/node"
export node_data_dir="$node_dir/data"

# shellcheck source=./scripts/docker/entrypoint.inc.sh
. "$bin_dir/entrypoint.inc.sh"

command=${1:-tezos-node}
shift 1

case $command in
    tezos-node)
        launch_node "$@"
        ;;
    tezos-upgrade-storage)
        upgrade_node_storage
        ;;
    tezos-snapshot-import)
        snapshot_import "$@"
        ;;
    tezos-baker)
        launch_baker "$@"
        ;;
    tezos-baker-test)
        launch_baker_test "$@"
        ;;
    tezos-endorser)
        launch_endorser "$@"
        ;;
    tezos-endorser-test)
        launch_endorser_test "$@"
        ;;
    tezos-accuser)
        launch_accuser "$@"
        ;;
    tezos-accuser-test)
        launch_accuser_test "$@"
        ;;
    tezos-client)
        configure_client
        exec "$client" "$@"
        ;;
    tezos-admin-client)
        configure_client
        exec "$admin_client" "$@"
        ;;
    tezos-signer)
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
- tezos-node [args]
  Initialize a new identity and run the tezos node.

- tezos-baker [keys]
- tezos-baker-test [keys]
- tezos-endorser [keys]
- tezos-endorser-test [keys]

Clients:
- tezos-client [args]
- tezos-signer [args]
- tezos-admin-client

Commands:
  - tezos-upgrade-storage
  - tezos-snapshot-import [args]
    Import a snapshot. The snapshot must be available in the file /snapshot
    Using docker run, you can make it available using the command :
       docker run -v <yourfilename>:/snapshot tezos/tezos tezos-snapshot-import
    <yourfilename> must be an absolute path.
EOF
        ;;
esac

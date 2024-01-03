#!/bin/sh

set -e

bin_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"

: "${BIN_DIR:="/usr/local/bin"}"
: "${DATA_DIR:="/var/run/tezos"}"

: "${NODE_HOST:="node"}"
: "${NODE_RPC_PORT:="8732"}"
# This is the bind address INSIDE the docker so as long as there
# is no explicit port redirection, it is not exposed to the
# outside world.
: "${NODE_RPC_ADDR:="[::]"}"

: "${PROTOCOL:="unspecified-PROTOCOL-variable"}"

# export all these variables to be used in the inc script
export node="$BIN_DIR/octez-node"
export client="$BIN_DIR/octez-client"
export admin_client="$BIN_DIR/octez-admin-client"
export baker="$BIN_DIR/octez-baker-$PROTOCOL"
export endorser="$BIN_DIR/octez-endorser-$PROTOCOL"
export accuser="$BIN_DIR/octez-accuser-$PROTOCOL"
export signer="$BIN_DIR/octez-signer"
export smart_rollup_node="$BIN_DIR/octez-smart-rollup-node"

export client_dir="$DATA_DIR/client"
export node_dir="$DATA_DIR/node"
export node_data_dir="$node_dir/data"
export smart_rollup_node_data_dir="$DATA_DIR/smart-rollup-node"

# shellcheck source=./scripts/docker/entrypoint.inc.sh
. "$bin_dir/entrypoint.inc.sh"

command=${1:-octez-node}
shift 1

case $command in
octez-node)
  launch_node "$@"
  ;;
octez-upgrade-storage)
  upgrade_node_storage
  ;;
octez-snapshot-import)
  snapshot_import "$@"
  ;;
octez-baker)
  launch_baker "$@"
  ;;
octez-baker-test)
  launch_baker_test "$@"
  ;;
octez-endorser)
  launch_endorser "$@"
  ;;
octez-endorser-test)
  launch_endorser_test "$@"
  ;;
octez-accuser)
  launch_accuser "$@"
  ;;
octez-accuser-test)
  launch_accuser_test "$@"
  ;;
octez-client)
  configure_client
  exec "$client" "$@"
  ;;
octez-admin-client)
  configure_client
  exec "$admin_client" "$@"
  ;;
octez-signer)
  exec "$signer" "$@"
  ;;
octez-smart-rollup-node)
  launch_smart_rollup_node "$@"
  ;;
*)
  cat << EOF
Available commands:

The following are wrappers around the octez binaries.
To call the octez binaries directly you must override the
entrypoint using --entrypoint . All binaries are in
$BIN_DIR and the tezos data in $DATA_DIR

You can specify the network with argument --network, for instance:
  --network carthagenet
(default is mainnet).

Daemons:
- octez-node [args]
  Initialize a new identity and run the octez node.

- octez-smart-rollup-node [args]
  Run the octez smart rollup node.

- octez-baker [keys]
- octez-baker-test [keys]
- octez-endorser [keys]
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

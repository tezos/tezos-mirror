This directory contains some script and Dockerfile used to start a evm
sequencer rollup.

This document does not explain how smart rollup, smart rollup node,
evm node and the sequencer kernel works.

The following directory allows to initialise an octez-node on a
specified network, originate a new evm rollup and start a rollup and
sequencer node.

The script assume the use of only 1 operator key for the rollup node.

First step is to create an `.env` containing all necessary variables:

```
# tag to use for the tezos docker. default to `master`
OCTEZ_TAG=${OCTEZ_TAG:-"master"}

# directory where all data dir are place, default to `./data`
HOST_TEZOS_DATA_DIR=${HOST_TEZOS_DATA_DIR:-$PWD/data}

# network used to initialized the octez node configuration
TZNETWORK=${TZNETWORK:-"https://teztnets.xyz/ghostnet"}
# snapshot to use to start the rollup node
SNAPSHOT_URL=${SNAPSHOT_URL:-"https://snapshots.eu.tzinit.org/ghostnet/full"}
# endpoint to use to originate the smart rollup.
# it could be possible to use the local node but it
# would require then to first start the octez-node sepratatly from the docker compose.
ENDPOINT=${ENDPOINT:-"https://rpc.ghosnet.teztnets.xyz"}

# alias to use for for rollup node operator default acount.
OPERATOR_ALIAS=${OPERATOR_ALIAS:-"operator"}
# alias to use for for rollup.
ROLLUP_ALIAS=${ROLLUP_ALIAS:-"evm_rollup"}
# the used mode for the rollup node
ROLLUP_NODE_MODE=${ROLLUP_NODE_MODE:-"operator"}
```

Then when the variables are defined, or default value is valid you can initialise the octez node with:
```
./init.sh init_octez_node
```

This initialise the octez-node configuration, download the snapshot
and import it.

Last step before running the docker compose is to bootstrap the rollup environment:
```
./init.sh init_rollup
```
This generate a new account, wait until the address has enough tz.
Then it build the evm kernel and originate a new rollup with it.
And finally initialise the rollup node configuration.


then start all node:
```
docker compose up
```

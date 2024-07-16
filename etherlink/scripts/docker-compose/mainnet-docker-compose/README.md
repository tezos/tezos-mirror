This directory contains a script and docker compose file to be used to
start an EVM node in proxy mode.

This document does not explain how smart rollup, smart rollup node,
evm node and the kernel works.

The following directory allows to initialise an octez-node on a
specified network, a rollup node using a snapshot and start an evm
node based on the rollup node data dir.

The script `init.sh` is here to take care of every necessary steps to
initialised all the node states. It can be called with
`./init.sh`. This call will bootstrap the 3 necessary nodes:
- octez-node, that follows the tezos chain
- octez-smart-rollup-node, that follows the rollup 'etherlink'
- octez-evm-node, that follows the evm chain 'etherlink'

The `.env` file defines the default value necessary to run the docker
compose. By default it sets the data of all nodes in the directory
`$PWD/.etherlink-mainnet-data`, but this can be parameterized
with the environment variable `HOST_TEZOS_DATA_DIR`.

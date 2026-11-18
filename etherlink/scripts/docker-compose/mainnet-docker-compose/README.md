This directory contains a script, a Docker Compose and an environment
file to start following the etherlink chain.

Note: This document does not explain how the smart rollup, smart
rollup node, EVM node, or the kernel works.

The provided docker-compose.yml assumes that the nodes have their data
directories initialized (with the configuration and snapshot
imported).

The script init.sh handles the initialization of all node states using
snapshots. You can run it with ./init.sh. This will bootstrap the
three necessary nodes:

    octez-node, which follows the Tezos mainnet chain.
    octez-smart-rollup-node, which follows the smart rollup 'etherlink'.
    octez-evm-node, which follows the EVM mainnet chain 'etherlink'.

The .env file defines the default values necessary to run the Docker
Compose setup. By default, it stores the node data in the directory
$PWD/.etherlink-mainnet-data, but this can be customized with the
environment variable HOST_TEZOS_DATA_DIR.

Refer to the .env file to check any configurable variables for the EVM
node. If an option is not yet covered, you can explore the available
configuration with `docker-compose run observer describe config`, or
view command-line options using `docker-compose run observer run
--help`.

At the time of this file’s last update, the following versions are
used for each binary:

    Octez-evm-node: Octez EVM node release 0.12
    Octez-node: Octez Release 21

The smart rollup node uses the ``octez-smart-rollup-node-latest`` tag, which
always points at the most recent rollup node release. Please ensure you use the
latest available versions for the other components and update the tags in the
.env file accordingly.

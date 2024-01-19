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
# network to use
# warning: date dependent variables won't be correctly interpreted in compose.yml
TZNETWORK=${TZNETWORK:-"ghostnet"}

# tag to use for the tezos docker. default to `master`
OCTEZ_TAG=${OCTEZ_TAG:-master}

# directory where all data dir are placed, default to `./.etherlink-${TZNETWORK}-data`
HOST_TEZOS_DATA_DIR=${HOST_TEZOS_DATA_DIR:-"$PWD/.etherlink-$TZNETWORK-data"}

# network used to initialize the octez node configuration
TZNETWORK_ADDRESS=${TZNETWORK_ADDRESS:-"https://teztnets.com/$TZNETWORK"}

# snapshot to use to start the octez node
SNAPSHOT_URL=${SNAPSHOT_URL-"https://snapshots.eu.tzinit.org/$TZNETWORK/rolling"}

# address of faucet to use with @tacoinfra/get-tez
FAUCET=${FAUCET:-"https://faucet.$TZNETWORK.teztnets.com"}

# endpoint to use to originate the smart rollup.
# it could be possible to use the local node but it
# would require then to first start the octez-node sepratatly from the docker compose.
ENDPOINT=${ENDPOINT:-"https://rpc.$TZNETWORK.teztnets.com"}

## Contract options

# alias to use for the exchanger
EXCHANGER_ALIAS=${EXCHANGER_ALIAS-"exchanger"}
# alias to use for the bridge
BRIDGE_ALIAS=${BRIDGE_ALIAS-"bridge"}
# alias to use for the delayed bridge
DELAYED_BRIDGE_ALIAS=${DELAYED_BRIDGE_ALIAS-"delayed_bridge"}
# alias to use for the evm admin contract
EVM_ADMIN_ALIAS=${EVM_ADMIN_ALIAS-"evm_admin"}
# alias to use for the evm admin contract
SEQUENCER_ADMIN_ALIAS=${SEQUENCER_ADMIN_ALIAS-"sequencer_admin"}

## Rollup options

# alias to use for for rollup node operator default acount.
OPERATOR_ALIAS=${OPERATOR_ALIAS:-"operator"}
MINIMUM_OPERATOR_BALANCE=${MINIMUM_OPERATOR_BALANCE:-10000}
# alias to use for the address that originate the rollup. Different from
# the operator to prevent some failure with 1M when reseting the rollup node.
ORIGINATOR_ALIAS=${ORIGINATOR_ALIAS:-"originator"}
# alias to use for rollup.
ROLLUP_ALIAS=${ROLLUP_ALIAS:-"evm_rollup"}
# the used mode for the rollup node
ROLLUP_NODE_MODE=${ROLLUP_NODE_MODE:-"batcher"}
# the chain_id
EVM_CHAIN_ID=${EVM_CHAIN_ID:-123123}
# ethereum accounts
EVM_ACCOUNTS=("6ce4d79d4e77402e1ef3417fdda433aa744c6e1c" "b53dc01974176e5dff2298c5a94343c2585e3c54" "9b49c988b5817be31dfb00f7a5a4671772dcce2b")
# sequencer address alias
SEQUENCER_ALIAS=${SEQUENCER_ALIAS:-"sequencer"}
# sequencer secret key
SEQUENCER_SECRET_KEY=${SEQUENCER_SECRET_KEY:-"edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh"}
# sequencer kernel config base file
SEQUENCER_CONFIG=${SEQUENCER_CONFIG:-"$PWD/evm_config.yaml"}
```

You can you the dailynet by only setting `TZNETWORK` and removing `SNAPSHOT_URL`:
```
export TZNETWORK="dailynet-$(date +%Y-%m-%d)"
export SNAPSHOT_URL=""
```

Then when the variables are defined, or default value is valid you can initialise the octez node with:
```
./init.sh init_octez_node
```
This initialise the octez-node configuration, download the snapshot
and import it.

If you need the tezos contracts to be deployed, you can run the command:
```
./init.sh originate_contracts
```
By default, this originates the exchanger contract, the bridge contract, the evm admin contract and the sequencer admin contract. It will also update the kernel config.
If you don't want to include one of the contracts, you cat set their alias to `""`.
You can modify the base kernel config by modifying the file `evm_config.yaml` (or the file given with `${SEQUENCER_CONFIG}`).

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

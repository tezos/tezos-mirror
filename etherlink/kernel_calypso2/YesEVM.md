# YesEVM

This document explains how you can run a proper manual upgrade/migration test starting from the **exact** state of the EVM rollup at the wanted level with the help of **Yes-node/wallet**.

## Preliminary steps

- Retrieve a full snapshot of the desired network and run a regular `octez-node` on it.

- Run a rollup node for the desired EVM rollup (with the correct preimages):

```
> octez-smart-rollup-node init operator config for <rollup-address> with operators <operator-alias> --data-dir ~/.evm-test/rollup-node
> octez-smart-rollup-node run observer for <rollup-address> with operators <operator-alias> --data-dir ~/.evm-test/rollup-node --rpc-port 8932
```

To ensure everything works properly, both the node and the rollup node will need to be on the same L1 level:

- Stop the node. Run it back with `[--connections 0]` to put it in a waiting mode. Get the L1 level with the `octez-client` and then stop the node. The rollup won't receive new blocks and wait at the same level, and can now be safely stopped.

- Create a snapshot of the node at the retrieved level.

> octez-node snapshot export --block <retrieved-level> --data-dir ~/evm-test/ghostnet-node

- Get a "snapshot" of the EVM rollup (basically all its data directory) and store it somewhere.

## Setup and use a yes-node/wallet

Fork the chain to make the tests in our deviated chain.

- Setup the yes-node/wallet on ghostnet:

```
# TODO: remove the following line. Should hopefully be merged to master soon, in the meantime:
> git checkout -B yes-evm
> git cherry-pick julien@yes-wallet-multi-network
> make
> ./scripts/patch-yes_node.sh
> cp octez-client octez-client-ok # octez-client compiled with the yes patches won't be able to actually inject transactions
> make octez-node octez-client
```

```
> octez-node config init --data-dir ~/.yes-node --synchronisation-threshold 0 --connections 0 --rpc-addr localhost:8732 --network <network>
```

```
> octez-node snapshot import <snapshot-file> --data-dir ~/.yes-node
```

```
Retrieve the list of bakers, here is an example for Ghostnet:

> curl https://api.ghostnet.tzkt.io/v1/delegates\?limit\=5000 | jq 'map(select ((.alias?) and .active) |{ "alias" : .alias, "address" : .address , "publicKey" : .publicKey})' > aliases.json
```

```
> dune exec devtools/yes_wallet/yes_wallet.exe -- create from context ~/.yes-node in ~/.yes-wallet --active-bakers-only --aliases aliases.json --network <network-name>
```

```
> octez-node run --data-dir ~/.yes-node
```

To push the chain forward:

```
> octez-client -d ~/.yes-wallet bake for --minimal-timestamp
```

Run a new EVM rollup node with the exported "snapshot" of the EVM rollup. Don't use the binary compiled with the __yes__ patches, as they will break the signature and the batcher of the rollup node won't be able to send operations.

Once this is done this network acts as a sandbox with the same values as the initial network you used. This means you can safely test whatever you want from your forked chain/EVM rollup (injecting transactions, applying upgrades to check they don't break the rollup, etc) while benefit from the tooling (indexers, wallets, etc).

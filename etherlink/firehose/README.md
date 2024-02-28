<!--
SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>

SPDX-License-Identifier: MIT
-->

# Firehose

A tool for stress-testing etherlink TPS.

## Setup

> If running from source, a handy alias is: `alias firehose='cargo run -q --release --'`

Firehose requires an Ethereum-like RPC endpoint, and a controller address. We assume the ghostnet instance of
Etherlink throughout, but other instances - such as _etherlink-nightly_, should work.

```sh
$ firehose configure --endpoint "https://node.ghostnet.etherlink.com"
```

This will initialise the Firehose configuration at `$XDG_CONFIG_DIR/etherlink/firehose.json` by default. In addition it generates a _controller_ address. This account will be used as the orchestrator for subsequent commands.

> You are free to override the controller account, and indeed the config location. See `firehose help` and `firehose configure help`, for more options.

You can ensure Firehose has been set up correctly by running:

```sh
$ firehose status
chain_id                128123
gas_price               0.050000000     Gwei

controller_address      0xf428d9185789644753d28afdbeef6f7bb0a00f85
controller_balance      0.000000000000000000    XTZ
```

Your output should look similar to the above - you can see Firehose is fetching information from the RPC endpoint correctly.

## Deposit

In order to run scenarios, we need the controller to be funded. We can do so with the following command:

```sh
$ octez-client --endpoint <L1_ENDPOINT> \
    transfer <amount> \
    from <l1_address> \
    to <bridge_contract> \
    --entrypoint deposit \
    --arg 'Pair "<etherlink rollup address>" <controller_address>'
```

After a short time, `firehose status` should indeed show an increase in the `controller_balance`.

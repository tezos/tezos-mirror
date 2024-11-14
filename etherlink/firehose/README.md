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

## Transfer

Firehose can perform transfers from the controller account to any other.
For simple transfers, you can select `xtz`, `gwei` & `wei` as units.

As an example:

```sh
$ firehose transfer --amount 1.2 gwei --to 0x63Ec92990F469bC0DD03516E0d32Ee9A4D8Ff214
0x63ec…f214 funded with 1 | hash: 0x9ef484b4ce9b8223040cde139b582cfcfb55f29cce89fb8f4017c30ef2fbed90
```

## Stress-testing

Firehose can perform stress-test scenarios, with the `flood` subcommand. The parameter `--workers` adjusts the number
of concurrent transfers that will be attempted, and will continue until the program is exited. `--random-nonce` assigns a random(incorrect) nonce to the transactions

Both `xtz` and `erc20` transfers are currently supported. `large-payload` attachs an 100MB payload to each xtz transfer

*NB* the setup step can be slightly flaky, you may need to re-attempt multiple times to get to the point of the
stress-test occuring.

For more information, see `firehose flood --help`.

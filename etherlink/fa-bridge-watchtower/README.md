FA bridge watchtower
======

### What

The FA bridge watchtower monitors
`QueuedDeposit(uint256,address,uint256,uint256,uint256)` topic and
claims deposits if found.

A whitelist can be provided to filter on the proxy adresses and specific ticket hashes.

### How

Build the binary as follows.

```
make fa-bridge-watchtower
```

Create a data directory for the FA bridge watchtower if desired (default is `$HOME/.fa-bridge-watchtower`).

Create a configuration file in the data directory as `<data_dir>/config.json`.
If not provided, the following configuration will be used by default.

```
{
  "evm_node_endpoint": "http://localhost:8545",
  "gas_limit": "1000000",
  "max_fee_per_gas": "100000000000",
}
```

Example of configuration for the watchtower running on rainbownet is
the following, where the watchtower only tracks tokens for the
[LKC](http://vps-43b6dfcb.vps.ovh.net/token/0xE8b8716743dC84fE31bfaF7511dD9EC1Fa9A37A6)
contract and specific ticket hashes.

```
{
  "evm_node_endpoint": "http://localhost:8565",
  "gas_limit": "1000000",
  "max_fee_per_gas": "100000000000",
  "rpc": {
    "addr": "127.0.0.1",
    "port": 4002
  },
  "secret_key": "0x...",
  "whitelist": [
    {
      "proxy": "0xe8b8716743dc84fe31bfaf7511dd9ec1fa9a37a6",
      "ticket_hashes": [
        "0x588b32ab610954bd36c6561f326abae2c149ae9abe780f92422cb57178eedcb3"
      ]
    }
  ]
}
```

To check for the `run` command documentation.


```
$ ./fa-bridge-watchtower man run
...
  run [-d --data-dir <path>] [-v --verbose] [--debug] [--evm-node <URL>] [--secret-key <0x...>]
      [--first-block <number>]
    Start FA bridge watchtower
    -d --data-dir <path>: Directory where data is stored for the FA bridge watchtower.
      Defaults to `/home/phink/.fa-bridge-watchtower`.
    -v --verbose: Sets logging level to info
    --debug: Sets logging level to debug
    --evm-node <URL>: Endpoint to reach EVM node. Websocket must be available on `/ws`.
    --secret-key <0x...>: secret_key
      If set, defaults to the value of FA_BRIDGE_WATCHTOWER_SK.
    --first-block <number>: First block number from which to look at deposits when running for the first
      time.
```

To run the watchtower, please note that the secret key is mandatory
and can be either provided by the `FA_BRIDGE_WATCHTOWER_SK`
environment variable, the CLI arg `--secret-key` or the configuration
file, in priority order.


```
./fa-bridge-watchtower run
```

To check that the watchtower is working as expected, one can either have a look at the logs (located under `<data_dir>/daily_logs/`)
and should observe the following, or directly look at the explorer and check for a token transfer following the deposit transaction.

```
May 07 08:38:38 etherlink-pinata-sequencer fa-bridge-watchtower[2984082]: May 07 08:38:38.502 NOTICE │ Deposit 10 1.23e-06
May 07 08:38:38 etherlink-pinata-sequencer fa-bridge-watchtower[2984082]: May 07 08:38:38.502 NOTICE │
May 07 08:38:38 etherlink-pinata-sequencer fa-bridge-watchtower[2984082]:
May 07 08:38:38 etherlink-pinata-sequencer fa-bridge-watchtower[2984082]: [4B blob data]
May 07 08:38:38 etherlink-pinata-sequencer fa-bridge-watchtower[2984082]: May 07 08:38:38.502 NOTICE │   to 0x0ce0ae67860ef01ec25922c7ac44aa6002a398ba in transaction
May 07 08:38:38 etherlink-pinata-sequencer fa-bridge-watchtower[2984082]: May 07 08:38:38.502 NOTICE │   e3d8a4b63639f2500c9daaf6a486625a23c7066d8770e6f266eceea850ec3532(0) of
May 07 08:38:38 etherlink-pinata-sequencer fa-bridge-watchtower[2984082]: May 07 08:38:38.502 NOTICE │   block 204509
May 07 08:38:38 etherlink-pinata-sequencer fa-bridge-watchtower[2984082]: May 07 08:38:38.511 NOTICE │ There are 1 unclaimed deposits
May 07 08:38:38 etherlink-pinata-sequencer fa-bridge-watchtower[2984082]: May 07 08:38:38.511 NOTICE │ Claiming deposit 10
May 07 08:38:39 etherlink-pinata-sequencer fa-bridge-watchtower[2984082]: May 07 08:38:39.023 NOTICE │ New etherlink head 204510 (1 txs)
May 07 08:38:39 etherlink-pinata-sequencer fa-bridge-watchtower[2984082]: May 07 08:38:39.028 NOTICE │ Claimed deposit 10 in transaction
May 07 08:38:39 etherlink-pinata-sequencer fa-bridge-watchtower[2984082]: May 07 08:38:39.028 NOTICE │   1130148a0e5588e32f4e0c3b143334488cb823c9ced9914cfa13b0606b7ad8dc(0) of
May 07 08:38:39 etherlink-pinata-sequencer fa-bridge-watchtower[2984082]: May 07 08:38:39.028 NOTICE │   block 204510
```

### RPC

If a RPC configuration is provided in the configuration file, a RPC
server is launch along the watchtower providing the following `GET`
endpoints.

- `/health`: simple health check.
- `/config`: displays the configuration used by the fa bridge watchtower. Does not display the secret key.
- `/deposits`: displays deposits and corresponding claimed data.
- `/unclaimed`: displays unclaimed deposits.

For instance, one can inspect the deposits as follows assuming the
configured RPC address is `127.0.0.1` and RPC port is `4002`.

```
$ curl -s http://localhost:4002/deposits | jq
[
  {
    "nonce": "57",
    "proxy": "0xe8b8716743dc84fe31bfaf7511dd9ec1fa9a37a6",
    "ticket_hash": "0x588b32ab610954bd36c6561f326abae2c149ae9abe780f92422cb57178eedcb3",
    "receiver": "0x0ce0ae67860ef01ec25922c7ac44aa6002a398ba",
    "amount": 1E-8,
    "token": "\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000 \u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0003LKC\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000",
    "transactionHash": "0xeb238c94cd634e0c2e3633175370030cd9d9edc06b6bec773783b9b2500564e1",
    "transactionIndex": "0",
    "logIndex": "0",
    "blockHash": "0x989761958584b15436a1409a4cab3430efbba36e26c6b092974f93717b9ae6db",
    "blockNumber": "234757",
    "claimed": {
      "transactionHash": "0x711b2bbd8081ebd9577feab622dc65315808614ac5f6491ba3f4908c0e8e206d",
      "transactionIndex": "0",
      "blockHash": "0xc2933f5985d4add652d22b8af2c73446a62c13ae10164af42be3673a70f44f83",
      "blockNumber": "234758"
    }
  },
  ..
```

### Requirements

- The secret key is mandatory and can be either provided by the `FA_BRIDGE_WATCHTOWER_SK` environment variable, the CLI arg `--secret-key` or the configuration file, in priority order.
- Websocket must be available on the evm node endpoint.

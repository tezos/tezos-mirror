# Benchmark data generation

## General methodology

The goal was to be able to describe benchmark scenarios with an API making the
intent of the scenario as transaparent of possible, by removing as much
bookeeping and ethereum details as possible.

The following scripts output 10 transfers between player1 and player2:

```
let txs = [];
// player1 get 10_000 from faucet
txs.push(utils.transfer(faucet, player1, 10000))
// 10 transfers of 1_000 from player1 to player2
for (let i = 0; i < 10; i++) {
    txs.push(utils.transfer(player1, player2, 1000))
}

utils.print_bench([txs])
```

The solidity source code of the smart contracts involved in the scenarios are
available in `./contracts`.

## How to use

- pick a scenario
- execute the script using node.js runtime

```
node bench_storage_1.js > inbox_bench_storage_1.json
```

Expected result is a file `inbox_bench_storage_1.json` that can be used as input
for the wasm debugger.

- execute the debugger on the kernel_benchmark using the inbox.

```
$ ./octez-smart-rollup-wasm-debugger --kernel kernel_benchmark.wasm --inputs inbox_bench_storage_1.json
> load inputs
Loaded 11 inputs at level 0
> bench
Ran for 15 kernel_run calls:
46962960 ticks in 15.270356 seconds
...
46855122 ticks in 19.860593 seconds
>

```

## How to write a scenario

### Players

The utilities for creating transactions use structs to aggregate wallet infos

```
{
    "addr": ...,
    "privateKey": ...,
    "publicKey": ...,
    "nonce": ...
}
```

Note that the nonce is included, so the same object should be used for
successive transactions during one scenario.

A few precalculated ones are available as `json` files, including the faucet
hardcoded in the kernel.

```
./players/player1.json
./players/player2.json
./players/faucet.json
```

New players can be created on the fly using utilities from `../lib/address.js`.
Note that the private keys are randomly generated.

```
const addr = require('../lib/address'); // import the utilities
let new_player = addr.create_player();  // randomly generate new player
```

### Transactions

#### struct

Utilities expect the transaction data to be bundled in a struct:

```
{
    "nonce": 0,
    "gasPrice": 30000000000,
    "gasLimit": "0x7FFFFFFFFFFFFFFF",
    "to": "0x4e1b2c985d729ae6e05ef7974013eeb48f394449",
    "value": 1000000000,
    "data": "",
    "chainId": 1,
    "v": 1,
    "r": 0,
    "s": 0
}
```

Those can be generated and don't need to be edited by hand. They should be
stored in list in order of creation to respect the nonce increment, for later
output.

#### Transfer

The file `utils.js` exports a function `send(sender,recipient,amount)` to create
simple transfer transactions.

```
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');
let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000))

```

#### Contract creation

The file `utils.js` exports a functions

- `create(player,initial_amount,crate_data)` to create contracts.
  The `create_data` is the contract initialisation bytecode as a hex string
  starting with `0x`.
- `send(player, contract_addr, amount, call_data)` to make contract calls.
  The `call_data` is a the call bytecode as a hex string strating with `0x`.

```
let contract = utils.create(player1, 0, create_data);    // {tx = "raw transaction"; addr = "address of creation"}
txs.push(create.tx)                                      // adds the creation transaction
txs.push(utils.send(player1, create.addr, 0, call_data)) // make a call to the contract address

```

### Output

The file `utils.js` exports a functions to output on stdout in the format
expected by the debugger.

```
utils.print_bench([inbox_level1 ; inbox_level2])
```

It expect a list of list of transactions.

Note that SOL, metadata and EOL are added by the debugger.

// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

// usage: node benchmarks/bench_erc1155.js

const utils = require('./utils');
const { contracts_directory, compile_contract_file, find_contract } = require("../lib/contract");
const path = require("path");
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');
let player2 = require('./players/player2.json');

let mode = utils.bench_args(process.argv, [{flag: '--count <n>', desc: 'Number of transfers', default: 0}]);

let nb_iter = mode.extra.count > 2 ? mode.extra.count : 1;
if (nb_iter < 0) nb_iter = 0;

// Using `MyMultiToken`
let contracts = compile_contract_file(contracts_directory, 'erc1155.sol');
let contract = find_contract(contracts, "MyMultiToken");
let create_data = contract.bytecode;

let number_of_tokens = 100;
let initial_value = 100000;

// Currently takes 2154662 gas
let batchMint_data =
    contract.interface.encodeFunctionData(
        "batchMint",
        [
            Array.from({ length: number_of_tokens }, (_v, i) => i),
            Array.from({ length: number_of_tokens }, (_v, _i) => initial_value),
            "0x"
        ]
    );

// Currently takes 2503047 gas
let safeBatchTransferFrom_data =
    contract.interface.encodeFunctionData(
        "safeBatchTransferFrom",
        [
            player1.addr,
            player2.addr,
            Array.from({ length: number_of_tokens }, (_v, i) => i),
            Array.from({ length: number_of_tokens }, (_v, _i) => Math.floor(initial_value / (1 + nb_iter))),
            "0x"
        ]);

// Safe margin to ensure the scenario still works with changes in the gas
// accounting of the kernel
let gasLimit = 3_000_000;

let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000000));
let create = utils.create(player1, 0, create_data, { gasLimit: 1000000000 });
txs.push(create.tx);
txs.push(utils.send(player1, create.addr, 0, batchMint_data, {gasLimit}));
for (let i = 0; i < nb_iter; i++) {
    txs.push(utils.send(player1, create.addr, 0, safeBatchTransferFrom_data, {gasLimit}));
}
utils.print_bench([txs], mode);

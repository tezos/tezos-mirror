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

let nb_iter = process.argv.length > 2 ? process.argv[2] : 1;
if (nb_iter < 0) nb_iter = 0;

// Using `MyMultiToken`
let contracts = compile_contract_file(contracts_directory, 'erc1155.sol');
let contract = find_contract(contracts, "MyMultiToken");
let create_data = contract.bytecode;

// Safe margin to ensure the scenario still works with changes in the gas
// accounting of the kernel
let gasLimit = 3_000_000;

let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000000));
for (let i = 0; i < nb_iter; i++) {
let create = utils.create(player1, 0, create_data);
txs.push(create.tx);
}
utils.print_bench([txs]);

// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// usage: node benchmarks/bench_creates_erc20.js
// several consecutive creates

const utils = require('./utils');
const { contracts_directory, compile_contract_file } = require("../lib/contract");
const path = require("path");
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');

let erc20_contract = compile_contract_file(contracts_directory, "erc20tok.sol")[0];
let create_data = erc20_contract.bytecode;

let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000000));
for (let index = 0; index < 10; index++) {
    let create = utils.create(player1, 0, create_data);
    txs.push(create.tx);
}

let mode = utils.bench_args(process.argv);

utils.print_bench([txs], mode)

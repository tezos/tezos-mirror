// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// usage: node benchmarks/bench_loop.js

// create contract and call it
// "loop.sol" example

const utils = require('./utils');
const addr = require('../lib/address');
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');
const { contracts_directory, compile_contract_file } = require("../lib/contract");

let contract = compile_contract_file(contracts_directory, 'loop.sol')[0];

let create_data = contract.bytecode;

// create call data with different values
let call_data = function (n) {
    return contract.interface.encodeFunctionData("loop", [n]);
}

let txs = [];

// initialisation
txs.push(utils.transfer(faucet, player1, 100000000))
let create = utils.create(player1, 0, create_data)
txs.push(create.tx)

let max = 100
// send calls
for (let i = 0; i <= max; i++) {
    txs.push(utils.send(player1, create.addr, 0, call_data(i)))
}


utils.print_bench([txs])

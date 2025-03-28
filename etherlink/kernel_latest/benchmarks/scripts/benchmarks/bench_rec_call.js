// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// usage: node benchmarks/bench_rec_call.js

// create contract and call it
// "rec_call.sol" example
// infinite recursive call, to reach max stack size

const utils = require('./utils');
const addr = require('../lib/address');
const { contracts_directory, compile_contract_file } = require("../lib/contract");
let faucet = require('./players/faucet.json');

let contract = compile_contract_file(contracts_directory, "rec_call.sol")[0];
let create_data = contract.bytecode;
let call_data = contract.interface.encodeFunctionData("call", []);

let player1 = require('./players/player1.json');

let txs = [];

// initialisation
txs.push(utils.transfer(faucet, player1, 100000000))
let create = utils.create(player1, 0, create_data)
txs.push(create.tx)

txs.push(utils.send(player1, create.addr, 0, call_data))

let mode = utils.bench_args(process.argv);

utils.print_bench([txs], mode)

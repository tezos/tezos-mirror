// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

// Usage: node benchmarks/bench_imbricated_calls.js
// The source code of the imbricated call contract can be found in
// etherlink/kernel_latest/solidity_examples/call_tracer_depth.sol

const utils = require('./utils');
const { contracts_directory, compile_contract_file } = require("../lib/contract");
let faucet = require('./players/faucet.json');
let account = require('./players/player1.json');

let contract = compile_contract_file(contracts_directory, "call_tracer_depth.sol")[3];
let create_data = contract.bytecode;

const start_imbricated_calls = function () {
    return contract.interface.encodeFunctionData("startDepth")
}

let txs = [];
txs.push(utils.transfer(faucet, account, 100000000000));
let create = utils.create(account, 0, create_data, {gasLimit: 2_600_000});
txs.push(create.tx);
txs.push(utils.send(account, create.addr, 0, start_imbricated_calls()));

let mode = utils.bench_args(process.argv);

utils.print_bench([txs], mode)

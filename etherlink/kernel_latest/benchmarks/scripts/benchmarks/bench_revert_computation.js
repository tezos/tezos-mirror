// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

// Usage: node benchmarks/bench_revert_computation.js
// The source code of the revert computation contract can be found in
// etherlink/kernel_latest/benchmarks/scripts/benchmarks/contracts/revert_computation.sol

const utils = require('./utils');
const { contracts_directory, compile_contract_file } = require("../lib/contract");
let faucet = require('./players/faucet.json');
let account = require('./players/player1.json');

let contract = compile_contract_file(contracts_directory, "revert_computation.sol")[0];
let create_data = contract.bytecode;

const add_to_five = function (value) {
    return contract.interface.encodeFunctionData("addToFive", [value])
}

let txs = [];
txs.push(utils.transfer(faucet, account, 100000000000));
let create = utils.create(account, 0, create_data, {gasLimit: 2_600_000});
txs.push(create.tx);
txs.push(utils.send(account, create.addr, 0, add_to_five(4)));
txs.push(utils.send(account, create.addr, 0, add_to_five(6)));

let mode = utils.bench_args(process.argv);

utils.print_bench([txs], mode)

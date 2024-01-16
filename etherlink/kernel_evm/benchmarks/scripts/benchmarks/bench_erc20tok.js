// SPDX-FileCopyrightText: 2023 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

// usage: node benchmarks/bench_erc20tok.js
// The source code of the ERC-20 Token contract can be found in etherlink/kernel_evm/solidy_examples/erc20tok.sol

const utils = require('./utils');
const { contracts_directory, compile_contract_file } = require("../lib/contract");
const path = require("path");
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');
let player2 = require('./players/player2.json');

let contract = compile_contract_file(contracts_directory, "erc20tok.sol")[0];
let create_data = contract.bytecode;

const transfer_token = function (to, amount) {
    return contract.interface.encodeFunctionData("transfer", [to.addr, amount])
}

const mint = function (amount) {
    return contract.interface.encodeFunctionData("mint", [amount])
}

const burn = function (amount) {
    return contract.interface.encodeFunctionData("burn", [amount])
}

let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000000));
txs.push(utils.transfer(faucet, player2, 100000000000));
let create = utils.create(player1, 0, create_data);
txs.push(create.tx);
txs.push(utils.send(player1, create.addr, 0, mint(20000)));
txs.push(utils.send(player1, create.addr, 0, transfer_token(player2, 5000)));
txs.push(utils.send(player1, create.addr, 0, burn(15000)));
txs.push(utils.send(player2, create.addr, 0, burn(5000)));

var args = process.argv.slice(2)
if (args.length == 1 && args[0] == '--blueprint') {
    utils.print_bench([txs], { blueprint: true })
} else {
    utils.print_bench([txs])
}

// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// usage: node benchmarks/bench_linear_erc20.js <n>
// if no argument <n> is given, count as 0 erc20 transfers, so only:
// - provision of player1 address from faucet,
// - contract creation
// - minting of tockens

const utils = require('./utils');
const { contracts_directory, compile_contract_file } = require("../lib/contract");
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');
let player2 = require('./players/player2.json');

let txs = [];
let contract = compile_contract_file(contracts_directory, "erc20tok.sol")[0];
let create_data = contract.bytecode;

const transfer_token = function (to, amount) {
    return contract.interface.encodeFunctionData("transfer", [to.addr, amount])
}

const mint = function (amount) {
    return contract.interface.encodeFunctionData("mint", [amount])
}

// player1 get 10_000 from faucet
txs.push(utils.transfer(faucet, player1, 10000000))
let create = utils.create(player1, 0, create_data);
txs.push(create.tx);
txs.push(utils.send(player1, create.addr, 0, mint(20000)));
// 10 transfers of 1_000 from player1 to player2
for (let i = 0; i < process.argv[2]; i++) {
    txs.push(utils.send(player1, create.addr, 0, transfer_token(player2, 1)));
}

utils.print_bench([txs])

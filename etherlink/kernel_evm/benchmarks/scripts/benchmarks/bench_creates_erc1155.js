// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// usage: node benchmarks/bench_creates_erc1155.js
// several consecutive creates

const utils = require('./utils');
const { contracts_directory, compile_contract_file, find_contract } = require("../lib/contract");
const path = require("path");
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');
let player2 = require('./players/player2.json');

let erc1155_contracts = compile_contract_file(contracts_directory, 'erc1155.sol');
let erc1155_contract = find_contract(erc1155_contracts, "ERC1155");
let create_data = erc1155_contract.bytecode;

let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000000));
for (let index = 0; index < 10; index++) {
    let create = utils.create(player1, 0, create_data);
    txs.push(create.tx);
}


utils.print_bench([txs])

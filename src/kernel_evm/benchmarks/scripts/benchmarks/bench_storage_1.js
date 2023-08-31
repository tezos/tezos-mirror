// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// create contract and call it
// storage.sol

const utils = require('./utils');
const { contracts_directory, compile_contract_file } = require("../lib/contract");
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');
let player2 = require('./players/player2.json');
let contract = compile_contract_file(contracts_directory, "storage.sol")[0];
let create_data = contract.bytecode;
let send_36 = contract.interface.encodeFunctionData("set", [36]);
let send_42 = contract.interface.encodeFunctionData("set", [42]);

let txs = [];
// initialisation
// player 1 gets funds, then originate the contract, then stores 36
txs.push(utils.transfer(faucet, player1, 100000000))
let create = utils.create(player1, 0, create_data)
txs.push(create.tx)
txs.push(utils.send(player1, create.addr, 0, send_36))

// player 2 gets funds then stores 42
txs.push(utils.transfer(faucet, player2, 100000000))
txs.push(utils.send(player2, create.addr, 0, send_42))

utils.print_bench([txs])

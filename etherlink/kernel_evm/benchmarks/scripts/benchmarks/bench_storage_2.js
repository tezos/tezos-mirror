// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// create contract and call it
// example "storage.sol"

const utils = require('./utils');
const addr = require('../lib/address');
const { contracts_directory, compile_contract_file } = require("../lib/contract");
let faucet = require('./players/faucet.json');

let contract = compile_contract_file(contracts_directory, "storage.sol")[0];
let create_data = contract.bytecode;
// call : set(42)
let call_data = contract.interface.encodeFunctionData("set", [42]);
let nb_players = 10
let nb_calls = 5
let players = []
for (let i = 0; i < nb_players; i++) {
    players.push(addr.create_player())
}

let txs_1 = [];
// every player get the same amount
for (player of players) {
    txs_1.push(utils.transfer(faucet, player, 100000000))
}

// first player originates the contract
let create = utils.create(players[0], 0, create_data)
txs_1.push(create.tx)

let txs_2 = [];
// every player store 42 "nb_calls" times
for (player1 of players) {
    for (let index = 0; index < nb_calls; index++) {
        txs_2.push(utils.send(player1, create.addr, 0, call_data))
    }
}

let mode = utils.bench_args(process.argv);

// first set of messages: initialisation
// second set of messages: calls
utils.print_bench([txs_1.concat(txs_2)], mode)

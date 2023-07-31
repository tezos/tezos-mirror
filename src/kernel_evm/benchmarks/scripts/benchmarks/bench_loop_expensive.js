// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// usage: node benchmarks/test_reboot_loop.js
// or to produce raw list of transactions: node benchmarks/test_reboot_loop.js raw

const utils = require('./utils');
const addr = require('../lib/address');
let faucet = require('./players/tezt_bootstraped.json');
let player1 = require('./players/player1.json');
let player2 = require('./players/player2.json');
const { contracts_directory, compile_contract_file } = require("../lib/contract");

let contract = compile_contract_file(contracts_directory, 'loop.sol')[0];

let create_data = contract.bytecode;


const TOO_MANY_TXS = 100;
const NB_LOOP = 100;

// create call data with different values
let call_data_prefix = "0x0b7d796e00000000000000000000000000000000"
let call_data = function (n) {
    return call_data_prefix + utils.encode_number(n)
}

let txs = [];

// initialisation
txs.push(utils.transfer(faucet, player1, 100000000))
let create = utils.create(player1, 0, create_data)
txs.push(create.tx)

let players = []
// one player per transaction
// this make the scenario work with the proxy, which now (temporarily) forces
// 1 tx max per account
for (let i = 0; i < TOO_MANY_TXS; i++) {
    players.push(addr.create_player())
}

for (let i = 0; i < TOO_MANY_TXS; i++) {
    txs.push(utils.send(players[i], create.addr, 0, call_data(NB_LOOP)))
}

var args = process.argv.slice(2)

if (args.length == 1 && args[0] == "raw") {
    utils.print_raw_txs(txs)
} else {
    utils.print_bench([txs])
}

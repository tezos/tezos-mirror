// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// usage: node benchmarks/test_reboot_loop.js
// or to produce raw list of transactions: node benchmarks/test_reboot_loop.js raw

const utils = require('./utils');
const addr = require('../lib/address');

let player1 = require('./players/player1.json');
const { contracts_directory, compile_contract_file } = require("../lib/contract");

const transfer_prototype_json = require('./transfer_prototype.json');

var faucet;
var args = process.argv.slice(2)
if (args.length == 1) {
    // for tests
    faucet = require('./players/tezt_bootstraped.json');
} else {
    // for benchmarking
    faucet = require('./players/faucet.json');
}

let contract = compile_contract_file(contracts_directory, 'loop.sol')[0];

let create_data = contract.bytecode;

const TOO_MANY_TXS = 100;
const NB_LOOP = 100;
const FUNDS_PER_ACCOUNT = 10000000000000;

// create call data with different values
let call_data_prefix = "0x0b7d796e00000000000000000000000000000000"
let call_data = function (n) {
    return call_data_prefix + utils.encode_number(n)
}

let txs = [];

let transfer_txs = [];

let players = []

// global variable that will decrease overtime
let gasPrice = transfer_prototype_json.gasPrice + (TOO_MANY_TXS * 2 + 2);

// one player per transaction
// this make the scenario work with the proxy, which now (temporarily) forces
// 1 tx max per account
for (let i = 0; i < TOO_MANY_TXS; i++) {
    players.push(addr.create_player())
}

// Generate all transfers for the scenario
// let total_funds = generate_transfers(players, 0, transfer_txs)

let total_funds = TOO_MANY_TXS * FUNDS_PER_ACCOUNT

transfer_txs.push(
    utils.transfer(faucet, player1, total_funds + FUNDS_PER_ACCOUNT, { gasPrice })
);
transfer_txs.push(
    utils.transfer(player1, players[0], total_funds, { gasPrice: --gasPrice })
);

for (let i = 0; i < TOO_MANY_TXS - 1; i++) {
    transfer_txs.push(
        utils.transfer(
            players[i],
            players[i + 1],
            (TOO_MANY_TXS - i - 1) * FUNDS_PER_ACCOUNT,
            { gasPrice: --gasPrice }
        )
    )
}

// initialisation
let create = utils.create(player1, 0, create_data, { gasPrice: --gasPrice })
txs.push(create.tx)

for (let i = 0; i < TOO_MANY_TXS; i++) {
    txs.push(
        utils.send(players[i], create.addr, 0, call_data(NB_LOOP), undefined, { gasPrice: --gasPrice })
    )
}


if (args.length == 1 && args[0] == "raw") {
    utils.print_raw_txs(transfer_txs)
    utils.print_raw_txs(txs)
} else if (args.length == 1 && args[0] == "for-test") {
    console.log("transfers")
    utils.print_raw_txs(transfer_txs)
    console.log("calls")
    utils.print_raw_txs(txs)
} else {
    utils.print_bench([transfer_txs.concat(txs)])
}

// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// usage: node benchmarks/bench_transfers_3.js

const addr = require('../lib/address');
const utils = require('./utils');
let faucet = require('./players/faucet.json');

let players = []
// there will be nb_players² transactions
let nb_players = 7
for (let i = 0; i < nb_players; i++) {
    players.push(addr.create_player())
}

let txs_1 = [];
// every player get the same amount
for (player of players) {
    txs_1.push(utils.transfer(faucet, player, nb_players))
}

let txs_2 = [];
// every player send 1 to every other player
for (player1 of players) {
    for (player2 of players) {
        if (player1 != player2) {
            txs_2.push(utils.transfer(player1, player2, 1))
        }
    }
}

utils.print_bench([txs_1, txs_2])

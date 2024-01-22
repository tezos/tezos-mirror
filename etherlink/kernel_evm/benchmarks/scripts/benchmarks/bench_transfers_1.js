// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// usage: node benchmarks/bench_transfers_1.js

const utils = require('./utils');
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');
let player2 = require('./players/player2.json');

let txs = [];
// player1 get 10_000 from faucet
txs.push(utils.transfer(faucet, player1, 220_000))
// 10 transfers of 1_000 from player1 to player2
for (let i = 0; i < 10; i++) {
    txs.push(utils.transfer(player1, player2, 1000))
}

utils.print_bench([txs])

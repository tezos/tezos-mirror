// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// usage: node benchmarks/bench_transfers_2.js

const utils = require('./utils');
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');
let player2 = require('./players/player2.json');
let players = [player1, player2]
let txs = [];
// player1 get 10_000 from faucet
txs.push(utils.transfer(faucet, player1, 10000))
// even player1 -> player2
// odd player2 <- player1
for (let i = 0; i < 10; i++) {
    txs.push(utils.transfer(players[i % 2], players[(i + 1) % 2], 1000))
}

utils.print_bench([txs])

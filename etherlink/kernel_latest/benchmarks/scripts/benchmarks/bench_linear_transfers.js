// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// usage: node benchmarks/bench_transfers.js <n>
// if no argument <n> is given, count as 0 transfers, so only:
// - provision of player1 address from faucet


const utils = require('./utils');
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');
let player2 = require('./players/player2.json');

let mode = utils.bench_args(process.argv, [{flag: '--count <n>', desc: 'Number of transfers', default: 0}]);

let txs = [];
// player1 get 10_000 from faucet
txs.push(utils.transfer(faucet, player1, 10000000))
// 10 transfers of 1_000 from player1 to player2
for (let i = 0; i < mode.extra.count; i++) {
    txs.push(utils.transfer(player1, player2, 1000))
}

utils.print_bench([txs], mode)

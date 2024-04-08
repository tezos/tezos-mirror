// SPDX-License-Identifier: MIT

const utils = require('../../utils');
let faucet = require('../../players/faucet.json');
let player1 = require('../../players/player1.json');
let playerZero = require('../../players/player_zero.json');

let txs = [];
let mode = utils.bench_args(process.argv, [{flag: '--count <n>', desc: 'Number of runs', default: 0}]);

// playerZero get 10_000_000 from faucet
txs.push(utils.transfer(faucet, playerZero, 10_000_000))
// X transfers of 0 from playerZero to player2
for (let i = 0; i < mode.extra.count; i++) {
    txs.push(utils.transfer(playerZero, "0x0", 0))
}

utils.print_bench([txs], mode)

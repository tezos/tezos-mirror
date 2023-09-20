// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// usage: node benchmarks/bench_rec_call.js

// create contract and call it
// "rec_call.sol" example
// infinite recursive call, to reach max stack size

const utils = require('./utils');
const addr = require('../lib/address');
let faucet = require('./players/faucet.json');

let create_data = "0x608060405234801561001057600080fd5b5060d18061001f6000396000f3fe6080604052348015600f57600080fd5b506004361060285760003560e01c806328b5e32b14602d575b600080fd5b60336035565b005b60003090508073ffffffffffffffffffffffffffffffffffffffff166328b5e32b6040518163ffffffff1660e01b8152600401600060405180830381600087803b158015608157600080fd5b505af11580156094573d6000803e3d6000fd5b505050505056fea2646970667358221220015c6d8adac4157ee7d1d64cca81b25b08378be6b510781f321e4dea29f8892964736f6c63430008120033"
let call_data = "0x28b5e32b"

let player1 = require('./players/player1.json');

let txs = [];

// initialisation
txs.push(utils.transfer(faucet, player1, 100000000))
let create = utils.create(player1, 0, create_data)
txs.push(create.tx)

txs.push(utils.send(player1, create.addr, 0, call_data))

utils.print_bench([txs])
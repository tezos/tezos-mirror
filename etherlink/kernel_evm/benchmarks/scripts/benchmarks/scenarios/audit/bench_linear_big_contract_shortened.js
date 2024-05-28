// SPDX-License-Identifier: MIT

const utils = require('../../utils');
const { contracts_directory, compile_contract_file } = require("../../../lib/contract");
let faucet = require('../../players/faucet.json');
let player1 = require('../../players/player1.json');
let player2 = require('../../players/player2.json');

let txs = [];
let contracts = compile_contract_file(contracts_directory, "audit/GovernorOLAS-flatten_shortened.sol");

let create_data = contracts[10].bytecode;

let mode = utils.bench_args(process.argv, [{flag: '--count <n>', desc: 'Number of transfers', default: 0}]);
txs.push(utils.transfer(faucet, player1, 10000000))
for (let i = 0; i < mode.extra.count; i++) {
    let create = utils.create(player1, 0, create_data);
    txs.push(create.tx);
}
utils.print_bench([txs], mode)

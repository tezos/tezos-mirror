// SPDX-License-Identifier: MIT

const utils = require('../../utils');
const { contracts_directory, compile_contract_file } = require("../../../lib/contract");
let faucet = require('../../players/faucet.json');
let player1 = require('../../players/player1.json');
let player2 = require('../../players/player2.json');

let txs = [];
txs.push(utils.transfer(faucet, player1, 10000000))

let contract = compile_contract_file(contracts_directory, "audit/blockhash_255.sol")[0];
let create_data = contract.bytecode;
let create = utils.create(player1, 0, create_data);
txs.push(create.tx);

const run = function (runs) {
    return contract.interface.encodeFunctionData("run", [runs])
}

let mode = utils.bench_args(process.argv, [{flag: '--count <n>', desc: 'Number of runs', default: 0}]);
txs.push(utils.send(player1, create.addr, 0, run(mode.extra.count)))

utils.print_bench([txs], mode)

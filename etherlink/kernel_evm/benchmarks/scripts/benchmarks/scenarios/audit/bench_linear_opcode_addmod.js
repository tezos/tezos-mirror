// SPDX-License-Identifier: MIT

const utils = require('../../utils');
const { contracts_directory, compile_contract_file } = require("../../../lib/contract");
let faucet = require('../../players/faucet.json');
let player1 = require('../../players/player1.json');
let player2 = require('../../players/player2.json');

let txs = [];
let contract = compile_contract_file(contracts_directory, "audit/opcode_addmod.sol")[0];
let create_data = contract.bytecode;


const run = function (runs, valueA, valueB, denominator) {
    return contract.interface.encodeFunctionData("run", [runs, valueA, valueB, denominator])
}

let mode = utils.bench_args(process.argv, [{flag: '--count <n>', desc: 'Number of runs', default: 0}]);
// player1 get 10_000 from faucet
txs.push(utils.transfer(faucet, player1, 10000000))
let create = utils.create(player1, 0, create_data);
txs.push(create.tx);
txs.push(utils.send(player1, create.addr, 0, run(mode.extra.count, 1_000_000_000, 2_111_111_111, 999)))

utils.print_bench([txs], mode)

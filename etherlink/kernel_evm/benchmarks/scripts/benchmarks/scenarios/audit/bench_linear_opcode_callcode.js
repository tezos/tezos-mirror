// SPDX-License-Identifier: MIT

const utils = require('../../utils');
const { contracts_directory, compile_contract_file } = require("../../../lib/contract");
let faucet = require('../../players/faucet.json');
let player1 = require('../../players/player1.json');
let player2 = require('../../players/player2.json');

let txs = [];
let contracts = compile_contract_file(contracts_directory, "audit/opcode_callcode.sol");

let create_data_delegate = contracts[0].bytecode;
let create_data_delegator = contracts[1].bytecode;

const caller = function (runs, delegateAddr) {
    return contracts[1].interface.encodeFunctionData("caller", [runs, delegateAddr])
}

// player1 get 10_000 from faucet
txs.push(utils.transfer(faucet, player1, 10000000))
let create_delegate = utils.create(player1, 0, create_data_delegate);
txs.push(create_delegate.tx);

let create_delegator = utils.create(player1, 0, create_data_delegator);
txs.push(create_delegator.tx);


let mode = utils.bench_args(process.argv, [{flag: '--count <n>', desc: 'Number of runs', default: 0}]);
txs.push(utils.send(player1, create_delegator.addr, 0, caller(mode.extra.count, create_delegate.addr)))
utils.print_bench([txs], mode)

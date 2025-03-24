// SPDX-License-Identifier: MIT

const utils = require('../../utils');
const { contracts_directory, compile_contract_file } = require("../../../lib/contract");
let faucet = require('../../players/faucet.json');
let player1 = require('../../players/player1.json');
let player2 = require('../../players/player2.json');
const { wrap } = require('pdfkit');

let txs = [];
txs.push(utils.transfer(faucet, player1, 10000000))

let contract_cre = compile_contract_file(contracts_directory, "audit/create_ext.sol")[0];
let create_data_cre = contract_cre.bytecode;
let create_cre = utils.create(player1, 0, create_data_cre);
txs.push(create_cre.tx);

let contracts_large = compile_contract_file(contracts_directory, "audit/GovernorOLAS-flatten.sol");
let create_data_large = contracts_large[10].bytecode;
let create_large = utils.create(player1, 0, create_data_large);
txs.push(create_large.tx);

const run = function(contractCode) {
    return contract_cre.interface.encodeFunctionData("run", [contractCode])
}

txs.push(utils.send(player1, create_cre.addr, 0, run(create_data_large)))

let mode = utils.bench_args(process.argv);

utils.print_bench([txs], mode)

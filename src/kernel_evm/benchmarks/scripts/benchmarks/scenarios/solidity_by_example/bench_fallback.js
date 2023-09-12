const utils = require('../../utils');
const { contracts_directory, compile_contract_file } = require("../../../lib/contract");
let faucet = require('../../players/faucet.json');

const path = require('path');
const scenario_type = "solidity_by_example"

let contract1 = compile_contract_file(path.join(contracts_directory, scenario_type), "fallback.sol")[0];
let create_data1 = contract1.bytecode;
let contract2 = compile_contract_file(path.join(contracts_directory, scenario_type), "send_to_fallback.sol")[0];
let create_data2 = contract2.bytecode;
let player1 = require('../../players/player1.json');

let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000))
let create1 = utils.create(player1, 0, create_data1)
let create2 = utils.create(player1, 0, create_data2)
txs.push(create1.tx)
txs.push(create2.tx)

let transferToFallback = contract2.interface.encodeFunctionData("transferToFallback", [create1.addr]);
let callFallback = contract2.interface.encodeFunctionData("callFallback", [create1.addr]);

txs.push(utils.send(player1, create2.addr, 1000, transferToFallback))
txs.push(utils.send(player1, create2.addr, 1000, callFallback))

utils.print_bench([txs])

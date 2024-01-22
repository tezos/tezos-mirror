const utils = require('../../utils');
const { contracts_directory, compile_contract_file } = require("../../../lib/contract");
let faucet = require('../../players/faucet.json');
let player1 = require('../../players/player1.json');

const path = require('path');
const scenario_type = "solidity_by_example"

let contract = compile_contract_file(path.join(contracts_directory, scenario_type), "create_contract.sol")[1];
let create_data = contract.bytecode;
let create_1 = contract.interface.encodeFunctionData("create", [player1.addr, "model1"]);

let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000))
let create = utils.create(player1, 0, create_data)
txs.push(create.tx)
txs.push(utils.send(player1, create.addr, 0, create_1))

utils.print_bench([txs])

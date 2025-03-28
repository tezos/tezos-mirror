const utils = require('../../utils');
const { contracts_directory, compile_contract_file } = require("../../../lib/contract");
let faucet = require('../../players/faucet.json');

const path = require('path');
const scenario_type = "solidity_by_example"

let contract = compile_contract_file(path.join(contracts_directory, scenario_type), "struct.sol")[0];
let create_data = contract.bytecode;
let player1 = require('../../players/player1.json');

let create_call = contract.interface.encodeFunctionData("create", ["string1"]);
let get = contract.interface.encodeFunctionData("get", [0]);
let updateText = contract.interface.encodeFunctionData("updateText", [0, "string2"]);
let toggleCompleted = contract.interface.encodeFunctionData("toggleCompleted", [0]);

let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000))
let create = utils.create(player1, 0, create_data)
txs.push(create.tx)

txs.push(utils.send(player1, create.addr, 0, create_call))
txs.push(utils.send(player1, create.addr, 0, get))
txs.push(utils.send(player1, create.addr, 0, updateText))
txs.push(utils.send(player1, create.addr, 0, toggleCompleted))

let mode = utils.bench_args(process.argv);

utils.print_bench([txs], mode)

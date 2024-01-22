const utils = require('../../utils');
const { contracts_directory, compile_contract_file } = require("../../../lib/contract");
let faucet = require('../../players/faucet.json');

const path = require('path');
const scenario_type = "solidity_by_example"

let contract = compile_contract_file(path.join(contracts_directory, scenario_type), "array.sol")[0];
let create_data = contract.bytecode;
let player1 = require('../../players/player1.json');

let get = contract.interface.encodeFunctionData("get", [0]);
let getArr = contract.interface.encodeFunctionData("getArr", []);
let push = contract.interface.encodeFunctionData("push", [1]);
let pop = contract.interface.encodeFunctionData("pop", []);
let getLength = contract.interface.encodeFunctionData("getLength", []);
let remove = contract.interface.encodeFunctionData("remove", [0]);
let examples = contract.interface.encodeFunctionData("examples", []);

let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000))
let create = utils.create(player1, 0, create_data)
txs.push(create.tx)

txs.push(utils.send(player1, create.addr, 0, push))
txs.push(utils.send(player1, create.addr, 0, push))
txs.push(utils.send(player1, create.addr, 0, push))
txs.push(utils.send(player1, create.addr, 0, get))
txs.push(utils.send(player1, create.addr, 0, getArr))
txs.push(utils.send(player1, create.addr, 0, getLength))
txs.push(utils.send(player1, create.addr, 0, pop))
txs.push(utils.send(player1, create.addr, 0, remove))
txs.push(utils.send(player1, create.addr, 0, examples))

utils.print_bench([txs])

const utils = require('../../utils');
const { contracts_directory, compile_contract_file } = require("../../../lib/contract");
let faucet = require('../../players/faucet.json');

const path = require('path');
const scenario_type = "solidity_by_example"

let contract = compile_contract_file(path.join(contracts_directory, scenario_type), "mapping.sol")[0];
let create_data = contract.bytecode;
let player1 = require('../../players/player1.json');
let addr = player1.addr;

let set = contract.interface.encodeFunctionData("set", [addr, 0, false]);
let get = contract.interface.encodeFunctionData("get", [addr, 0]);
let remove = contract.interface.encodeFunctionData("remove", [addr, 0]);

let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000))
let create = utils.create(player1, 0, create_data)
txs.push(create.tx)

txs.push(utils.send(player1, create.addr, 0, set))
txs.push(utils.send(player1, create.addr, 0, get))
txs.push(utils.send(player1, create.addr, 0, remove))

let mode = utils.bench_args(process.argv);

utils.print_bench([txs], mode)

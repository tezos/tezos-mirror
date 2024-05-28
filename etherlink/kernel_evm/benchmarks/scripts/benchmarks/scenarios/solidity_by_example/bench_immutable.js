const utils = require('../../utils');
const { contracts_directory, compile_contract_file } = require("../../../lib/contract");
let faucet = require('../../players/faucet.json');

const path = require('path');
const scenario_type = "solidity_by_example"

let contract = compile_contract_file(path.join(contracts_directory, scenario_type), "immutable.sol")[0];
let create_data = contract.bytecode;
let player1 = require('../../players/player1.json');

const encodedParameters = contract.interface.encodeDeploy([0]).slice(2);

let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000))
let create = utils.create(player1, 0, create_data + encodedParameters)
txs.push(create.tx)

let mode = utils.bench_args(process.argv);

utils.print_bench([txs], mode)

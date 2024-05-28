// The scenario contains transactions that fail due to issue: https://gitlab.com/tezos/tezos/-/issues/6430
const utils = require('../../utils');
const { contracts_directory, compile_contract_file } = require("../../../lib/contract");
let faucet = require('../../players/faucet.json');

const path = require('path');
const scenario_type = "solidity_by_example"

let contract1 = compile_contract_file(path.join(contracts_directory, scenario_type), "delegatecall.sol")[0];
let create_data1 = contract1.bytecode;
let contract2 = compile_contract_file(path.join(contracts_directory, scenario_type), "delegatecall.sol")[1];
let create_data2 = contract2.bytecode;
let player1 = require('../../players/player1.json');

let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000))
let create1 = utils.create(player1, 0, create_data1)
let create2 = utils.create(player1, 0, create_data2)
txs.push(create1.tx)
txs.push(create2.tx)

let setVars = contract2.interface.encodeFunctionData("setVars", [create1.addr, 100]);

txs.push(utils.send(player1, create2.addr, 0, setVars))

let mode = utils.bench_args(process.argv);

utils.print_bench([txs], mode)

// The scenario contains transactions that fail due to issue: https://gitlab.com/tezos/tezos/-/issues/6430
const utils = require('../../utils');
const { contracts_directory, compile_contract_file } = require("../../../lib/contract");

let faucet = require('../../players/faucet.json');

const path = require('path');
const scenario_type = "solidity_by_example"

let contract = compile_contract_file(path.join(contracts_directory, scenario_type), "abi_decode.sol")[0];
let create_data = contract.bytecode;
let player1 = require('../../players/player1.json');

let test = contract.interface.encodeFunctionData("test", []);


let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000000))
let create = utils.create(player1, 0, create_data, {gasLimit: 1000000000})
txs.push(create.tx)

txs.push(utils.send(player1, create.addr, 0, test, {gasLimit: 1000000000}))

let mode = utils.bench_args(process.argv);

utils.print_bench([txs], mode)

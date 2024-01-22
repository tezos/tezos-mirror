// The scenario contains transactions that fail due to issue: https://gitlab.com/tezos/tezos/-/issues/6430
const utils = require('../../utils');
const { contracts_directory, compile_contract_file } = require("../../../lib/contract");
let faucet = require('../../players/faucet.json');

const path = require('path');
const scenario_type = "solidity_by_example"

let proxy = compile_contract_file(path.join(contracts_directory, scenario_type), "minimal_proxy.sol")[0];
let counter = compile_contract_file(path.join(contracts_directory, scenario_type), "counter.sol")[0];
let create_data_proxy = proxy.bytecode;
let create_data_counter = counter.bytecode;
let player1 = require('../../players/player1.json');

let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000))
let create_counter = utils.create(player1, 0, create_data_counter)
txs.push(create_counter.tx)

let create_proxy = utils.create(player1, 0, create_data_proxy)
txs.push(create_proxy.tx)

let clone = proxy.interface.encodeFunctionData("clone", [create_counter.addr]);
let inc = counter.interface.encodeFunctionData("inc", []);
let dec = counter.interface.encodeFunctionData("dec", []);
let get = counter.interface.encodeFunctionData("get", []);

txs.push(utils.send(player1, create_proxy.addr, 0, clone))

txs.push(utils.send(player1, create_proxy.addr, 0, inc))
txs.push(utils.send(player1, create_proxy.addr, 0, dec))
txs.push(utils.send(player1, create_proxy.addr, 0, get))

utils.print_bench([txs])

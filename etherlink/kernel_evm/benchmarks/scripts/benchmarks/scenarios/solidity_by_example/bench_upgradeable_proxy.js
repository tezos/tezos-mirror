// The scenario contains transactions that fail due to issue: https://gitlab.com/tezos/tezos/-/issues/6430
const utils = require('../../utils');
const { contracts_directory, compile_contract_file } = require("../../../lib/contract");
let faucet = require('../../players/faucet.json');

const path = require('path');
const scenario_type = "solidity_by_example"

let counter_v1 = compile_contract_file(path.join(contracts_directory, scenario_type), "upgradeable_proxy.sol")[1];
let counter_v2 = compile_contract_file(path.join(contracts_directory, scenario_type), "upgradeable_proxy.sol")[2];
let proxy = compile_contract_file(path.join(contracts_directory, scenario_type), "upgradeable_proxy.sol")[4];
let proxy_admin = compile_contract_file(path.join(contracts_directory, scenario_type), "upgradeable_proxy.sol")[5];
let player1 = require('../../players/player1.json');
let player2 = require('../../players/player2.json');

let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000))
txs.push(utils.transfer(faucet, player2, 100000000))
let create_counter_v1 = utils.create(player1, 0, counter_v1.bytecode)
txs.push(create_counter_v1.tx)

let create_counter_v2 = utils.create(player1, 0, counter_v2.bytecode)
txs.push(create_counter_v2.tx)

let create_proxy = utils.create(player1, 0, proxy.bytecode)
txs.push(create_proxy.tx)

let create_proxy_admin = utils.create(player1, 0, proxy_admin.bytecode)
txs.push(create_proxy_admin.tx)

let changeAdmin = proxy.interface.encodeFunctionData("changeAdmin", [create_proxy_admin.addr]);
let upgrade_1 = proxy_admin.interface.encodeFunctionData("upgrade", [create_proxy.addr, create_counter_v1.addr]);
let inc = counter_v1.interface.encodeFunctionData("inc", []);
let dec = counter_v2.interface.encodeFunctionData("dec", []);
let upgrade_2 = proxy_admin.interface.encodeFunctionData("upgrade", [create_proxy.addr, create_counter_v2.addr]);

txs.push(utils.send(player1, create_proxy.addr, 0, changeAdmin))
txs.push(utils.send(player1, create_proxy_admin.addr, 0, upgrade_1))
txs.push(utils.send(player2, create_proxy.addr, 0, inc))
txs.push(utils.send(player2, create_proxy.addr, 0, dec)) // expected to fail because upgrade 1 does not implement dec() 
txs.push(utils.send(player1, create_proxy_admin.addr, 0, upgrade_2))
txs.push(utils.send(player2, create_proxy.addr, 0, inc))
txs.push(utils.send(player2, create_proxy.addr, 0, dec))

let mode = utils.bench_args(process.argv);

utils.print_bench([txs], mode)

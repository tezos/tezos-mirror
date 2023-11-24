// The scenario contains transactions that fail due to issue: https://gitlab.com/tezos/tezos/-/issues/6430
const utils = require('../../utils');
const { contracts_directory, compile_contract_file } = require("../../../lib/contract");
let faucet = require('../../players/faucet.json');

const path = require('path');
const scenario_type = "solidity_by_example"

let contract = compile_contract_file(path.join(contracts_directory, scenario_type), "send_ether.sol")[0];
let create_data = contract.bytecode;
let player1 = require('../../players/player1.json');
let player2 = require('../../players/player2.json');

let sendViaTransfer = contract.interface.encodeFunctionData("sendViaTransfer", [player2.addr]);
let sendViaSend = contract.interface.encodeFunctionData("sendViaSend", [player2.addr]);
let sendViaCall = contract.interface.encodeFunctionData("sendViaCall", [player2.addr]);

let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000))
let create = utils.create(player1, 0, create_data)
txs.push(create.tx)

txs.push(utils.send(player1, create.addr, 1000, sendViaTransfer))
txs.push(utils.send(player1, create.addr, 1000, sendViaSend))
txs.push(utils.send(player1, create.addr, 1000, sendViaCall))

utils.print_bench([txs])

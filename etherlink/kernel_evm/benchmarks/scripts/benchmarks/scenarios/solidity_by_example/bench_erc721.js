// The scenario contains transactions that fail due to issue: https://gitlab.com/tezos/tezos/-/issues/6430
const utils = require('../../utils');
const { contracts_directory, compile_contract_file } = require("../../../lib/contract");
let faucet = require('../../players/faucet.json');

const path = require('path');
const scenario_type = "solidity_by_example"

let contract = compile_contract_file(path.join(contracts_directory, scenario_type), "erc721.sol")[4];
let create_data = contract.bytecode;
let player1 = require('../../players/player1.json');
let player2 = require('../../players/player2.json');

let mint = contract.interface.encodeFunctionData("mint", [player1.addr, 10]);
let approve = contract.interface.encodeFunctionData("approve", [player2.addr ,10]);
let transfer = contract.interface.encodeFunctionData("transferFrom", [player1.addr, player2.addr ,10]);
let burn = contract.interface.encodeFunctionData("burn", [10]);


let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000))
txs.push(utils.transfer(faucet, player2, 100000000))
let create = utils.create(player1, 0, create_data, {gasLimit: 10000000})
txs.push(create.tx)

txs.push(utils.send(player1, create.addr, 0, mint))
txs.push(utils.send(player1, create.addr, 0, approve))
txs.push(utils.send(player2, create.addr, 0, transfer))
txs.push(utils.send(player2, create.addr, 0, burn))


utils.print_bench([txs])

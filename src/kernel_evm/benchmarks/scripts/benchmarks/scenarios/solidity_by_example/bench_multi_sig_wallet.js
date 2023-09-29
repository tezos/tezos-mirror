// The scenario contains transactions that fail due to issue: https://gitlab.com/tezos/tezos/-/issues/6430
const utils = require('../../utils');
const { contracts_directory, compile_contract_file } = require("../../../lib/contract");

let faucet = require('../../players/faucet.json');

const path = require('path');
const scenario_type = "solidity_by_example"

let contract = compile_contract_file(path.join(contracts_directory, scenario_type), "multi_sig_wallet.sol")[0];
let player1 = require('../../players/player1.json');
let player2 = require('../../players/player2.json');

const encodedParameters = contract.interface.encodeDeploy(
  [[player1.addr, player2.addr], 2]
).slice(2);

let submitTransaction = contract.interface.encodeFunctionData("submitTransaction", [player2.addr, 1000, "0x"]);
let confirmTransaction = contract.interface.encodeFunctionData("confirmTransaction", [0]);
let executeTransaction = contract.interface.encodeFunctionData("executeTransaction", [0]);

let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000))
txs.push(utils.transfer(faucet, player2, 100000000))
let create = utils.create(player1, 0, contract.bytecode + encodedParameters)
txs.push(create.tx)

txs.push(utils.transfer(player1, create, 10000))
txs.push(utils.send(player1, create.addr, 0, submitTransaction))
txs.push(utils.send(player1, create.addr, 0, confirmTransaction))
txs.push(utils.send(player2, create.addr, 0, confirmTransaction))
txs.push(utils.send(player2, create.addr, 0, executeTransaction))

utils.print_bench([txs])

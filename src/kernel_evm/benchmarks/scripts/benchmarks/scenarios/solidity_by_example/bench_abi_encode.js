const utils = require('../../utils');
const { contracts_directory, compile_contract_file } = require("../../../lib/contract");
let faucet = require('../../players/faucet.json');
let player1 = require('../../players/player1.json');
let player2 = require('../../players/player2.json');

const path = require('path');
const scenario_type = "solidity_by_example"

let token = compile_contract_file(path.join(contracts_directory, scenario_type), "abi_encode.sol")[1];
let abi_encode = compile_contract_file(path.join(contracts_directory, scenario_type), "abi_encode.sol")[0];
let create_data_token = token.bytecode;
let create_data_encode = abi_encode.bytecode;



let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000))
let create = utils.create(player1, 0, create_data_token)
txs.push(create.tx)

let create_encode = utils.create(player1, 0, create_data_encode)
txs.push(create_encode.tx)

let test = abi_encode.interface.encodeFunctionData("test", [create.addr, player2.addr, 10000]);

txs.push(utils.send(player1, create.addr, 0, test))

utils.print_bench([txs])

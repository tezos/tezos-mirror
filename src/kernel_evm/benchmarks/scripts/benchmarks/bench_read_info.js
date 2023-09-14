const utils = require('./utils');
const { contracts_directory, compile_contract_file } = require("../lib/contract");
let faucet = require('./players/faucet.json');

let contract = compile_contract_file(contracts_directory, "read_info.sol")[0];
let create_data = contract.bytecode;
let player1 = require('./players/player1.json');

let get_time_stamp = contract.interface.encodeFunctionData("timestamp", []);
let get_gas_price = contract.interface.encodeFunctionData("gas_price", []);
let get_origin = contract.interface.encodeFunctionData("origin", []);
let get_coinbase =contract.interface.encodeFunctionData("coin_base", []);

let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000))
let create = utils.create(player1, 0, create_data)
txs.push(create.tx)

txs.push(utils.send(player1, create.addr, 0, get_time_stamp))
txs.push(utils.send(player1, create.addr, 0, get_gas_price))
txs.push(utils.send(player1, create.addr, 0, get_origin))
txs.push(utils.send(player1, create.addr, 0, get_coinbase))

utils.print_bench([txs])

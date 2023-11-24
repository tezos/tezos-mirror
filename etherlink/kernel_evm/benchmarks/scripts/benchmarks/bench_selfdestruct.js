const utils = require('./utils');
const { contracts_directory, compile_contract_file } = require("../lib/contract");
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');

let contract = compile_contract_file(contracts_directory, "selfdestruct.sol")[0];
let create_data = contract.bytecode;

let close = contract.interface.encodeFunctionData("close", [])

let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000000));
let create = utils.create(player1, 0, create_data);
txs.push(create.tx);

txs.push(utils.transfer(player1, create.addr, 10000));
txs.push(utils.send(player1, create.addr, 0, close));

utils.print_bench([txs])

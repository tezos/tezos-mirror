const utils = require('../../utils');
const { contracts_directory, compile_contract_file } = require("../../../lib/contract");
let faucet = require('../../players/faucet.json');

const path = require('path');
const scenario_type = "solidity_by_example"

let contract = compile_contract_file(path.join(contracts_directory, scenario_type), "bitwise_op.sol")[0];
let create_data = contract.bytecode;
let player1 = require('../../players/player1.json');

let and = contract.interface.encodeFunctionData("and", [100,200]);
let or = contract.interface.encodeFunctionData("or", [100,200]);
let xor = contract.interface.encodeFunctionData("xor", [100,200]);
let not = contract.interface.encodeFunctionData("not", [100]);
let shiftLeft = contract.interface.encodeFunctionData("shiftLeft", [100,3]);
let shiftRight = contract.interface.encodeFunctionData("shiftRight", [100,3]);
let getLastNBits = contract.interface.encodeFunctionData("getLastNBits", [100,3]);
let getLastNBitsUsingMod = contract.interface.encodeFunctionData("getLastNBitsUsingMod", [100,3]);
let mostSignificantBit = contract.interface.encodeFunctionData("mostSignificantBit", [100]);
let getFirstNBits = contract.interface.encodeFunctionData("getFirstNBits", [100,3, 10]);


let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000))
let create = utils.create(player1, 0, create_data)
txs.push(create.tx)

txs.push(utils.send(player1, create.addr, 0, and))
txs.push(utils.send(player1, create.addr, 0, or))
txs.push(utils.send(player1, create.addr, 0, xor))
txs.push(utils.send(player1, create.addr, 0, not))
txs.push(utils.send(player1, create.addr, 0, shiftLeft))
txs.push(utils.send(player1, create.addr, 0, shiftRight))
txs.push(utils.send(player1, create.addr, 0, getLastNBits))
txs.push(utils.send(player1, create.addr, 0, getLastNBitsUsingMod))
txs.push(utils.send(player1, create.addr, 0, mostSignificantBit))
txs.push(utils.send(player1, create.addr, 0, getFirstNBits))

let mode = utils.bench_args(process.argv);

utils.print_bench([txs], mode)

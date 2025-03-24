// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// create contract and call it
// keccak.sol

const utils = require('./utils');
const { contracts_directory, compile_contract_file } = require("../lib/contract");
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');

let contract = compile_contract_file(contracts_directory, "keccak.sol")[0];
let create_data = contract.bytecode;
let call_answer = "0x85bb7d69"
let call_guess_soli = contract.interface.encodeFunctionData("guess", ["Solidity"])
let call_guess_test = contract.interface.encodeFunctionData("guess", ["test"])
let call_guess_long = contract.interface.encodeFunctionData("guess", ["Ceci est un text un peu long, sisi, un peu long vous dis-je"])
let call_lorem_ipsum =  contract.interface.encodeFunctionData("guess", ["Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."])
let txs = [];
// initialisation
// player 1 gets funds, then originate the contract, then stores 36
txs.push(utils.transfer(faucet, player1, 100000000))
let create = utils.create(player1, 0, create_data)
txs.push(create.tx)
txs.push(utils.send(player1, create.addr, 0, call_answer))
txs.push(utils.send(player1, create.addr, 0, call_guess_soli))
txs.push(utils.send(player1, create.addr, 0, call_guess_test))
txs.push(utils.send(player1, create.addr, 0, call_guess_long))
txs.push(utils.send(player1, create.addr, 0, call_lorem_ipsum))


let mode = utils.bench_args(process.argv);

utils.print_bench([txs], mode)

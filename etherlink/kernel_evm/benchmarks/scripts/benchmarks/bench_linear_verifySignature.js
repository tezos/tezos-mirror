// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// create contract and call it
// verifySignature.sol

const utils = require('./utils');
const { contracts_directory, compile_contract_file } = require("../lib/contract");
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');

let nb_iter = process.argv.length > 2 ? process.argv[2] : 1;
if (nb_iter < 0) nb_iter = 0;

let contract = compile_contract_file(contracts_directory, "verifySignature.sol")[0];
let create_data = contract.bytecode;

let call_getMessageHash =
    contract.interface.encodeFunctionData(
        "getMessageHash", [
        '0x14723A09ACff6D2A60DcdF7aA4AFf308FDDC160C',
        123n,
        'coffee and donuts',
        1n
    ]);

let txs = [];
// initialisation
txs.push(utils.transfer(faucet, player1, 100000000))
let create = utils.create(player1, 0, create_data)
txs.push(create.tx)
for (let i = 0; i < nb_iter; i++) {
    txs.push(utils.send(player1, create.addr, 0, call_getMessageHash))
}
utils.print_bench([txs])

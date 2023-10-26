// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// create contract and call it
// verifySignature.sol

const utils = require('./utils');
const { contracts_directory, compile_contract_file } = require("../lib/contract");
let faucet = require('./players/faucet.json');
let player1 = require('./players/player1.json');


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
let call_getEthSignedMessageHash =
    contract.interface.encodeFunctionData(
        "getEthSignedMessageHash",
        ["0xcf36ac4f97dc10d91fc2cbb20d718e94a8cbfe0f82eaedc6a4aa38946fb797cd"]
    );
let call_verify =
    contract.interface.encodeFunctionData(
        "verify",
        [
            '0xB273216C05A8c0D4F0a4Dd0d7Bae1D2EfFE636dd',
            '0x14723A09ACff6D2A60DcdF7aA4AFf308FDDC160C',
            123n,
            'coffee and donuts',
            1n,
            '0x993dab3dd91f5c6dc28e17439be475478f5635c92a56e17e82349d3fb2f166196f466c0b4e0c146f285204f0dcb13e5ae67bc33f4b888ec32dfe0a063e8f3f781b'
        ])

let txs = [];
// initialisation
// player 1 gets funds, then originate the contract, then stores 36
txs.push(utils.transfer(faucet, player1, 100000000))
let create = utils.create(player1, 0, create_data)
txs.push(create.tx)
txs.push(utils.send(player1, create.addr, 0, call_getMessageHash))
txs.push(utils.send(player1, create.addr, 0, call_getEthSignedMessageHash))

// following tx needs more than 3999000 gas
// tick model can't accept more for now
txs.push(utils.send(player1, create.addr, 0, call_verify, { gasLimit: 3999000 }))

utils.print_bench([txs])

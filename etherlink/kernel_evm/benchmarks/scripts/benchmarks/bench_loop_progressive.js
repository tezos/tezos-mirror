// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// usage: node benchmarks/bench_loop.js

// create contract and call it
// "loop.sol" example

const utils = require('./utils');
const addr = require('../lib/address');

var faucet;

let mode = utils.bench_args(process.argv,
    [
        {flag: '--for-reboot-limits-test', desc: 'Generate the raw inputs for the reboot Tezt test', default: false},
        {flag: '--reboot-limits-debugger', desc: 'Generate the raw inputs for the reboot test for the debugger', default: false},
        {flag: '--for-reboot-discard-test', desc: 'Generate the raw inputs for the transaction discard Tezt test', default: false},
        {flag: '--reboot-discard-debugger', desc: 'Generate the raw inputs for the transaction discard test in the debugger', default: false},
    ]
);

if (mode.extra.forRebootLimitsTest || mode.extra.forRebootDiscardTest) {
    // for tests
    faucet = require('./players/tezt_bootstraped.json');
} else {
    // for benchmarking
    faucet = require('./players/faucet.json');
}

let player1 = require('./players/player1.json');
let player2 = require('./players/player2.json');
const { contracts_directory, compile_contract_file } = require("../lib/contract");

let contract = compile_contract_file(contracts_directory, 'loop.sol')[0];

let create_data = contract.bytecode;

// create call data with different values
let call_data = function (n) {
    return contract.interface.encodeFunctionData("loop", [n]);
}

let txs = [];

// Scenario for tests
// Due to the 1 operation per address per block in the node, the semantics for tezt are the following:
//
// **Block 1**:
// - Faucet -> player1
// - player1 -> player2
//
// **Block 2**:
// - player1: loop(1300) ~800k gas, 2G ticks
// - player2: loop(4800) ~2274k gas, 6.87G ticks
//    -> under the 4M gas limit but  above the 8.5G ticks, induces a reboot as it cannot fit in the same run as the first loop

// initialisation
function init(txs, amount) {
    txs.push(utils.transfer(faucet, player2, amount))
    txs.push(utils.transfer(player2, player1, (Math.floor (amount * 2 / 3))))
    let create = utils.create(player1, 0, create_data, { gasLimit: 160000 })
    txs.push(create.tx);
    return create.addr
}


function test_reboot_limits(player1, player2, txs, create_addr) {
    txs.push(utils.send(player1, create_addr, 0, call_data(1300), { gasLimit: 1_000_000 }))
    txs.push(utils.send(player2, create_addr, 0, call_data(4600), { gasLimit: 2_800_000 }))
}

function test_reboot_discard(player1, txs, create_addr) {
    txs.push(utils.send(player1, create_addr, 0, call_data(5800), { gasLimit: 4_000_000 }))
}

if (mode.extra.forRebootLimitsTest) {
    let addr = init(txs, 9000);
    test_reboot_limits(player1, player2, txs, addr);
    utils.print_raw_txs(txs)
} else if (mode.extra.rebootLimitsDebugger) {
    let addr = init(txs, 1000000);
    test_reboot_limits(player1, player2, txs, addr);
    utils.print_bench([txs], mode)
} else if (mode.extra.forRebootDiscardTest) {
    let addr = init(txs, 9000);
    test_reboot_discard(player1, txs, addr);
    utils.print_raw_txs(txs)
} else if (mode.extra.rebootDiscardDebugger) {
    let addr = init(txs, 1000000);
    test_reboot_discard(player1, txs, addr);
    utils.print_bench([txs], mode)
} else {
    txs.push(utils.transfer(faucet, player1, 1000000))
    let create = utils.create(player1, 0, create_data, { gasLimit: 160000 })
    txs.push(create.tx);
    let max = 100;
    // send calls
    for (let i = 0; i <= max; i++) {
        txs.push(utils.send(player1, create.addr, 0, call_data(i)))
    }

    utils.print_bench([txs], mode)
}

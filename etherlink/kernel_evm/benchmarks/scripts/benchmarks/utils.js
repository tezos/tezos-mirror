// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// Utilities module to create bench
// use transfer(A,B,N) to obtain a signed transaction where "A" sends "N" to "B"
//   - A and B are players (see ./players directory for examples)
//   - N is an amount
//   - "A" will see its nonce incremented
// use print_bench(inputs) to print a benchmark
//   - inputs is an array of array of transactions (as strings)
//   - see bench1.js

const { sign } = require('../lib/signature');
const { legacy_contract_address } = require('../lib/contract');
const { execSync } = require('child_process');
const external = require("../lib/external")
const commander = require('commander');

const transfer_prototype_json = require('./transfer_prototype.json');
const create_prototype_json = require('./create_prototype.json');
const CHUNKER = external.bin("./octez-evm-node")

// fee is 4 mutez per byte, but has to be converted in wei
// 1 tez = 1 eth = 10^18 wei
// 1 mutez = 10^-6 tez = 10^12 wei
const DA_FEE_PER_BYTE = 4 * Math.pow(10, 12);

const ASSUMED_TX_ENCODED_SIZE = 150;
const add_fee = function (tx) {
    let gas_price = tx.gasPrice;
    let size = ASSUMED_TX_ENCODED_SIZE;
    if (tx.data) {
        size += tx.data.length / 2;
    }
    let da_fee = size * DA_FEE_PER_BYTE;
    let fee_gas = Math.ceil(da_fee / gas_price);
    tx.gasLimit += fee_gas;
}
const print_full = function (rawTx) {

    console.log(`tx = 0x${rawTx.rawTx}`);
    console.log("msgHash = " + rawTx.msgHash);
    console.log("r = " + rawTx.signature.slice(0, 32).toString('hex'));
    console.log("s = " + rawTx.signature.slice(32, 64).toString('hex'));
}

exports.transfer = function (playera, playerb, amount, options = {}) {
    let tx = {
        gasLimit: 21000,
        ...transfer_prototype_json,
        ...options
    };
    tx.nonce = playera.nonce;
    playera.nonce += 1;
    tx.to = playerb.addr;
    // Enforce amount to be in Eth, not Wei
    tx.value = Math.round(amount * 1_000_000_000_000_000_000);
    add_fee(tx);
    let rawTx = sign(tx, playera.privateKey)
    return rawTx.rawTx;
}

exports.create = function (player, amount, data, options = {}) {
    let tx = {
        ...create_prototype_json,
        ...options,
    };
    tx.nonce = player.nonce;
    let address = legacy_contract_address(player.addr, player.nonce);
    player.nonce += 1;
    tx.value = Math.round(amount * 1_000_000_000_000_000_000);
    tx.data = data;
    add_fee(tx);
    let rawTx = sign(tx, player.privateKey)
    return {
        addr: address,
        tx: rawTx.rawTx
    };
}

exports.send = function (player, contract_addr, amount, data, options = {}) {
    let tx = {
        ...transfer_prototype_json,
        ...options
    };
    tx.nonce = player.nonce;
    player.nonce += 1;
    tx.to = contract_addr;
    tx.value = Math.round(amount * 1_000_000_000_000_000_000);
    tx.data = data;
    add_fee(tx);
    let rawTx = sign(tx, player.privateKey)
    return rawTx.rawTx;
}

const print_list = function (src, mode, blueprint_number) {
    const txs = src.slice();
    console.log("[")
    let data = txs.join(" ");
    let messages = mode.sequencer_key ?
        chunk_data_into_blueprint(data, blueprint_number, mode.sequencer_key) :
        chunk_data(data);
    for (var j = 0; j < messages.length; j++) {
        seperator = (j < messages.length - 1) ? "," : "";
        console.log(`{"external": "${messages[j]}"}${seperator}`);
    }
    console.log("]");
    return txs.length
}

const chunk_data = function (src) {
    run_chunker_command = `${CHUNKER} chunk data "${src}" --devmode`
    chunked_message = new Buffer.from(execSync(run_chunker_command)).toString();
    return chunked_message.split("\n").slice(1, -1);
}

const chunk_data_into_blueprint = function (src, number, sequencer_key) {
    run_chunker_command = `${CHUNKER} chunk data ${src} --devmode --as-blueprint --number ${number} --timestamp ${number} --sequencer-key text:${sequencer_key}`
    chunked_message = new Buffer.from(execSync(run_chunker_command)).toString();
    return chunked_message.split("\n").slice(1, -1);
}

exports.print_bench = function (src, mode = {}) {
    let number = 0;
    const inputs = src.slice();
    console.log("[")
    while (inputs.length > 1) {
        let txs_length = print_list(inputs.shift(), mode, number)
        console.log(",");
        number += txs_length;
    }
    print_list(inputs.shift(), mode, number)
    console.log("]")
}

exports.print_raw_txs = function (src) {
    const txs = src.slice();
    txs.forEach(element => {
        console.log(element)
    });

}
exports.encode_number = function (n) {

    let s = "00000000000000000000000000000000"
    var zeroFilled = (s + n.toString(16)).slice(-32)
    return zeroFilled
}

// Generate the commands for each benchmarks to use a sequencer or not.
exports.bench_args = function (argv, extra_options = []) {
    let commands =
        commander
            .usage("[OPTIONS]")
            .option('--sequencer <private key>',
                'Generates a chunked blueprint signed with <private key>');

    for (option of extra_options) {
        commands.option(option.flag, option.desc, option.default)
    }

    commands.parse(argv);

    return commander.opts().sequencer ?
        { sequencer_key: commander.opts().sequencer, extra : commands.opts() } :
        { extra: commands.opts() };

}

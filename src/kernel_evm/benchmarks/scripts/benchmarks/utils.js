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

const transfer_prototype_json = require('./transfer_prototype.json');
const create_prototype_json = require('./create_prototype.json');
const CHUNKER = external.bin("./octez-evm-node")

const print_full = function (rawTx) {

    console.log(`tx = 0x${rawTx.rawTx}`);
    console.log("msgHash = " + rawTx.msgHash);
    console.log("r = " + rawTx.signature.slice(0, 32).toString('hex'));
    console.log("s = " + rawTx.signature.slice(32, 64).toString('hex'));
}

exports.transfer = function (playera, playerb, amount, options = {}) {
    let tx = { 
        ...transfer_prototype_json,
        ...options 
    };
    tx.nonce = playera.nonce;
    playera.nonce += 1;
    tx.to = playerb.addr;
    // Enforce amount to be in Eth, not Wei
    tx.value = Math.round(amount * 1_000_000_000_000_000_000);
    tx.gasLimit = 21000;
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
    let rawTx = sign(tx, player.privateKey)
    return rawTx.rawTx;
}

const print_list = function (src) {
    const txs = src.slice();
    console.log("[")
    for (var i = 0; i < txs.length; i++) {
        transaction = txs[i];
        messages = chunk_data(transaction);
        for (var j = 0; j < messages.length; j++) {
            seperator = (i < txs.length - 1 || j < messages.length - 1) ? "," : "";
            console.log(`{"external": "${messages[j]}"}${seperator}`);
        }
    }
    console.log("]")
}

const chunk_data = function (src) {
    run_chunker_command = `${CHUNKER} chunk data "${src}" --devmode`
    chunked_message = new Buffer.from(execSync(run_chunker_command)).toString();
    return chunked_message.split("\n").slice(1, -1);
}

exports.print_bench = function (src) {
    const inputs = src.slice();
    console.log("[")
    while (inputs.length > 1) {
        print_list(inputs.shift())
        console.log(",")
    }
    print_list(inputs.shift())
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

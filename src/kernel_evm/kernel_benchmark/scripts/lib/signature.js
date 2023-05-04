// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// Use this module in a nodeJs program to sign transaction an ethereum transaction provided as a json, using a given private key.
// See transactions_example/*.json for examples of format.
// Note that a deployement transaction must not include a "to" field.
// Returns a raw transaction:
// let rawTx2 = {
// rawData: any[];
// msgHash: any;
// DER: number[];
// signature: Buffer;
// rawTx: string; // signed transaction
// }
// rlp encoded signed transaction is rawTx2.rawTx
// r is rawTx2.signature.slice(0, 32)
// s is rawTx2.signature.slice(32, 64)

// see sign_tx.js script

const { sign: enthereumjs_sign } = require('@warren-bank/ethereumjs-tx-sign')

const BN = require('bn.js')

const clean_input = function (str) {
    // nb in hex format
    if ((typeof str === 'number') || (str.isBigNumber === true) || BN.isBN(str)) str = `${str.toString(16)}`
    // wrong type data
    if ((!str) || (typeof str !== 'string') || (str === '0x')) str = '00'
    // normalise 0x prefix if present
    if (str.indexOf('0x') === 0) str = str.substr(2)
    // leading 0 if needed
    if (str.length % 2 === 1) str = `0${str}`
    // return in hex format lead by 0x
    return `0x${str}`
}

const sign = function (json, privateKey) {
    // remove 0x if present
    if (privateKey.indexOf('0x') === 0) privateKey = privateKey.substr(2)

    if (json.to == undefined) {
        // clean input
        const txData2 = {
            nonce: clean_input(json.nonce),
            gasPrice: clean_input(json.gasPrice),
            gasLimit: clean_input(json.gasLimit),
            // to: clean_input(json.to),
            value: clean_input(json.value),
            data: json.data,
            chainId: json.chainId
        }
        // sign transaction
        return enthereumjs_sign(txData2, privateKey);
    } else {
        // clean input
        const txData2 = {
            nonce: clean_input(json.nonce),
            gasPrice: clean_input(json.gasPrice),
            gasLimit: clean_input(json.gasLimit),
            to: clean_input(json.to),
            value: clean_input(json.value),
            data: json.data,
            chainId: json.chainId
        }
        // sign transaction
        return enthereumjs_sign(txData2, privateKey);
    }
}
module.exports = { sign, clean_input }
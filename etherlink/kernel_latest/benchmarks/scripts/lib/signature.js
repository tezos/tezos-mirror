// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// Use this module in a nodeJs program to sign an ethereum transaction
// provided as a json, using a given private key.
// See transactions_example/*.json for examples of format.
// Note that a contract creation transaction must not include a "to" field.
// Returns a struct with the signed transaction and other useful values:
// {
//   rawTx: string,       // signed rlp encoded transaction, no 0x
//   msgHash: any,        // msgHash used for signature
//   txData: transaction, // cleaned up transaction data (no signature)
//   r: any,              // signature : r
//   s: any,              // signature : s
//   v: number,           // signature : v (eip-155, include chain_id encoding)
// }

// see sign_tx.js script

const { sign: enthereumjs_sign, unsign: ethereumjs_unsign } = require('@warren-bank/ethereumjs-tx-sign')

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

// sign and compile all usefull info in a struct
const sign_info = function (txData2, privateKey) {
    let signed = enthereumjs_sign(txData2, privateKey);
    let unsigned = ethereumjs_unsign(signed.rawTx);
    return {
        msgHash: signed.msgHash,
        rawTx: signed.rawTx,
        txData: txData2,
        r: unsigned.signature.r,
        s: unsigned.signature.s,
        v: parseInt(unsigned.signature.v, 16)
    }
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
        return sign_info(txData2, privateKey);
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
        return sign_info(txData2, privateKey);
    }
}

module.exports = { sign, clean_input }
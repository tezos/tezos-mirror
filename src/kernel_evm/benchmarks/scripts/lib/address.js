// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// Use this module to create wallet info, and "players" for benchmark
// see make_addr.js

var Wallet = require('ethereumjs-wallet');


const create_wallet = function (privateKey) {
    if (arguments.length == 0) {
        return Wallet.default.generate();
    } else {

        // remove 0x if present
        if (privateKey.indexOf('0x') === 0) privateKey = privateKey.substr(2)
        var array = new Uint8Array(privateKey.match(/../g).map(h => parseInt(h, 16)))
        var buf = Buffer.from(array);
        return Wallet.default.fromPrivateKey(buf);
    }

}

const print_wallet = function (wallet) {
    console.log("address: " + wallet.getAddressString());
    console.log("privateKey: " + wallet.getPrivateKeyString());
    console.log("publicKey: " + wallet.getPublicKeyString());

}

const create_player = function (wallet) {
    if (arguments.length == 0) {
        wallet = create_wallet()
    }
    return {
        addr: wallet.getAddressString(),
        privateKey: wallet.getPrivateKeyString(),
        publicKey: wallet.getPublicKeyString(),
        nonce: 0
    }

}

const print_player = function (wallet) {
    console.log(JSON.stringify(self.create_player(wallet), null, 1))

}

module.exports = { create_wallet, create_player, print_wallet, print_player }
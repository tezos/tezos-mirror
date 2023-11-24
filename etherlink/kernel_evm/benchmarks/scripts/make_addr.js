// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// Use this script to generate ethereum addresses.
// Either by providing a private key, or at random.
// In either cases, the key pair is printed along side the address.
// usage:
// - node make_addr.js
// - node make_addr.js 'dcdff53b4f013dbcdc717f89fe3bf4d8b10512aae282b48e01d7530470382701'
// dependency: npm install ethereumjs-wallet --save

const addr = require('./lib/address')

var wallet;
if (process.argv.length == 3) {
    wallet = addr.create_wallet(process.argv[2]);
} else {
    wallet = addr.create_wallet();
}

addr.print_wallet(wallet)

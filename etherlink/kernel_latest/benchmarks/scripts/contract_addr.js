// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// Use this script tocompute the address of a contract from the sender address
// and nonce

// usage:  node ./contract_addr.js "0x6ce4d79d4E77402e1ef3417Fdda433aA744C6e1c" 0
// expected: d77420f73b4612a7a99dba8c2afd30a1886b0344

const { legacy_contract_address } = require('./lib/contract');

// get args
let addr = process.argv[2]
let nonce = process.argv[3]

console.log(legacy_contract_address(addr, parseInt(nonce)))
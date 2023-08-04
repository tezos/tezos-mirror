// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// use this module to compute the address of a contract from sender address and
// nonce

const rlp = require("rlp");
const keccak = require("keccak");
const { clean_input } = require('./signature');

// nonce must be a number !
const legacy_contract_address = function (player_address, nonce) {
    let hex_addr = clean_input(player_address)
    let rlp_encoded = rlp.encode([hex_addr, nonce]);
    var contract_address_long = keccak("keccak256")
        .update(rlp_encoded)
        .digest("hex");
    return contract_address_long.substring(24);
}

module.exports = { legacy_contract_address }
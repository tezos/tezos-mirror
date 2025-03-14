// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT
pragma solidity <=0.4.26;

contract CallPrecompile {
    function testCallcode() public payable {
        address to = 0xff00000000000000000000000000000000000001;
        to.callcode.value(msg.value)(abi.encodeWithSignature("withdraw_base58(string)", "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j"));
    }
}
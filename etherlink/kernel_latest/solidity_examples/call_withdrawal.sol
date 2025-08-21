// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.0;

contract CallPrecompile {
    function testCall() public payable {
        address to = 0xff00000000000000000000000000000000000001;

        (bool success, ) = to.call{value: msg.value}(
            abi.encodeWithSignature(
                "withdraw_base58(string)",
                "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j"
            )
        );
        require(success);
    }

    function testDelegatecall() public payable {
        address to = 0xff00000000000000000000000000000000000001;

        (bool success, ) = to.delegatecall(
            abi.encodeWithSignature(
                "withdraw_base58(string)",
                "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j"
            )
        );
        require(success);
    }

    function testStaticcall() public payable {
        address to = 0xff00000000000000000000000000000000000001;

        (bool success, ) = to.staticcall(
            abi.encodeWithSignature(
                "withdraw_base58(string)",
                "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j"
            )
        );
        require(success);
    }

}
// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

pragma solidity >=0.8.2 <0.9.0;

contract DummyComputation {
    uint256 public result;

    function addToFive(uint256 a) public {
        result = a + 5;

        if (result > 10) {
            revert("Result exceeds the maximum allowed value of 10");
        }
    }
}

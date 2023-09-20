// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

pragma solidity >=0.8.2 <0.9.0;
contract Loop {
    uint256 count;

    // ask the contract to run the loop a given number of times
    function loop(uint256 iter) public {
        for (uint256 i = 0; i < iter; i++) {
        count += 1;
        }

    }
}
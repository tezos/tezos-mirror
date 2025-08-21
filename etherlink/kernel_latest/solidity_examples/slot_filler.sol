// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.0;

contract SlotFiller {
    uint256 public counter;

    // Mapping to fill storage slots dynamically
    mapping(uint256 => uint256) public storageMap;

    // Function that keeps filling storage slots until it runs out of gas
    function fillSlots() external {
        while (true) {
            storageMap[counter] = counter;
            unchecked {
                counter++;
            }
        }
    }
}

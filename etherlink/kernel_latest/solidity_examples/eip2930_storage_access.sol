// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.0;

contract StorageAccess {
    uint256 public value = 1;

    function setValue(uint256 newValue) public {
        value = newValue;
    }

    receive() external payable {}
}

// SPDX-FileCopyrightText: 2025 Nomadic Labs
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.20;

contract UseTransientStorage {
    function store(uint256 key, uint256 value) external {
        assembly {
            tstore(key, value)
        }
    }

    function load(uint256 key) external view returns (uint256 val) {
        assembly {
            val := tload(key)
        }
    }
}
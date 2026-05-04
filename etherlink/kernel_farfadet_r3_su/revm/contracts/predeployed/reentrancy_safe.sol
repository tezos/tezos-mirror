// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

contract ReentrancySafe {
    bool private transient locked;

    modifier nonReentrant() {
        require(!locked, "Reentrancy is not allowed");
        locked = true;
        _;
        locked = false;
    }
}

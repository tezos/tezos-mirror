// SPDX-FileCopyrightText: 2025 Nomadic Labs
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.0;

contract RecursiveCaller {
    uint256 public depth;

    function recurse() public {
        if (gasleft() < 100_000) return;
        depth += 1;
        this.recurse(); // external recursive call
    }
}
// SPDX-FileCopyrightText: 2025 Nomadic Labs
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.0;

contract MemoryBlower {
    function expandMemory() public {
        for (uint i = 0; i < 1_000_000; i++) {
            if (gasleft() < 100_000) break;
            assembly {
                mstore(mul(i, 0x20), i)
            }
        }
    }
}
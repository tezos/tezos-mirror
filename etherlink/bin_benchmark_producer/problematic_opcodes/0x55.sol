// SPDX-FileCopyrightText: 2025 Nomadic Labs
//
// SPDX-License-Identifier: MIT

// Triggers SSTORE by writing to storage
contract UseSStore {
    uint256 public x;

    function store(uint256 value) public {
        x = value; // This compiles down to an SSTORE (0x55)
    }
}
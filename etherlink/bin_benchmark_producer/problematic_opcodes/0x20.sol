// SPDX-FileCopyrightText: 2025 Nomadic Labs
//
// SPDX-License-Identifier: MIT

// Triggers SHA3 via keccak256()
contract UseSHA3 {
    function hashSomething(bytes memory data) public pure returns (bytes32) {
        return keccak256(data); // EVM emits 0x20 (SHA3)
    }
}
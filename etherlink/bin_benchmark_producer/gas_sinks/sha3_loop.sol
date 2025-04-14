// SPDX-FileCopyrightText: 2025 Nomadic Labs
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.17;

contract UseSHA3Loop {
    function hashManyTimes(bytes memory data) public returns (bytes32) {
        bytes32 hash = keccak256(data);
        for (uint i = 0; i < 1_000_000; i++) {
            if (gasleft() < 100_000) break;
            hash = keccak256(abi.encodePacked(hash));
        }
        return hash;
    }
}
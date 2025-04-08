// SPDX-FileCopyrightText: 2025 Nomadic Labs
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.20;

// Triggers CALLDATACOPY during calldata read
contract UseCalldataCopy {
    function echo() public pure returns (bytes memory result) {
        assembly {
            // Allocate 36 bytes: 32 for length, 4 for data
            result := mload(0x40)
            mstore(result, 4) // set length = 4
            let dataStart := add(result, 32)
            calldatacopy(dataStart, 0, 4) // copy first 4 bytes of calldata to result
            mstore(0x40, add(dataStart, 32)) // update free memory pointer
        }
    }
}
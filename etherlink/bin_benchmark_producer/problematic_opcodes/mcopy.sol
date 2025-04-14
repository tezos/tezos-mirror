// SPDX-FileCopyrightText: 2025 Nomadic Labs
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.20;

contract UseMCopy {
    function mcopy() external pure returns (bytes memory result) {
    assembly {
        let src := mload(0x40)
        mstore(src, 0x12345678)

        let resultMem := add(src, 0x20)
        mcopy(resultMem, src, 0x20)

        result := mload(0x40)
        mstore(result, 0x20)               // length = 32 bytes
        mstore(add(result, 0x20), mload(resultMem)) // copy content
        mstore(0x40, add(result, 0x40))    // move free memory pointer
    }
}
}
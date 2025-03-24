// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

contract MCOPY {
    bytes public source;
    bytes public destination;
    uint256 constant WORD_SIZE = 32;

    constructor() {
        source = new bytes(WORD_SIZE);
        for (uint256 i = 0; i < WORD_SIZE; i++) source[i] = bytes1(uint8(i));

        // Initialize destination with the same length (will be overwritten)
        destination = new bytes(WORD_SIZE);
    }

    function mcopy() external {
        // Copy the source from storage to memory.
        bytes memory src = source;
        // Allocate a new memory array with the same length as src.
        bytes memory dest = new bytes(src.length);

        assembly {
            // The actual byte data starts at an offset of 32 bytes.
            // Use the MCOPY opcode to copy `len` bytes from src to dest.
            mcopy(add(dest, WORD_SIZE), add(src, WORD_SIZE), WORD_SIZE)
        }
        // Write the copied memory array back to the destination state variable.
        destination = dest;
    }

    function getSource() external view returns (bytes memory) {
        return source;
    }

    function getDestination() external view returns (bytes memory) {
        return destination;
    }
}

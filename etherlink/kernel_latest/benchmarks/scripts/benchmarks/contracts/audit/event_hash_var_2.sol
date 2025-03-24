// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract event_hash {
    event LogHash(
        address indexed sender,
        bytes32 messageHash
    );

    function hashing (uint256 runs, uint256 init) pure internal returns (bytes32) {
        // convert address to bytes memory
        bytes memory input = abi.encodePacked(init);
        bytes32 result;
        for (uint256 i = 0; i < runs; i++) {
            result = keccak256(input);
            // convert bytes32 to bytes memory
            input = abi.encodePacked(result);

        }
        return result;
    }

    function event_hashing(uint256 event_loops, uint256 hash_loops) public {
        for (uint256 i=0; i < event_loops; i++) {
            emit LogHash(
                msg.sender,
                hashing(hash_loops, event_loops)
            );
        }

    }
}

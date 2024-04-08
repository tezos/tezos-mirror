// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract event_hash {
    event LogHash(
        address indexed sender,
        bytes32 messageHash
    );

    function event_hashing(uint256 runs) public {
        for (uint256 i=0; i < runs; i++) {
            emit LogHash(
                msg.sender,
                keccak256(abi.encodePacked(keccak256(abi.encodePacked(keccak256(abi.encodePacked(keccak256(abi.encodePacked(
                    keccak256(abi.encodePacked(keccak256(abi.encodePacked(keccak256(abi.encodePacked(keccak256(abi.encodePacked(
                        keccak256(abi.encodePacked(keccak256(abi.encodePacked(keccak256(abi.encodePacked(keccak256(abi.encodePacked(
                            keccak256(abi.encodePacked(keccak256(abi.encodePacked(keccak256(abi.encodePacked(keccak256(abi.encodePacked(
                                keccak256(abi.encodePacked(keccak256(abi.encodePacked(keccak256(abi.encodePacked(keccak256(abi.encodePacked(
                                    keccak256(abi.encodePacked(keccak256(abi.encodePacked(keccak256(abi.encodePacked(keccak256(abi.encodePacked(
                                        keccak256(abi.encodePacked(keccak256(abi.encodePacked(keccak256(abi.encodePacked(keccak256(abi.encodePacked(
                                            keccak256(abi.encodePacked(keccak256(abi.encodePacked(keccak256(abi.encodePacked(keccak256(abi.encodePacked(
                                                keccak256(abi.encodePacked(keccak256(abi.encodePacked(keccak256(abi.encodePacked(keccak256(abi.encodePacked(
                                                    keccak256(abi.encodePacked((runs)))
                                                ))))))))
                                            ))))))))
                                        ))))))))
                                    ))))))))
                                ))))))))
                            ))))))))
                        ))))))))
                    ))))))))
                ))))))))
            );
        }
    }
}

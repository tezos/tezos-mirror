// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract event_topic {
    event LogLongTopic(
        address indexed sender,
        uint256 a1,
        uint256 a2,
        uint256 a3,
        uint256 a4,
        uint256 a5,
        uint256 a6,
        uint256 a7,
        uint256 a8,
        uint256 a9,
        uint256 a10,
        uint256 a11,
        uint256 a12
    );

    function event_long_topic(uint256 runs) public {
        for (uint256 i=0; i < runs; i++) {
            emit LogLongTopic(
                msg.sender,
                1,
                2,
                3,
                4,
                5,
                6,
                7,
                8,
                9,
                10,
                11,
                12
            );
        }

    }
}

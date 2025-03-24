// SPDX-License-Identifier: MIT
pragma solidity ^0.8.17;

contract log3 {
    event Log3(
        address indexed send1,
        address indexed send2
    );

    function run( uint256 loops ) public {
        for (uint256 i=0; i< loops; i++) {
            emit Log3(msg.sender, msg.sender);
        }
    }
}

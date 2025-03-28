// SPDX-License-Identifier: MIT
pragma solidity ^0.8.17;

contract log4 {
    event Log4(
        address indexed send1,
        address indexed send2,
        address indexed send3
    );

    function run( uint256 loops ) public {
        for (uint256 i=0; i< loops; i++) {
            emit Log4(msg.sender, msg.sender, msg.sender);
        }
    }
}

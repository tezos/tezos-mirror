// SPDX-License-Identifier: MIT
pragma solidity ^0.8.17;

contract log2 {
    event Log2(
        address indexed send1
    );

    function run( uint256 loops ) public {
        for (uint256 i=0; i< loops; i++) {
            emit Log2(msg.sender);
        }
    }
}

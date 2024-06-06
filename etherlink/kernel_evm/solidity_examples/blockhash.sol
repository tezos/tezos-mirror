// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract Blockhash {
    function getHash(uint number) public view returns (bytes32) {
        return blockhash(number);
    }
}

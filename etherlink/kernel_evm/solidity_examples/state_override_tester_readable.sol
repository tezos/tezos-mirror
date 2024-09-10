// SPDX-License-Identifier: MIT
pragma solidity ^0.8.4;

contract StateOverrideTester {
    int private count = 42;

    function getBalance() public view returns (uint256) {
        return msg.sender.balance;
    }

    function getCount() public view returns (int) {
        return count;
    }
}

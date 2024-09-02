// SPDX-License-Identifier: MIT
pragma solidity ^0.8.4;

contract StateOverrideTester {

    function getBalance() public view returns (uint256) {
        return msg.sender.balance;
    }
}

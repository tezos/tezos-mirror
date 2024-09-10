// SPDX-License-Identifier: MIT
pragma solidity ^0.8.4;

contract Child {

    constructor() payable {
    }
}

contract StateOverrideTester {

    int private count = 42;

    function getBalance() public view returns (uint256) {
        return msg.sender.balance;
    }

    function create() public
    returns (address addr)
    {
        Child child = new Child();
        return address(child);
    }
}

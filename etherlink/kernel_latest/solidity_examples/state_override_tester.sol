// SPDX-License-Identifier: MIT
pragma solidity ^0.8.4;

contract Child {

    constructor() payable {
    }
}

contract StateOverrideTester {

    uint32 private count = 42; // private, so requires an explicit accessor
    uint32 public const2 = 0xffffffff;
    uint256 public sep = 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff;
    uint32 public const3 = 0xffffffff;

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

// SPDX-License-Identifier: MIT
pragma solidity ^0.8.4;

contract StateOverrideTester {
    // first two 32bit value should be in same memory slot
    uint32 private count = 42; // private, so require an explicit accessor
    uint32 public const2 = 0xffffffff;

    // define a 32byte value to open a new memory slot
    uint256 public sep = 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff;

    // should be a third slot
    uint32 public const3 = 0xffffffff;

    function getBalance() public view returns (uint256) {
        return msg.sender.balance;
    }

    function getCount() public view returns (uint32) {
        return count;
    }
}

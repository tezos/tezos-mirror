// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.17;

contract MappingStorage {
    uint pos0;
    mapping(address => uint) pos1;
    function foo() public {
        pos0 = 1234;
        pos1[msg.sender] = 5678;
    }
}

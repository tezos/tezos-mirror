// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.17;

interface IEvents {
    function emitA(uint value) external;
    function emitB(uint value) external;
    function emitBoth(uint value) external;

    event EventA(address indexed from, address indexed to, uint value);
    event EventB(address indexed owner, address indexed spender, uint value);
}


contract Events is IEvents {
    function emitA(uint value) external  {
        emit EventA(address(0), msg.sender, value);
    }

    function emitB(uint value) external {
        emit EventB(msg.sender, address(0), value);
    }

    function emitBoth(uint value) external {
        emit EventA(address(0), msg.sender, value);
        emit EventB(msg.sender, address(0), value);
    }
}

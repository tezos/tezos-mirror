// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

pragma solidity >=0.8.2 <0.9.0;

contract SimpleLogger {
    event LogValue(uint256 indexed value);

    function logValue(uint256 value) public {
        emit LogValue(value);
    }
}

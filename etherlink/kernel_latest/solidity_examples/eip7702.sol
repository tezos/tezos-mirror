// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

contract EIP7702Contract {
    event EIP7702Call(address caller);

    function emitEvent() external payable {
        emit EIP7702Call(msg.sender);
    }
}

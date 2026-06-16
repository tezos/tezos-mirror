// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

contract EIP7702FallbackContract {
    event EIP7702FallbackCall(address caller);

    fallback() external payable {
        emit EIP7702FallbackCall(msg.sender);
    }

    receive() external payable {
        emit EIP7702FallbackCall(msg.sender);
    }
}

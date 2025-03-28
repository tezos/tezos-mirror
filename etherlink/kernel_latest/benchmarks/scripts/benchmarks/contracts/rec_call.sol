// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.17;
contract A {

    // trigger an unbounded recursion
    function call() public {
        A callee = A(address(this));
        callee.call();
    }
}
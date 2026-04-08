// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.17;

/// @title StoreAndReturn
/// @notice Minimal contract that stores a uint256 and returns it in the same
///         call.  Used by CRAC callback e2e tests: the gateway forwards the
///         return value to the Michelson callback contract.
contract StoreAndReturn {
    uint256 public value;

    function store(uint256 _value) public returns (uint256) {
        value = _value;
        return _value;
    }
}

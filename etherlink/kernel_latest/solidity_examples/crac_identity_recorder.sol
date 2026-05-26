// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.20;

/// @title IdentityRecorder
/// @notice Records msg.sender and tx.origin of the last call to record().
///         Used by the regression tezt to verify that an EVM ->
///         gateway -> EVM round-trip preserves the native caller's identity
///         (msg.sender must be the calling EVM contract, never the alias
///         derived from its address).
contract IdentityRecorder {
    address public lastSender;
    address public lastOrigin;

    /// Aliased to `run()` so the contract slots straight into the
    /// existing CRAC runner harness, which always invokes `run()` on
    /// the target.
    function run() external payable {
        lastSender = msg.sender;
        lastOrigin = tx.origin;
    }
}

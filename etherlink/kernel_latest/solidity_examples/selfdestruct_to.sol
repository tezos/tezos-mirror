// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.20;

/// @notice Strands its full balance at `target` via SELFDESTRUCT: the
/// balance transfer bypasses `target`'s receive()/fallback() entirely
/// (SELFDESTRUCT moves value without executing code), so it never triggers
/// a forwarder sweep there. Used to test residue recovery on a later poke.
contract SelfdestructTo {
    function destructTo(address payable target) external payable {
        selfdestruct(target);
    }
}

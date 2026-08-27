// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity >=0.8.2 <0.9.0;

contract Reverter {
    event WillRevert(uint256 indexed value);

    function emitAndRevert(uint256 value) external {
        emit WillRevert(value);
        revert("reverting on purpose");
    }
}

// The parent low-level-calls a child that emits a log then reverts,
// swallows the revert, and emits its own log. Expected callTracer shape:
// the root keeps [Ok], the child frame keeps its error and loses WillRevert.
contract RevertSwallower {
    event Ok(uint256 indexed value);

    Reverter public reverter;

    constructor() {
        reverter = new Reverter();
    }

    function run(uint256 value) external {
        (bool ok, ) = address(reverter).call(
            abi.encodeWithSignature("emitAndRevert(uint256)", value)
        );
        if (!ok) {
            // Swallow the revert.
        }
        emit Ok(value);
    }
}

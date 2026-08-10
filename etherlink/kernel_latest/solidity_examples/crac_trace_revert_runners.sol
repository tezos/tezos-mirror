// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.20;

// Minimal runners used by the `crac_trace_revert` tezt suite
// (see cross_runtime.ml). Each runner exposes a parameterless `run()`
// so it can be invoked by the cross-runtime bridge
// `cross_runtime_run_evm.tz`.

/// @notice Child of `CracReverter` that emits a log and returns
///         successfully. It runs (and closes, recording its log) BEFORE
///         its parent reverts, so the reverted re-entrant subtree has an
///         already-closed descendant whose log must be dropped - the
///         path exercised by the call tracer's descendant `drop_logs`
///         (call_tracer.rs).
contract CracReverterChild {
    event Marked(uint256 n);

    function emitAndReturn() external {
        emit Marked(1);
    }
}

/// @notice Runner whose `run()` first drives a child sub-call that emits
///         a log and returns, then reverts with a known string reason.
///         The reason is identical to the public constant `REASON()` so
///         tests can assert on the exact bytes returned by the EVM in
///         `Error(string)` form.
contract CracReverter {
    string public constant REASON = "CRAC-REVERT-MARK";
    address public child;

    function initialize(address _child) external {
        child = _child;
    }

    function run() external payable {
        // The child frame closes (and records its log) before the revert
        // below unwinds it, so the reverted subtree carries an
        // already-closed descendant with a dropped log.
        CracReverterChild(child).emitAndReturn();
        revert(REASON);
    }
}

// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.20;

// Minimal runners used as the EVM leaf of a CRAC pipeline by the
// `crac_trace_call_tracer` tezt suite (see cross_runtime.ml). Each runner
// exposes a parameterless `run()` so it can be invoked by the
// cross-runtime bridge `cross_runtime_run_evm.tz`. The body of `run()`
// performs exactly one inner call of the targeted type so the captured
// REVM call tree, nested under the synthetic CRAC top-level frame,
// contains a frame whose `type` matches DELEGATECALL or STATICCALL.

/// @notice Library invoked via DELEGATECALL by `DelegateRunner.run()`.
contract DelegateLib {
    function inc() external pure returns (uint256) {
        return 1;
    }
}

/// @notice Runner whose `run()` performs a single DELEGATECALL.
contract DelegateRunner {
    address public delegate;

    function initialize(address _delegate) external {
        delegate = _delegate;
    }

    function run() external payable {
        (bool ok, ) = delegate.delegatecall(
            abi.encodeWithSignature("inc()")
        );
        require(ok, "delegatecall failed");
    }
}

/// @notice Pure-view target read via STATICCALL by `StaticRunner.run()`.
contract StaticView {
    function readOne() external pure returns (uint256) {
        return 1;
    }
}

/// @notice Runner whose `run()` performs a single STATICCALL.
contract StaticRunner {
    address public target;

    function initialize(address _target) external {
        target = _target;
    }

    function run() external payable {
        (bool ok, ) = target.staticcall(
            abi.encodeWithSignature("readOne()")
        );
        require(ok, "staticcall failed");
    }
}

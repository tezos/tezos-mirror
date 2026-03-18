// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// Calls `run()` on a configurable list of callees.
//
// `run()` iterates over a list of callees. For each callee, it
// increments `count` then calls `callee.run()`. Each callee must
// expose a parameterless `run()` function. If `doCatch` is set
// for a callee, the call is wrapped in try/catch and `catches` is
// incremented on failure. After all callees, the contract reverts
// if `willRevert` is true, otherwise increments `count` one last
// time.
//
// The `count` witness and `catches` counter allow tests to verify
// exactly which parts of the call tree executed and which were
// reverted.
//
// Since this contract itself exposes `run()`, it can be composed
// with itself and with cross-runtime adapters
// (cross_runtime_run_tez.sol, cross_runtime_run_evm.tz) to build
// arbitrary cross-runtime call trees.
//
// Michelson counterpart: multi_run_caller.tz (without catch).

interface IRunnable {
    function run() external payable;
}

contract MultiRunCaller {
    uint256 public catches;
    uint256 public count;
    bool public willRevert;

    struct Callee {
        address addr;
        bool doCatch;
    }

    Callee[] public callees;

    function initialize(bool _willRevert, Callee[] calldata _callees) external {
        willRevert = _willRevert;
        delete callees;
        for (uint256 i = 0; i < _callees.length; i++) {
            callees.push(_callees[i]);
        }
    }

    function incrementWitness() internal {
        count++;
    }

    function run() external payable {
        for (uint256 i = 0; i < callees.length; i++) {
            incrementWitness();
            if (callees[i].doCatch) {
                try IRunnable(callees[i].addr).run{value: 0}() {}
                catch { catches++; }
            } else {
                IRunnable(callees[i].addr).run{value: 0}();
            }
        }
        if (willRevert) {
            revert();
        }
        incrementWitness();
    }
}

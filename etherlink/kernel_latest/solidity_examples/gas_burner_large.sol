// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// Burns a large, predictable amount of gas when run() is called.
//
// Identical in shape to gas_burner.sol but with many more storage slots
// (480) so that a warm run() burns roughly 2.5M EVM gas.  This is used by
// the CRAC gas-accounting investigation test (cross_runtime.ml), where the
// inner EVM cost must dominate the CRAC overhead by at least 10x for the
// gas-accounting bounds to be robust.  The cross-runtime CRAC overhead grew
// when the Michelson<->EVM gas conversion was recalibrated, so the standard
// gas_burner.sol (180 slots, ~950K gas) no longer dominates; this larger
// variant restores the margin.  Callers must supply a high enough gas limit
// (the first, cold call writes ~22,100 gas per slot, i.e. ~10.6M total).
//
// Exposes the standard run() interface so it can be used as a leaf in the
// CRAC runner test framework (cross_runtime.ml).

contract GasBurnerLarge {
    uint256[480] public data;

    function run() external payable {
        for (uint256 i = 0; i < 480; i++) {
            data[i] = data[i] + 1;
        }
    }
}

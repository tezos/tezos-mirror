// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// Burns a large, predictable amount of gas when run() is called.
//
// Each iteration writes to a distinct storage slot (warm SSTORE ~5000 gas
// after the first call).  With 120 slots the run() function burns roughly
// 600000+ EVM gas on warm calls, which is enough to:
//   - make callee-gas charging clearly observable in cross-runtime tests
//   - dominate the CRAC overhead (precompile + aliases + TEZ execution)
//     by at least 10x, ensuring gas accounting bounds are robust
//
// Exposes the standard run() interface so it can be used as a leaf in the
// CRAC runner test framework (cross_runtime.ml).

contract GasBurner {
    uint256[120] public data;

    function run() external payable {
        for (uint256 i = 0; i < 120; i++) {
            data[i] = data[i] + 1;
        }
    }
}

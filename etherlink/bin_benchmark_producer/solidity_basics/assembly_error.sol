// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract AssemblyError {
    function yul_revert(uint x) public pure returns (uint z) {
        assembly {
            if gt(x, 10) { revert(0, 0) }
        }
    }
}

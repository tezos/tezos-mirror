// SPDX-License-Identifier: MIT
pragma solidity ^0.8.17;

contract sdiv_opcode {

    function run(uint256 runs, int256 numerator, int256 denominator) public pure {
        for (uint256 i=0; i < runs; i++) {
            assembly {
                let result := sdiv(numerator, denominator)
            }
        }
    }
}

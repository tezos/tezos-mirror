/// SPDX-License-Identifier: MIT
pragma solidity ^0.8.17;

contract smod_opcode {

    function run(uint256 runs, int256 numerator, int256 denominator) public pure {
        for (uint256 i=0; i < runs; i++) {
            assembly {
                let result := smod(numerator, denominator)
            }
        }
    }
}

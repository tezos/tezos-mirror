// SPDX-License-Identifier: MIT
pragma solidity ^0.8.17;

contract mulmod_opcode {

    function run(uint256 runs, uint256 valueA, uint256 valueB, uint256 denominator) public pure {
        for (uint256 i=0; i < runs; i++) {
            assembly {
                let result := mulmod(valueA, valueB, denominator)
            }
        }
    }
}

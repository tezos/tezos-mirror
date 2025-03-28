/// SPDX-License-Identifier: MIT
pragma solidity ^0.8.17;

contract signextend_opcode {

    function run(uint256 runs, uint256 extend, uint256 x) public pure {
        for (uint256 i=0; i < runs; i++) {
            assembly {
                let result := signextend(extend, x)
            }
        }
    }
}

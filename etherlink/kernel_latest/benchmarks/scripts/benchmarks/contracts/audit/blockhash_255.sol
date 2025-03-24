// SPDX-License-Identifier: MIT
pragma solidity ^0.8.17;

contract b_hash {

    function run(uint256 runs) view public {
        for (uint256 i = 0; i < runs; i++) {
            assembly {
                let hash := blockhash(255)
            }
        }
    }
}

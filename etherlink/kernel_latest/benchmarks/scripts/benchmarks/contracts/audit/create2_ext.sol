// SPDX-License-Identifier: MIT
pragma solidity ^0.8.17;

contract create_ext {

    function run(uint256 runs, bytes memory code) public {
        for (uint256 i = 0; i < runs; i++) {
            assembly {
                let size := mload(code)
                let new_contract := create2(0, add(code,0x20), size, i)
            }
        }
    }
}

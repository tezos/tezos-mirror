// SPDX-License-Identifier: MIT
pragma solidity ^0.8.17;

contract keccak_ext {

    function run(uint256 runs, address remote) view public {
        for (uint256 i = 0; i < runs; i++) {
            assembly {
                let fp := mload(0x40)
                let size := extcodesize(remote)
                extcodecopy(remote, fp, 0x00, size)
                let hash := keccak256(fp, size)

            }
        }
    }
}

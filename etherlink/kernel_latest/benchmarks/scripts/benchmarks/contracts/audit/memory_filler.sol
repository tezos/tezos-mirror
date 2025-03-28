// SPDX-License-Identifier: MIT
pragma solidity ^0.8.17;

contract mem {
    function run(uint256 loops) pure public {
        for (uint256 i=0; i< loops; i++) {
            assembly {
                let fp := mload(0x40)
                mstore(fp,i)
                mstore(0x40,add(fp,0x20))
            }
        }
    }
}

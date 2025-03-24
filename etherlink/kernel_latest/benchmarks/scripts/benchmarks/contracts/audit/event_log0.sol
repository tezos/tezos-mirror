// SPDX-License-Identifier: MIT
pragma solidity ^0.8.17;

contract log0 {
    struct Something {
            uint256 value;
    }

    function run( uint256 loops ) public {
        // Store a value in a struct in order to prevent the value gets stored on the stack.
        Something memory some = Something(1);
        for (uint256 i=0; i< loops; i++) {
            assembly {
                log0(0x80,0x20)
            }
        }
    }

}

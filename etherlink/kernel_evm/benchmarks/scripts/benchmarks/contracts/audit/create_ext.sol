// SPDX-License-Identifier: MIT
pragma solidity ^0.8.17;

contract create_ext {

    function run(bytes memory code) public {
        assembly {
            let size := mload(code)
            let new_contract := create(0, add(code,0x20), size)
        }

    }
}

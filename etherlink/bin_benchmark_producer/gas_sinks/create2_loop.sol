// SPDX-FileCopyrightText: 2025 Nomadic Labs
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.17;

contract DeployerLoop {
    function deployMany(bytes memory bytecode, bytes32 salt) public {
        for (uint i = 0; i < 1_000_000; i++) {
            if (gasleft() < 100_000) break;
            address addr;
            bytes32 salted = keccak256(abi.encodePacked(salt, i));
            assembly {
                addr := create2(0, add(bytecode, 0x20), mload(bytecode), salted)
            }
            require(addr != address(0), "CREATE2 failed");
        }
    }
}
// SPDX-FileCopyrightText: 2025 Nomadic Labs
//
// SPDX-License-Identifier: MIT

contract Deployer {
    function deploy(bytes memory bytecode, bytes32 salt) public returns (address) {
        address addr;
        assembly {
            addr := create2(0, add(bytecode, 0x20), mload(bytecode), salt) // 0xf5
        }
        require(addr != address(0), "CREATE2 failed");
        return addr;
    }
}
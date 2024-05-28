// SPDX-License-Identifier: MIT
pragma solidity ^0.8.17;

contract calldata_example {
    mapping (uint256 => string) sto;

    function store(string calldata variable, uint256 runner) public {
        for (uint256 i = 0; i < runner; i++) {
            sto[i] = variable;
        }
    }
}

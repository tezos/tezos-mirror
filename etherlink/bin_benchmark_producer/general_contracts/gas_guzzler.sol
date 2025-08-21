// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.0;

contract GasGuzzler {
    uint public sum = 0;

    // Only updates a local variable
    function incrementLocalSum(uint iterations) public view returns (uint) {
        uint localSum = 0;
        for (uint i = 0; i < iterations; i++) {
            // Perform some operation that involves a state variable
            localSum += i;
            // Adding some entropy to confuse constant propagation and
            // loop unrolling optimizations
            if ((localSum % 10) == 0) {
                localSum += block.timestamp % 10;
            }
        }
        return sum;
    }

    // Updates a global variable in every iteration of the loop
    function incrementGlobalSum(uint iterations) public returns (uint) {
        for (uint i = 0; i < iterations; i++) {
            // Perform some operation that involves a state variable
            sum += i;
            // Adding some entropy to confuse constant propagation and
            // loop unrolling optimizations
            if ((sum % 10) == 0) {
                sum += block.timestamp % 10;
            }
        }
        // Update the state variable at the end of the loop
        return sum;
    }
}

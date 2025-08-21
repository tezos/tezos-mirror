// SPDX-License-Identifier: MIT

pragma solidity ^0.8.17;

contract EvenBlockGasConsumer {
    uint val = 0;

    function consume() public {
        if (block.number % 2 == 0) for (uint i = 0; i < 10000; i++) val += i;
    }
}

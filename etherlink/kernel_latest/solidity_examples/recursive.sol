// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract Recursive {
    function call(uint x) public returns (bool) {
        if (x > 0) {
            Recursive c = this;
            c.call(x - 1);
        }
        return true;
    }
}

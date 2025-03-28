// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.8.2 <0.9.0;

contract GasLeft {
    function check() public view {
        if (gasleft() <= 30_000_000) {
            revert();
        }
    }

}

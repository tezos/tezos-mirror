// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.8.2 <0.9.0;

contract Gas_limit {
    function retrieve() public view returns (uint256){
        return block.gaslimit;
    }
}
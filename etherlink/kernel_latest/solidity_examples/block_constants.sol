//SPDX-License-Identifier: MIT

pragma solidity ^0.8.0;

contract BlockConstants {
    uint256 block_number;
    uint256 block_timestamp;

    function set_block_number() public {
        block_number = block.number;
    }

    function set_timestamp() public {
        block_timestamp = block.timestamp;
    }

    function view_stored_block_number () public view returns (uint256){
        return block_number;
    }

    function view_stored_block_timestamp () public view returns (uint256){
        return block_timestamp;
    }
}
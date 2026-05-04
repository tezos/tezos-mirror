// SPDX-License-Identifier: MIT
pragma solidity ^0.8.30;

error RevertCreate(address addr);

contract CreateAndRevert {
    function createAndRevert(bytes memory bytecode) public {
        address child;
        assembly{
            mstore(0x0, bytecode)
            child := create(0,0xa0, calldatasize())
        }
        revert RevertCreate({
            addr: child
        });
    }
}

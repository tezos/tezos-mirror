// SPDX-License-Identifier: MIT
pragma solidity ^0.8.30;

contract CallAndRevert {
    function callAndRevert(address target, bytes memory callArgs) public {
        (bool success, ) = target.call(callArgs);
        require(success, "Call failed");
        revert("Reverting");
    }
}

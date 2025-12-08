// SPDX-License-Identifier: MIT
pragma solidity ^0.8.30;

contract AlwaysRevert {
    error AlwaysReverts();

    receive() external payable {
        revert AlwaysReverts();
    }

    fallback() external payable {
        revert AlwaysReverts();
    }
}

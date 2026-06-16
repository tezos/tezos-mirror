// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

contract DrainBalance {
    /// @notice Transfers the entire balance of this contract (i.e. the
    ///         delegating EOA when called via EIP-7702) to `to`.
    function drain(address payable to) external {
        (bool success,) = to.call{value: address(this).balance}("");
        require(success, "transfer failed");
    }
}

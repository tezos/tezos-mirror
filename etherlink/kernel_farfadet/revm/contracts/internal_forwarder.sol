// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

import "./constants.sol";

contract InternalForwarder {
    modifier onlyTicketTable() {
        require(
            msg.sender == Constants.faWithdrawals,
            "InternalForwarder: unauthorized caller"
        );
        _;
    }

    /// @notice Forward a call to a target contract with provided data.
    /// @dev Only callable by the FAWithdrawal contract.
    /// @param target The address of the target contract to call.
    /// @param data The calldata to send to the target contract.
    /// @return success True if the forwarded call succeeded, false otherwise.
    /// @return returnData The returned data from the forwarded call.
    function forward(
        address target,
        bytes calldata data
    ) external onlyTicketTable returns (bool success, bytes memory returnData) {
        (success, returnData) = target.call(data);
    }

    receive() external payable {}

    fallback() external payable {}
}

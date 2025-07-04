// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

contract InternalForwarder {
    address private constant faWithdrawals =
        0xff00000000000000000000000000000000000002;

    modifier onlyTicketTable() {
        require(
            msg.sender == faWithdrawals,
            "InternalForwarder: unauthorized caller"
        );
        _;
    }

    function forward(
        address target,
        bytes calldata data
    ) external onlyTicketTable returns (bool success, bytes memory returnData) {
        (success, returnData) = target.call(data);
    }

    receive() external payable {}

    fallback() external payable {}
}

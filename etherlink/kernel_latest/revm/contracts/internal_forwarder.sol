// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

contract InternalForwarder {
    address public constant TICKET_TABLE =
        0xFf00000000000000000000000000000000000004;

    modifier onlyTicketTable() {
        require(
            msg.sender == TICKET_TABLE,
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
}

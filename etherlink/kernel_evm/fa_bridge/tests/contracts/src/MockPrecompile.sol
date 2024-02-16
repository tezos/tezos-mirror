// SPDX-FileCopyrightText: 2023 PK Lab <contact@pklab.io>
//
// SPDX-License-Identifier: MIT

pragma solidity >=0.8.19;

/**
 * MockWrapper is a helper contract to generate ABIs
 * for the FA brige precompile.
 */
contract MockPrecompile {

    event Deposit(
        uint256 indexed ticketHash,
        address ticketOwner,
        address receiver,
        uint256 amount,
        uint256 inboxLevel,
        uint256 inboxMsgId
    );

    event Withdrawal(
        uint256 indexed ticketHash,
        address sender,
        address ticketOwner,
        bytes22 receiver,
        uint256 amount,
        uint256 outboxLevel,
        uint256 outboxMsgId

    );

    function withdraw(
        address ticketOwner,
        bytes memory receiver,
        uint256 amount,
        bytes22 ticketer,
        bytes memory content
    ) public {}
}
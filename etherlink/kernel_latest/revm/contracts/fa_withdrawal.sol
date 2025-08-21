// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

contract FAWithdrawal {
    uint256 public withdrawalCounter;

    event SolStandardWithdrawalEvent(
        address indexed sender,
        address indexed ticketOwner,
        // TODO: https://gitlab.com/tezos/tezos/-/issues/8021
        // bytes22 receiver,
        // bytes22 proxy,
        uint256 amount,
        uint256 withdrawalId
    );

    function withdraw(
        address ticketOwner,
        bytes memory routingInfo,
        uint256 amount,
        bytes22 ticketer,
        bytes memory content
    ) external payable {
        address outboxSender = 0xFF00000000000000000000000000000000000003;
        address ticketTable = 0xFf00000000000000000000000000000000000004;

        // Revert if amount is zero
        require(amount > 0, "Empty withdrawals are not allowed");

        // Compute ticket hash
        uint256 ticketHash = uint256(
            keccak256(abi.encodePacked(ticketer, content))
        );

        // Call ticketTable.ticket_balance_remove
        bytes memory ticketData = abi.encodeWithSignature(
            "ticket_balance_remove(uint256,address,uint256)",
            ticketHash,
            ticketOwner,
            amount
        );
        (bool ticketSuccess, ) = ticketTable.call(ticketData);
        require(
            ticketSuccess,
            "Failed to decrement ticket balance: ticket_balance_remove call was unsuccessful"
        );

        // Call proxy
        bytes memory proxyData = abi.encodeWithSignature(
            "withdraw(address,uint256,uint256)",
            msg.sender,
            amount,
            ticketHash
        );
        (bool proxySuccess, ) = ticketOwner.call(proxyData);
        require(proxySuccess, "Proxy call failed");

        // Call outboxSender.push_fa_withdrawal_to_outbox
        bytes memory payload = abi.encodeWithSignature(
            "push_fa_withdrawal_to_outbox(bytes,uint256,bytes22,bytes)",
            routingInfo,
            amount,
            ticketer,
            content
        );
        (bool ok, ) = outboxSender.call(payload);
        require(
            ok,
            "Failed to produce message: push_fa_withdrawal_to_outbox call unsuccessful"
        );

        // Emit withdrawal event
        emit SolStandardWithdrawalEvent(
            msg.sender,
            ticketOwner,
            amount,
            withdrawalCounter
        );

        // Increment withdrawal counter
        withdrawalCounter++;
    }
}

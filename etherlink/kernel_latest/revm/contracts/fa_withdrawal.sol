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

    event ClaimDepositEvent(
        address ticketOwner,
        address receiver,
        uint256 amount,
        uint256 inboxLevel,
        uint256 inboxMsgId
    );

    struct FaDepositWithProxy {
        uint256 amount;
        address receiver;
        address proxy;
        uint256 ticketHash;
        uint256 inboxLevel;
        uint256 inboxMsgId;
    }

    function decode(
        bytes memory input
    ) internal pure returns (FaDepositWithProxy memory) {
        return abi.decode(input, (FaDepositWithProxy));
    }

    function claim(uint256 depositId) external payable {
        address system = 0x0000000000000000000000000000000000000000;
        address ticketTable = 0xFf00000000000000000000000000000000000004;

        // Retrieve deposit
        bytes memory input = abi.encodeWithSignature(
            "find_deposit(uint256)",
            depositId
        );
        (bool success, bytes memory output) = ticketTable.call(input);
        require(success, "Could not find deposit");

        // Decode deposit data
        FaDepositWithProxy memory deposit = decode(output);

        // Encode call data sent to the deposit proxy
        bytes memory proxyCallData = abi.encodeWithSignature(
            "deposit(address,uint256,uint256)",
            deposit.receiver,
            deposit.amount,
            deposit.ticketHash
        );

        // Encode and send to the system forwarded to call
        bytes memory systemInput = abi.encodeWithSignature(
            "forward(address,bytes)",
            deposit.proxy,
            proxyCallData
        );
        (bool proxySuccess, ) = system.call(systemInput);

        // If proxy succeeds it becomes the ticketOwner if not fallback on the receiver
        address ticketOwner = proxySuccess ? deposit.proxy : deposit.receiver;

        // Call ticketTable.ticket_balance_remove
        bytes memory ticketData = abi.encodeWithSignature(
            "ticket_balance_add(uint256,address,uint256)",
            deposit.ticketHash,
            ticketOwner,
            deposit.amount
        );
        (bool ticketSuccess, ) = ticketTable.call(ticketData);
        require(
            ticketSuccess,
            "Failed to decrement ticket balance: ticket_balance_remove call was unsuccessful"
        );

        // TODO: Warn tooling maintainers about these event changes, previously:
        // - Emitted from addr zero
        // - Keccak256 of Deposit(uint256,address,address,uint256,uint256,uint256) as topic
        emit ClaimDepositEvent(
            ticketOwner,
            deposit.receiver,
            deposit.amount,
            deposit.inboxLevel,
            deposit.inboxMsgId
        );

        // Remove deposit
        bytes memory remove = abi.encodeWithSignature(
            "remove_deposit(uint256)",
            depositId
        );
        (bool removeSuccess, ) = ticketTable.call(remove);
        require(removeSuccess, "Could not remove deposit");
    }

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

// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

contract FAWithdrawal {
    uint256 public withdrawalCounter;
    bool private locked;

    event Withdrawal(
        uint256 indexed ticketHash,
        address sender,
        address ticketOwner,
        bytes22 receiver,
        bytes22 proxy,
        uint256 amount,
        uint256 withdrawalId
    );

    // TODO: Warn tooling maintainers about these event changes, previously:
    // - Keccak256 of FastFaWithdrawal(address,address,bytes22,bytes22,uint256,uint256,uint256,bytes)
    // - First uint256 is missing and required for ticket hash topic
    event FastFaWithdrawal(
        uint256 indexed ticketHash,
        address sender,
        address ticketOwner,
        bytes22 receiver,
        bytes22 proxy,
        uint256 amount,
        uint256 withdrawalId,
        uint256 timestamp,
        bytes payload
    );

    struct RoutingInfo {
        bytes22 target;
        bytes22 proxy;
    }

    event Deposit(
        uint256 indexed ticketHash,
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

    modifier nonReentrant() {
        require(!locked, "Reentrancy is not allowed");
        locked = true;
        _;
        locked = false;
    }

    function decode(
        bytes memory input
    ) internal pure returns (FaDepositWithProxy memory) {
        return abi.decode(input, (FaDepositWithProxy));
    }

    function claim(uint256 depositId) external payable nonReentrant {
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
        (bool forwarderSuccess, bytes memory proxyReturn) = system.call(
            systemInput
        );
        require(forwarderSuccess, "System forwarder failed");
        (bool proxySuccess, ) = abi.decode(proxyReturn, (bool, bytes));

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

        // Emit claimed deposit event
        emit Deposit(
            deposit.ticketHash,
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
    ) external payable nonReentrant {
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
        bytes memory outboxData = abi.encodeWithSignature(
            "push_fa_withdrawal_to_outbox(bytes,uint256,bytes22,bytes)",
            routingInfo,
            amount,
            ticketer,
            content
        );
        (bool ok, bytes memory encoded) = outboxSender.call(outboxData);
        require(
            ok,
            "Failed to produce message: push_fa_withdrawal_to_outbox call unsuccessful"
        );
        RoutingInfo memory decoded = abi.decode(encoded, (RoutingInfo));

        // Emit withdrawal event
        emit Withdrawal(
            ticketHash,
            msg.sender,
            ticketOwner,
            decoded.target,
            decoded.proxy,
            amount,
            withdrawalCounter
        );

        // Increment withdrawal counter
        withdrawalCounter++;
    }

    function fa_fast_withdraw(
        address ticketOwner,
        bytes memory routingInfo,
        uint256 amount,
        bytes22 ticketer,
        bytes memory content,
        string memory fastWithdrawalContract,
        bytes memory payload
    ) external payable nonReentrant {
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

        // Call outboxSender.push_fast_fa_withdrawal_to_outbox
        bytes memory outboxData = abi.encodeWithSignature(
            "push_fast_fa_withdrawal_to_outbox(bytes,uint256,bytes22,bytes,string,bytes,uint256)",
            routingInfo,
            amount,
            ticketer,
            content,
            fastWithdrawalContract,
            payload,
            withdrawalCounter
        );
        (bool ok, bytes memory encoded) = outboxSender.call(outboxData);
        require(
            ok,
            "Failed to produce message: push_fast_fa_withdrawal_to_outbox call unsuccessful"
        );
        RoutingInfo memory decoded = abi.decode(encoded, (RoutingInfo));

        // Emit withdrawal event
        emit FastFaWithdrawal(
            ticketHash,
            msg.sender,
            ticketOwner,
            decoded.target,
            decoded.proxy,
            amount,
            withdrawalCounter,
            block.timestamp,
            payload
        );

        // Increment withdrawal counter
        withdrawalCounter++;
    }
}

// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

import "./constants.sol";
import "./interfaces.sol";
import "./internal_forwarder.sol";
import "./reentrancy_safe.sol";

contract FAWithdrawal is ReentrancySafe {
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

    event Deposit(
        uint256 indexed ticketHash,
        address ticketOwner,
        address receiver,
        uint256 amount,
        uint256 inboxLevel,
        uint256 inboxMsgId
    );

    /// @notice Claim a queued FA deposit identified by `depositId`.
    /// @param depositId The identifier of the deposit to claim.
    function claim(uint256 depositId) external payable nonReentrant {
        // Retrieve deposit
        bytes memory input = abi.encodeCall(ITable.find_deposit, (depositId));
        (bool success, bytes memory output) = Constants.ticketTable.call(input);
        require(success, "Could not find deposit");

        // Decode deposit data
        ITable.FaDepositWithProxy memory deposit = abi.decode(
            output,
            (ITable.FaDepositWithProxy)
        );

        // Encode call data sent to the deposit proxy
        bytes memory proxyCallData = abi.encodeCall(
            IProxy.deposit,
            (deposit.receiver, deposit.amount, deposit.ticketHash)
        );

        // Encode and send to the system forwarded to call
        bytes memory systemInput = abi.encodeCall(
            InternalForwarder.forward,
            (deposit.proxy, proxyCallData)
        );
        (bool forwarderSuccess, bytes memory proxyReturn) = Constants
            .system
            .call(systemInput);
        require(forwarderSuccess, "System forwarder failed");
        (bool proxySuccess, ) = abi.decode(proxyReturn, (bool, bytes));

        // If proxy succeeds it becomes the ticketOwner if not fallback on the receiver
        address ticketOwner = proxySuccess ? deposit.proxy : deposit.receiver;

        // Call ticketTable.ticket_balance_add
        bytes memory ticketData = abi.encodeCall(
            ITable.ticket_balance_add,
            (deposit.ticketHash, ticketOwner, deposit.amount)
        );
        (bool ticketSuccess, ) = Constants.ticketTable.call(ticketData);
        require(
            ticketSuccess,
            "Failed to increment ticket balance: ticket_balance_add call was unsuccessful"
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
        bytes memory remove = abi.encodeCall(
            ITable.remove_deposit,
            (depositId)
        );
        (bool removeSuccess, ) = Constants.ticketTable.call(remove);
        require(removeSuccess, "Could not remove deposit");
    }

    /// @notice Withdraw a specific amount of FA ticket.
    /// @param ticketOwner The FA ticket owner.
    /// @param routingInfo Routing information for the FA ticket transfer.
    /// @param amount The amount of the FA ticket to withdraw.
    /// @param ticketer The FA ticket issuer, encoded as `bytes22`.
    /// @param content Metadata associated with the FA ticket used to build the outbox message.
    function withdraw(
        address ticketOwner,
        bytes memory routingInfo,
        uint256 amount,
        bytes22 ticketer,
        bytes memory content
    ) external payable nonReentrant {
        // Revert if amount is zero
        require(amount > 0, "Empty withdrawals are not allowed");

        // Compute ticket hash
        uint256 ticketHash = uint256(
            keccak256(abi.encodePacked(ticketer, content))
        );

        // Get and increment global counter
        uint256 counter = ICounter(Constants.globalCounter).get_and_increment();

        // Call ticketTable.ticket_balance_remove
        bytes memory ticketData = abi.encodeCall(
            ITable.ticket_balance_remove,
            (ticketHash, ticketOwner, amount)
        );
        (bool ticketSuccess, ) = Constants.ticketTable.call(ticketData);
        require(
            ticketSuccess,
            "Failed to decrement ticket balance: ticket_balance_remove call was unsuccessful"
        );

        // Call outboxSender.push_fa_withdrawal_to_outbox
        bytes memory outboxData = abi.encodeCall(
            IOutbox.push_fa_withdrawal_to_outbox,
            (routingInfo, amount, ticketer, content)
        );
        (bool outboxSuccess, bytes memory encoded) = Constants
            .outboxSender
            .call(outboxData);
        require(
            outboxSuccess,
            "Failed to produce message: push_fa_withdrawal_to_outbox call unsuccessful"
        );
        IOutbox.RoutingInfo memory decoded = abi.decode(
            encoded,
            (IOutbox.RoutingInfo)
        );

        // Emit withdrawal event
        emit Withdrawal(
            ticketHash,
            msg.sender,
            ticketOwner,
            decoded.target,
            decoded.proxy,
            amount,
            counter
        );

        // Call proxy
        bytes memory proxyData = abi.encodeCall(
            IProxy.withdraw,
            (msg.sender, amount, ticketHash)
        );
        (bool proxySuccess, ) = ticketOwner.call(proxyData);
        require(proxySuccess, "Proxy withdraw failed");
    }

    /// @notice Perform a fast withdrawal of FA ticket.
    /// @param ticketOwner The FA ticket owner.
    /// @param routingInfo Routing information for the FA ticket transfer.
    /// @param amount The amount of FA the ticket to withdraw.
    /// @param ticketer The ticket issuer, encoded as `bytes22`.
    /// @param content Metadata associated with the ticket.
    /// @param fastWithdrawalContract Address of the fast withdrawal lending contract to interact with.
    /// @param payload Arbitrary payload data to send to the fast withdrawal contract.
    function fa_fast_withdraw(
        address ticketOwner,
        bytes memory routingInfo,
        uint256 amount,
        bytes22 ticketer,
        bytes memory content,
        string memory fastWithdrawalContract,
        bytes memory payload
    ) external payable nonReentrant {
        // Revert if amount is zero
        require(amount > 0, "Empty withdrawals are not allowed");

        // Compute ticket hash
        uint256 ticketHash = uint256(
            keccak256(abi.encodePacked(ticketer, content))
        );

        // Get and increment global counter
        uint256 counter = ICounter(Constants.globalCounter).get_and_increment();

        // Call ticketTable.ticket_balance_remove
        bytes memory ticketData = abi.encodeCall(
            ITable.ticket_balance_remove,
            (ticketHash, ticketOwner, amount)
        );
        (bool ticketSuccess, ) = Constants.ticketTable.call(ticketData);
        require(
            ticketSuccess,
            "Failed to decrement ticket balance: ticket_balance_remove call was unsuccessful"
        );

        // Call outboxSender.push_fast_fa_withdrawal_to_outbox
        bytes memory outboxData = abi.encodeCall(
            IOutbox.push_fast_fa_withdrawal_to_outbox,
            (
                routingInfo,
                amount,
                ticketer,
                content,
                fastWithdrawalContract,
                payload,
                counter
            )
        );
        (bool outboxSuccess, bytes memory encoded) = Constants
            .outboxSender
            .call(outboxData);
        require(
            outboxSuccess,
            "Failed to produce message: push_fast_fa_withdrawal_to_outbox call unsuccessful"
        );
        IOutbox.RoutingInfo memory decoded = abi.decode(
            encoded,
            (IOutbox.RoutingInfo)
        );

        // Emit withdrawal event
        emit FastFaWithdrawal(
            ticketHash,
            msg.sender,
            ticketOwner,
            decoded.target,
            decoded.proxy,
            amount,
            counter,
            block.timestamp,
            payload
        );

        // Call proxy
        bytes memory proxyData = abi.encodeCall(
            IProxy.withdraw,
            (msg.sender, amount, ticketHash)
        );
        (bool proxySuccess, ) = ticketOwner.call(proxyData);
        require(proxySuccess, "Proxy withdraw failed");
    }
}

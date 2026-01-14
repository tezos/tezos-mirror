// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

import "./constants.sol";
import "./interfaces.sol";
import "./internal_forwarder.sol";
import "./reentrancy_safe.sol";

contract FABridge is ReentrancySafe {
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

    event QueuedDeposit(
        uint256 indexed ticketHash,
        address indexed proxy,
        uint256 nonce,
        address receiver,
        uint256 amount,
        uint256 inboxLevel,
        uint256 inboxMsgId
    );

    struct FaDepositWithoutProxy {
        uint256 amount;
        address receiver;
        uint256 ticketHash;
        uint256 inboxLevel;
        uint256 inboxMsgId;
    }

    modifier onlyFeed() {
        require(
            msg.sender == Constants.feedDepositAddress,
            "InternalForwarder: unauthorized caller"
        );
        _;
    }

    /// @notice Queue a deposit into the ticket table with proxy information.
    /// @dev
    ///  - Can only be called through the feed deposit address
    ///  - Increments the global counter and uses it as a deposit nonce.
    ///  - Queues the deposit for later execution.
    /// @param deposit Struct containing the deposit data:
    ///   - `amount`      Amount of the deposit.
    ///   - `receiver`    Address that will receive the deposited funds.
    ///   - `ticketHash`  Hash identifying the deposit, computed as keccak256(L1 ticketer + content).
    ///   - `proxy`       Proxy address through which the deposit is routed.
    ///   - `inboxLevel`  Inbox level at which the deposit was submitted, directly correlated with L1 block levels.
    ///   - `inboxMsgId`  Inbox message identifier.
    function queue(
        ITable.FaDepositWithProxy memory deposit
    ) external payable onlyFeed nonReentrant {
        // Get and increment global counter
        uint256 counter = ICounter(Constants.globalCounter).get_and_increment();

        // Push deposit to the queue
        bytes memory input = abi.encodeCall(
            ITable.queue_deposit,
            (deposit, counter)
        );
        (bool success, ) = Constants.ticketTable.call(input);
        require(success, "Could not queue deposit");

        // Emit queued deposit event
        emit QueuedDeposit(
            deposit.ticketHash,
            deposit.proxy,
            counter,
            deposit.receiver,
            deposit.amount,
            deposit.inboxLevel,
            deposit.inboxMsgId
        );
    }

    /// @notice Executes a deposit that does not require a proxy.
    /// @dev
    ///  - Can only be called through the feed deposit address
    ///  - Used for direct deposits without a proxy address.
    ///  - Consumes and processes the deposit information.
    /// @param deposit Struct containing the deposit data:
    ///   - `amount`      Amount of the deposit.
    ///   - `receiver`    Address that will receive the deposited funds.
    ///   - `ticketHash`  Hash identifying the deposit, computed as keccak256(L1 ticketer + content).
    ///   - `inboxLevel`  Inbox level at which the deposit was submitted, directly correlated with L1 block levels.
    ///   - `inboxMsgId`  Inbox message identifier.
    function execute_without_proxy(
        FaDepositWithoutProxy memory deposit
    ) external payable onlyFeed nonReentrant {
        // Increase ticket balance of receiver as there is no proxy
        bytes memory ticketData = abi.encodeCall(
            ITable.ticket_balance_add,
            (deposit.ticketHash, deposit.receiver, deposit.amount)
        );
        (bool ticketSuccess, ) = Constants.ticketTable.call(ticketData);
        require(ticketSuccess, "Failed to increment ticket balance");

        // Emit claimed deposit event
        // We have receiver twice as both ticket owner and receiver
        emit Deposit(
            deposit.ticketHash,
            deposit.receiver,
            deposit.receiver,
            deposit.amount,
            deposit.inboxLevel,
            deposit.inboxMsgId
        );
    }

    /// @notice Claims a previously queued FA deposit for later execution.
    /// @dev
    ///  - Consumes and processes a deposit that was registered via `queue`.
    ///  - Can only be called once per depositId.
    ///  - Ticket owner becomes L2 proxy most of the times.
    ///  - When proxy fails fallback on the deposit receiver.
    /// @param depositId Identifier of the queued deposit to claim.
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

        // Encode and send to the system forwarder to call
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

    /// @notice Withdraws a specific amount of FA tickets to L1.
    /// @param ticketOwner   Address of the FA ticket owner (usually the L2 proxy).
    /// @param routingInfo   Tuple containing the L1 receiver address and the L1 ticketer contract for routing.
    /// @param amount        Amount of FA ticket balance to withdraw.
    /// @param ticketer      L1 contract that issued the FA ticket, encoded as `bytes22`.
    /// @param content       Metadata associated with the FA ticket, used to build the outbox message.
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

        // Encode proxy data
        bytes memory proxyData = abi.encodeCall(
            IProxy.withdraw,
            (msg.sender, amount, ticketHash)
        );

        // Send proxy data to the system forwarder
        bytes memory systemInput = abi.encodeCall(
            InternalForwarder.forward,
            (ticketOwner, proxyData)
        );
        (bool forwarderSuccess, bytes memory proxyReturn) = Constants
            .system
            .call(systemInput);
        require(forwarderSuccess, "System forwarder failed");

        // Decode proxy execution return
        (bool proxySuccess, ) = abi.decode(proxyReturn, (bool, bytes));
        require(proxySuccess, "Proxy withdraw failed");
    }

    /// @notice Performs a fast withdrawal of FA tickets to L1.
    /// @param ticketOwner              Address of the FA ticket owner (usually the L2 proxy).
    /// @param routingInfo              Tuple containing the L1 receiver address and the L1 ticketer contract for routing.
    /// @param amount                   Amount of FA ticket balance to withdraw.
    /// @param ticketer                 L1 contract that issued the FA ticket, encoded as `bytes22`.
    /// @param content                  Metadata associated with the FA ticket, used to build the outbox message.
    /// @param fastWithdrawalContract   Address of the L1 fast withdrawal lending contract to interact with.
    /// @param payload                  Arbitrary payload data to forward to the fast withdrawal contract.
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

        // Encode proxy data
        bytes memory proxyData = abi.encodeCall(
            IProxy.withdraw,
            (msg.sender, amount, ticketHash)
        );

        // Send proxy data to the system forwarder
        bytes memory systemInput = abi.encodeCall(
            InternalForwarder.forward,
            (ticketOwner, proxyData)
        );
        (bool forwarderSuccess, bytes memory proxyReturn) = Constants
            .system
            .call(systemInput);
        require(forwarderSuccess, "System forwarder failed");

        // Decode proxy execution return
        (bool proxySuccess, ) = abi.decode(proxyReturn, (bool, bytes));
        require(proxySuccess, "Proxy withdraw failed");
    }
}

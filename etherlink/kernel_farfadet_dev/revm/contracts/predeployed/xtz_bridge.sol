// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

import "./constants.sol";
import "./interfaces.sol";
import "./reentrancy_safe.sol";
import "./feed_guard.sol";

contract XTZBridge is ReentrancySafe, FeedGuard {
    bool private locked;

    event Withdrawal(
        uint256 amount,
        address sender,
        bytes22 receiver,
        uint256 withdrawalId
    );

    event FastWithdrawal(
        bytes22 receiver,
        uint256 withdrawalId,
        uint256 amount,
        uint256 timestamp,
        bytes payload,
        address l2Caller
    );

    struct XTZDeposit {
        address receiver;
        uint256 inbox_level;
        uint256 inbox_msg_id;
    }

    event Deposit(
        uint256 amount,
        address receiver,
        uint256 inbox_level,
        uint256 inbox_msg_id
    );

    event QueuedDeposit(
        uint256 amount,
        uint256 nonce,
        address receiver,
        uint256 inbox_level,
        uint256 inbox_msg_id
    );

    struct QueuedDepositEntry {
        uint256 amount;
        address receiver;
        uint256 inbox_level;
        uint256 inbox_msg_id;
        bool exists;
    }

    // @dev global deposit nonce
    uint256 private globalDepositNonce = 0;

    /// @dev Pending XTZ deposits per deposit id (used for EIP-7702 / contracts).
    mapping(uint256 => QueuedDepositEntry) private queuedDeposits;

    // Convert wei to mutez (1 mutez = 10^12 Wei)
    function mutez_from_wei(uint256 weiAmount) private pure returns (uint256) {
        uint256 mutezAmount = weiAmount / Constants.mutezFactor;
        uint256 remainder = weiAmount % Constants.mutezFactor;

        require(weiAmount > 0, "Amount is zero");
        require(
            remainder == 0,
            "Rounding would lose wei, amount must be multiple of 10^12 (1 mutez)"
        );
        return mutezAmount;
    }

    /// @notice Perform a standard XTZ withdrawal using a Base58-encoded target.
    /// @param target The Base58-encoded recipient or destination address.
    function withdraw_base58(
        string memory target
    ) external payable nonReentrant {
        // Retrieve withdrawn value
        uint256 weiAmount = msg.value;
        uint256 mutezAmount = mutez_from_wei(weiAmount);

        // Get and increment global counter
        uint256 counter = ICounter(Constants.globalCounter).get_and_increment();

        // Call push_withdrawal_to_outbox
        bytes memory outboxData = abi.encodeCall(
            IOutbox.push_withdrawal_to_outbox,
            (target, mutezAmount)
        );
        (bool outboxSuccess, bytes memory receiver) = Constants
            .outboxSender
            .call(outboxData);
        require(outboxSuccess, "Call to the outbox sender failed");

        // Burn (send to burnAddress)
        (bool sent, ) = Constants.burnAddress.call{value: weiAmount}("");
        require(sent, "Failed to burn");

        // Emit withdrawal event
        emit Withdrawal(weiAmount, msg.sender, bytes22(receiver), counter);
    }

    /// @notice Perform a fast XTZ withdrawal using a Base58-encoded target and contract.
    /// @param target The Base58-encoded recipient or destination address.
    /// @param fastWithdrawalContract The Base58-encoded fast withdrawal lending contract to interact with.
    /// @param payload Arbitrary payload data to send to the fast withdrawal contract.
    function fast_withdraw_base58(
        string memory target,
        string memory fastWithdrawalContract,
        bytes memory payload
    ) external payable nonReentrant {
        // Retrieve withdrawn value
        uint256 weiAmount = msg.value;
        uint256 mutezAmount = mutez_from_wei(weiAmount);

        // Get and increment global counter
        uint256 counter = ICounter(Constants.globalCounter).get_and_increment();

        // Call push_fast_withdrawal_to_outbox
        bytes memory outboxData = abi.encodeCall(
            IOutbox.push_fast_withdrawal_to_outbox,
            (target, fastWithdrawalContract, payload, mutezAmount, counter)
        );
        (bool outboxSuccess, bytes memory receiver) = Constants
            .outboxSender
            .call(outboxData);
        require(outboxSuccess, "Call to the outbox sender failed");

        // Burn (send to burnAddress)
        (bool sent, ) = Constants.burnAddress.call{value: weiAmount}("");
        require(sent, "Failed to burn");

        // Emit fast withdrawal event
        emit FastWithdrawal(
            bytes22(receiver),
            counter,
            weiAmount,
            block.timestamp,
            payload,
            msg.sender
        );
    }

    /// @notice Handle an XTZ deposit, either executing it directly or queueing it.
    /// @dev
    /// - Intended to handle both EOAs (with or without code) and contracts.
    /// - The kernel prefunds this call and provides `deposit.amount` as `msg.value`.
    /// - If `deposit.receiver` has no code (EOA), the deposit is executed immediately:
    ///   - No storage is written.
    ///   - Transfers XTZ directly to the receiver.
    ///   - Emits `Deposit`.
    /// - If `deposit.receiver` has code (e.g. EIP-7702 accounts, smart contracts):
    ///   - The deposit is queued using a global nonce.
    ///   - Storage is written.
    ///   - Emits `QueuedDeposit`.
    /// @param deposit The deposit metadata parsed from the inbox.
    function handle_xtz_deposit(
        XTZDeposit memory deposit
    )
        external
        payable
        onlyFeed
        nonReentrant
    {
        // Receiver has code: queue the XTZ deposit
        if (deposit.receiver.code.length > 0) {
            uint256 depositId = globalDepositNonce;
            globalDepositNonce += 1;

            queuedDeposits[depositId] = QueuedDepositEntry({
                amount: msg.value,
                receiver: deposit.receiver,
                inbox_level: deposit.inbox_level,
                inbox_msg_id: deposit.inbox_msg_id,
                exists: true
            });

            emit QueuedDeposit(
                msg.value,
                depositId,
                deposit.receiver,
                deposit.inbox_level,
                deposit.inbox_msg_id
            );
        }
        // Receiver is an EOA without code: execute the XTZ deposit directly
        else {
            (bool sent, ) = deposit.receiver.call{value: msg.value}("");
            require(sent, "XTZ transfer failed");

            emit Deposit(
                msg.value,
                deposit.receiver,
                deposit.inbox_level,
                deposit.inbox_msg_id
            );
        }
    }



    /// @notice Claim queued XTZ deposits for a receiver.
    /// @dev
    /// - Callable by anyone.
    /// - Clears storage before external interaction.
    /// - Sends the right amount to the receiver address.
    /// - Emits `Deposit`.
    /// @param depositId the unique deposit ID to claim.
    function claim_xtz(uint256 depositId) external nonReentrant {
        QueuedDepositEntry memory deposit = queuedDeposits[depositId];
        require(deposit.exists, "No deposit for this ID");

        // Clear storage before external call
        delete queuedDeposits[depositId];

        (bool sent, ) = deposit.receiver.call{value: deposit.amount}("");
        require(sent, "XTZ transfer failed");

        emit Deposit(
            deposit.amount,
            deposit.receiver,
            deposit.inbox_level,
            deposit.inbox_msg_id
        );
    }
}

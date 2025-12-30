// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

import "./constants.sol";
import "./interfaces.sol";
import "./reentrancy_safe.sol";

contract XTZBridge is ReentrancySafe {
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
}

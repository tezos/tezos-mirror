// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

import "./constants.sol";
import "./interfaces.sol";
import "./reentrancy_safe.sol";

contract XTZWithdrawal is ReentrancySafe {
    uint256 public withdrawalCounter;
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
        emit Withdrawal(
            weiAmount,
            msg.sender,
            bytes22(receiver),
            withdrawalCounter
        );

        // Increment ID counter
        withdrawalCounter++;
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

        // Call push_fast_withdrawal_to_outbox
        bytes memory outboxData = abi.encodeCall(
            IOutbox.push_fast_withdrawal_to_outbox,
            (
                target,
                fastWithdrawalContract,
                payload,
                mutezAmount,
                withdrawalCounter
            )
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
            withdrawalCounter,
            weiAmount,
            block.timestamp,
            payload,
            msg.sender
        );

        // Increment ID counter
        withdrawalCounter++;
    }
}

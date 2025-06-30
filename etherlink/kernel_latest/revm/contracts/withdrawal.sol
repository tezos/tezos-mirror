// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

contract XTZWithdrawal {
    uint256 public withdrawalCounter;

    event StandardWithdrawalEvent(
        uint256 amount,
        address indexed sender,
        // bytes22 receiver,
        uint256 withdrawalId
    );

    function withdraw_base58(string memory target) external payable {
        // Outbox precompile is known
        address outboxSender = 0xFF00000000000000000000000000000000000003;

        // Convert to mutez (1 mutez = 10^12 Wei)
        uint256 weiAmount = msg.value;
        uint256 factor = 1e12;
        uint256 mutezAmount = weiAmount / factor;
        uint256 remainder = weiAmount % factor;

        require(weiAmount > 0, "Amount is zero");
        require(
            remainder == 0,
            "Amount must be multiple of 10^12 Wei (1 mutez)"
        );

        // Call push_withdrawal_to_outbox
        bytes memory payload = abi.encodeWithSignature(
            "push_withdrawal_to_outbox(string,uint256)",
            target,
            mutezAmount
        );
        (bool ok, ) = outboxSender.call(payload);
        require(ok, "Call to the outbox sender failed");

        // Burn (send to address(0))
        (bool sent, ) = address(0).call{value: weiAmount}("");
        require(sent, "Failed to burn");

        // Emit withdrawal event
        emit StandardWithdrawalEvent(
            weiAmount,
            msg.sender,
            // TODO: FIX THIS
            // target,
            withdrawalCounter
        );

        // Increment ID counter
        withdrawalCounter++;
    }
}

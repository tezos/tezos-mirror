// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

contract XTZWithdrawal {
    uint256 public withdrawalCounter;

    event StandardWithdrawalEvent(
        address indexed sender,
        uint256 amount,
        bytes22 receiver,
        uint256 withdrawalId
    );

    function withdraw_base58(string calldata b58Address) external payable {
        // Ticketer and outbox precompile are known
        string memory ticketer = "KT1CeFqjJRJPNVvhvznQrWfHad2jCiDZ6Lyj";
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
            "push_withdrawal_to_outbox(string,string,uint256)",
            ticketer,
            b58Address,
            mutezAmount
        );
        (bool ok, ) = outboxSender.call(payload);
        require(ok, "Call to the outbox sender failed");

        // Burn (send to address(0))
        (bool sent, ) = address(0).call{value: weiAmount}("");
        require(sent, "Failed to burn");

        // Emit withdrawal event
        bytes22 target = bytes22(bytes(b58Address));
        emit StandardWithdrawalEvent(
            msg.sender,
            weiAmount,
            target,
            withdrawalCounter
        );

        // Increment ID counter
        withdrawalCounter++;
    }
}

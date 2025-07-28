// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

contract XTZWithdrawal {
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

    modifier nonReentrant() {
        require(!locked, "Reentrancy is not allowed");
        locked = true;
        _;
        locked = false;
    }

    // Convert wei to mutez (1 mutez = 10^12 Wei)
    function mutez_from_wei(uint256 weiAmount) private pure returns (uint256) {
        uint256 factor = 1e12;
        uint256 mutezAmount = weiAmount / factor;
        uint256 remainder = weiAmount % factor;

        require(weiAmount > 0, "Amount is zero");
        require(
            remainder == 0,
            "Rounding would lose wei, amount must be multiple of 10^12 (1 mutez)"
        );
        return mutezAmount;
    }

    function withdraw_base58(
        string memory target
    ) external payable nonReentrant {
        // Outbox precompile is known
        address outboxSender = 0xFF00000000000000000000000000000000000003;

        // Retrieve withdrawn value
        uint256 weiAmount = msg.value;
        uint256 mutezAmount = mutez_from_wei(weiAmount);

        // Call push_withdrawal_to_outbox
        bytes memory outboxData = abi.encodeWithSignature(
            "push_withdrawal_to_outbox(string,uint256)",
            target,
            mutezAmount
        );
        (bool ok, bytes memory receiver) = outboxSender.call(outboxData);
        require(ok, "Call to the outbox sender failed");

        // Burn (send to address(0))
        (bool sent, ) = address(0).call{value: weiAmount}("");
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

    function fast_withdraw_base58(
        string memory target,
        string memory fastWithdrawalContract,
        bytes memory payload
    ) external payable nonReentrant {
        // Outbox precompile is known
        address outboxSender = 0xFF00000000000000000000000000000000000003;

        // Retrieve withdrawn value
        uint256 weiAmount = msg.value;
        uint256 mutezAmount = mutez_from_wei(weiAmount);

        // Call push_fast_withdrawal_to_outbox
        bytes memory outboxData = abi.encodeWithSignature(
            "push_fast_withdrawal_to_outbox(string,string,bytes,uint256,uint256)",
            target,
            fastWithdrawalContract,
            payload,
            mutezAmount,
            withdrawalCounter
        );
        (bool ok, bytes memory receiver) = outboxSender.call(outboxData);
        require(ok, "Call to the outbox sender failed");

        // Burn (send to address(0))
        (bool sent, ) = address(0).call{value: weiAmount}("");
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

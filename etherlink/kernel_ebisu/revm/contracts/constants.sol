// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

library Constants {
    uint256 public constant mutezFactor = 1e12;

    address public constant burnAddress =
        0x000000000000000000000000000000000000dEaD;

    address public constant system = 0x0000000000000000000000000000000000000000;

    address public constant faWithdrawals =
        0xff00000000000000000000000000000000000002;

    address public constant outboxSender =
        0xFF00000000000000000000000000000000000003;

    address public constant ticketTable =
        0xFf00000000000000000000000000000000000004;

    address public constant globalCounter =
        0xff00000000000000000000000000000000000005;
}

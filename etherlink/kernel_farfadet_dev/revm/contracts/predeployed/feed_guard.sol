// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

import "./constants.sol";

abstract contract FeedGuard {
    modifier onlyFeed() {
        require(
            msg.sender == Constants.feedDepositAddress,
            "unauthorized caller"
        );
        _;
    }
}

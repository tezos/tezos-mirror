// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

interface IOutbox {
    function push_withdrawal_to_outbox(
        string calldata target,
        uint256 amount
    ) external;

    function push_fast_withdrawal_to_outbox(
        string calldata target,
        string calldata fast_withdrawal_contract,
        bytes calldata payload,
        uint256 amount,
        uint256 withdrawal_id
    ) external;

    function push_fa_withdrawal_to_outbox(
        bytes calldata routing_info,
        uint256 amount,
        bytes22 ticketer,
        bytes calldata content
    ) external;

    function push_fast_fa_withdrawal_to_outbox(
        bytes calldata routing_info,
        uint256 amount,
        bytes22 ticketer,
        bytes calldata content,
        string calldata fast_withdrawal_contract_address,
        bytes calldata payload,
        uint256 withdrawal_id
    ) external;

    struct RoutingInfo {
        bytes22 target;
        bytes22 proxy;
    }
}

interface ITable {
    function ticket_balance_add(
        uint256 ticket_hash,
        address owner,
        uint256 amount
    ) external;

    function ticket_balance_remove(
        uint256 ticket_hash,
        address owner,
        uint256 amount
    ) external;

    function find_deposit(uint256 deposit_id) external;

    function remove_deposit(uint256 deposit_id) external;

    struct FaDepositWithProxy {
        uint256 amount;
        address receiver;
        address proxy;
        uint256 ticketHash;
        uint256 inboxLevel;
        uint256 inboxMsgId;
    }
}

interface ICounter {
    function get_and_increment() external returns (uint256);
}

interface IProxy {
    function deposit(
        address target,
        uint256 amount,
        uint256 ticketHash
    ) external;

    function withdraw(
        address target,
        uint256 amount,
        uint256 ticetHash
    ) external;
}

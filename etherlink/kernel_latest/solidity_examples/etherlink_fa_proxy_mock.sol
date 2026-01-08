// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

// see https://github.com/baking-bad/etherlink-bridge/blob/main/etherlink/src/ERC20Proxy.sol
// for reference implementation
contract Proxy {
    address public constant system = 0x0000000000000000000000000000000000000000;
    uint256 _ticketHash;

    function hashTicket(
        bytes22 ticketer,
        bytes memory content
    ) private pure returns (uint256 ticketHash) {
        ticketHash = uint256(keccak256(abi.encodePacked(ticketer, content)));
    }

    constructor(bytes22 ticketer, bytes memory content) {
        _ticketHash = hashTicket(ticketer, content);
    }

    function withdraw(address, uint256, uint256 ticketHash) public view {
        require(msg.sender == system);
        require(ticketHash == _ticketHash);
    }

    function deposit(address, uint256, uint256 ticketHash) public view {
        require(msg.sender == system);
        require(ticketHash == _ticketHash);
    }
}

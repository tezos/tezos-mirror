// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

/// @title AliasForwarder
/// @notice Contract deployed at alias addresses to forward tez to native Tezos addresses
/// @dev This contract is deployed as a precompile and alias addresses use EIP-7702
///      delegation to point to it. Each alias stores its native address in storage.
contract AliasForwarder {
    /// @notice The RuntimeGateway precompile address for cross-runtime transfers
    address constant RUNTIME_GATEWAY = 0xfF00000000000000000000000000000000000007;

    /// @notice The TezosX internal caller address (only this address can call init)
    address constant TEZOSX_CALLER = 0x7e20580000000000000000000000000000000001;

    /// @notice The native Tezos address this alias forwards to (stored at slot 0)
    string public nativeAddress;

    /// @notice Whether the alias has been initialized
    bool public initialized;

    /// @notice Emitted when funds are forwarded to the native address
    event Forwarded(string indexed nativeAddress, uint256 amount);

    /// @notice Emitted when the alias is initialized
    event Initialized(string nativeAddress, uint256 forwardedBalance);

    error AlreadyInitialized();
    error NotAuthorized();
    error TransferFailed();

    /// @notice Initialize the alias with its native address
    /// @dev Can only be called once by the TezosX internal address
    /// @param _nativeAddress The Tezos address (e.g., "tz1...") to forward funds to
    function init_tezosx_alias(string calldata _nativeAddress) external payable {
        if (initialized) {
            revert AlreadyInitialized();
        }
        if (msg.sender != TEZOSX_CALLER) {
            revert NotAuthorized();
        }

        nativeAddress = _nativeAddress;
        initialized = true;

        // Forward any pre-existing balance (sent before alias was created)
        uint256 balance = address(this).balance;
        if (balance > 0) {
            _forwardBalance(balance);
        }

        emit Initialized(_nativeAddress, balance);
    }

    /// @notice Receive function to accept plain tez transfers
    receive() external payable {
        _forwardBalance(msg.value);
    }

    /// @notice Fallback function to accept any calls with tez
    fallback() external payable {
        _forwardBalance(msg.value);
    }

    /// @notice Internal function to forward balance to the native address
    /// @param amount The amount to forward
    function _forwardBalance(uint256 amount) internal {
        if (amount == 0) {
            return;
        }

        // Call the RuntimeGateway to transfer to the native Tezos address
        (bool success, ) = RUNTIME_GATEWAY.call{value: amount}(
            abi.encodeWithSignature("transfer(string)", nativeAddress)
        );

        if (!success) {
            revert TransferFailed();
        }

        emit Forwarded(nativeAddress, amount);
    }
}

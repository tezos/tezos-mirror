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

    /// @notice Precompile address for Tezos signature verification
    address constant VERIFY_TEZOS_SIG = 0xfF0000000000000000000000000000000000000a;

    /// @notice The native Tezos address this alias forwards to (stored at slot 0)
    string public nativeAddress;

    /// @notice Whether the alias has been initialized
    bool public initialized;

    /// @notice The native public key of the Tezos account (raw bytes from storage)
    bytes public nativePublicKey;

    /// @notice Emitted when funds are forwarded to the native address
    event Forwarded(string indexed nativeAddress, uint256 amount);

    /// @notice Emitted when the alias is initialized
    event Initialized(string nativeAddress, bytes nativePublicKey, uint256 forwardedBalance);

    error AlreadyInitialized();
    error NotAuthorized();
    error TransferFailed();

    /// @notice Initialize the alias with its native address and public key
    /// @dev Can only be called once by the TezosX internal address
    /// @param _nativeAddress The Tezos address (e.g., "tz1...") to forward funds to
    /// @param _nativePublicKey Binary-encoded public key of the Tezos source account
    function init_tezosx_alias(string calldata _nativeAddress, bytes calldata _nativePublicKey) external payable {
        if (initialized) {
            revert AlreadyInitialized();
        }
        if (msg.sender != TEZOSX_CALLER) {
            revert NotAuthorized();
        }

        nativeAddress = _nativeAddress;
        nativePublicKey = _nativePublicKey;
        initialized = true;

        // Forward any pre-existing balance (sent before alias was created)
        uint256 balance = address(this).balance;
        if (balance > 0) {
            _forwardBalance(balance);
        }

        emit Initialized(_nativeAddress, _nativePublicKey, balance);
    }

    /// @notice EIP-1271 signature validation
    /// @dev Delegates to the Tezos signature verification precompile using
    ///      the stored nativePublicKey.
    ///
    ///      Per Tezos signing conventions, the precompile internally
    ///      Blake2b-hashes the supplied input before running the curve
    ///      verification. Signers must therefore produce the signature
    ///      over `blake2b(hash)`, not over `hash` directly. This differs
    ///      from standard EIP-1271 callers that assume the caller-supplied
    ///      hash is signed as-is.
    /// @param hash The hash that was signed. Note this value is
    ///             Blake2b-hashed again inside the precompile, so the
    ///             signature must cover `blake2b(hash)`.
    /// @param signature The Tezos signature bytes
    /// @return magicValue 0x1626ba7e if valid, 0xffffffff otherwise
    function isValidSignature(bytes32 hash, bytes calldata signature) external view returns (bytes4) {
        (bool ok, bytes memory result) = VERIFY_TEZOS_SIG.staticcall(
            abi.encodeWithSignature(
                "verifyTezosSignature(bytes,bytes32,bytes)",
                nativePublicKey,
                hash,
                signature
            )
        );
        if (ok && result.length >= 32 && abi.decode(result, (bool))) {
            return bytes4(0x1626ba7e);
        }
        return bytes4(0xffffffff);
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

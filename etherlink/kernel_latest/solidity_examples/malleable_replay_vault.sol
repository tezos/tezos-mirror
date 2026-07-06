// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.21;

interface IERC1271 {
    function isValidSignature(bytes32 hash, bytes calldata signature)
        external
        view
        returns (bytes4);
}

/// @title MalleableReplayVault
/// @notice A voucher vault that pays a fixed reward to whoever presents a
///         signature -- by an authorized Tezos account exposed as an EIP-1271
///         `signer` -- over a given `hash`.
///
///         Anti-replay is enforced by remembering *consumed signatures*
///         (`consumed[keccak256(signature)]`). This is the realistic-but-flawed
///         pattern that ECDSA malleability defeats: it assumes a signature is a
///         unique, one-time token per (key, hash). But for any valid (r, s)
///         the twin (r, n - s) (n = curve group order) is an equally-valid
///         signature with a *different* keccak256, so it slips past
///         `consumed` while `isValidSignature` still accepts it. A holder of a
///         single valid voucher signature can therefore redeem twice and drain
///         the vault.
///
///         tz3 (P-256) keys are vulnerable iff the kernel's P-256 verifier
///         accepts high-S signatures. The companion Tezt test exercises exactly
///         this against the verify_tezos_signature precompile (reached via the
///         alias forwarder's isValidSignature).
contract MalleableReplayVault {
    // EIP-1271 magic value returned by isValidSignature on success.
    bytes4 private constant MAGIC = 0x1626ba7e;

    // Fixed reward paid out per accepted, not-yet-consumed signature.
    uint256 public constant PAYOUT = 1 ether;

    // keccak256(signature) => already redeemed.
    mapping(bytes32 => bool) public consumed;

    constructor() payable {}

    /// @notice Redeem the voucher, paying PAYOUT to the caller, on
    ///         presentation of a signature accepted by `signer` over `hash`.
    /// @dev Reverts if the signature was already consumed or is not accepted.
    function redeem(address signer, bytes32 hash, bytes calldata signature)
        external
    {
        bytes32 sigId = keccak256(signature);
        require(!consumed[sigId], "signature already redeemed");
        require(
            IERC1271(signer).isValidSignature(hash, signature) == MAGIC,
            "invalid signature"
        );
        consumed[sigId] = true;
        (bool ok, ) = payable(msg.sender).call{value: PAYOUT}("");
        require(ok, "payout failed");
    }

    receive() external payable {}
}

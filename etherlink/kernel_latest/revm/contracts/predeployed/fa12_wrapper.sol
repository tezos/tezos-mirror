// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

/// @title FA1.2 Token Wrapper (Predeployed)
/// @notice Provides an EVM-friendly interface for FA1.2 tokens on the
/// Michelson runtime via the RuntimeGateway cross-runtime precompile.
/// @dev This is a stateless predeployed contract. The KT1 address of
/// the target FA1.2 token contract is passed as a parameter to each
/// function call.
///
/// Tezos address binary encoding (22 bytes):
///   tz1: 0x0000 + 20-byte Ed25519 public key hash
///   tz2: 0x0001 + 20-byte Secp256k1 public key hash
///   tz3: 0x0002 + 20-byte P256 public key hash
///   KT1: 0x01   + 20-byte contract hash + 0x00
contract FA12Wrapper {
    address constant RUNTIME_GATEWAY =
        0xfF00000000000000000000000000000000000007;

    /// @notice Transfer FA1.2 tokens between Tezos addresses.
    /// @param tokenAddress KT1 address of the FA1.2 token contract
    /// @param from Source address (22-byte binary Tezos address)
    /// @param to Destination address (22-byte binary Tezos address)
    /// @param value Amount of tokens to transfer
    function transfer(
        string calldata tokenAddress,
        bytes22 from,
        bytes22 to,
        uint256 value
    ) external {
        // FA1.2 transfer: Pair(address :from, Pair(address :to, nat :value))
        bytes memory params = _encodePair(
            _encodeAddress(from),
            _encodePair(_encodeAddress(to), _encodeNat(value))
        );
        _callGateway(tokenAddress, "transfer", params);
    }

    /// @notice Approve a spender for FA1.2 tokens.
    /// @param tokenAddress KT1 address of the FA1.2 token contract
    /// @param spender Spender address (22-byte binary Tezos address)
    /// @param value Amount of tokens to approve
    function approve(
        string calldata tokenAddress,
        bytes22 spender,
        uint256 value
    ) external {
        // FA1.2 approve: Pair(address :spender, nat :value)
        bytes memory params = _encodePair(
            _encodeAddress(spender),
            _encodeNat(value)
        );
        _callGateway(tokenAddress, "approve", params);
    }

    function _callGateway(
        string calldata tokenAddress,
        string memory entrypoint,
        bytes memory params
    ) internal {
        (bool success, ) = RUNTIME_GATEWAY.call(
            abi.encodeWithSignature(
                "call(string,string,bytes)",
                tokenAddress,
                entrypoint,
                params
            )
        );
        require(success, "FA12Wrapper: cross-runtime call failed");
    }

    /// @dev Encode a 22-byte Tezos address as a Micheline bytes value.
    /// Micheline binary: 0x0a (bytes tag) + 4-byte BE length + data
    function _encodeAddress(
        bytes22 addr
    ) internal pure returns (bytes memory) {
        return abi.encodePacked(bytes1(0x0a), bytes4(0x00000016), addr);
    }

    /// @dev Encode a uint256 as a Micheline Int (for nat values).
    /// Micheline binary: 0x00 (int tag) + Zarith-encoded value
    function _encodeNat(
        uint256 value
    ) internal pure returns (bytes memory) {
        return abi.encodePacked(bytes1(0x00), _zarith(value));
    }

    /// @dev Encode a Micheline Pair with two arguments and no annotations.
    /// Micheline binary: 0x07 (prim-2-args-no-annots) + 0x07 (D_Pair) +
    /// left + right
    function _encodePair(
        bytes memory left,
        bytes memory right
    ) internal pure returns (bytes memory) {
        return abi.encodePacked(bytes2(0x0707), left, right);
    }

    /// @dev Zarith variable-length encoding for non-negative integers.
    /// First byte: 6 data bits + sign bit (0) + continuation bit.
    /// Subsequent bytes: 7 data bits + continuation bit.
    function _zarith(
        uint256 value
    ) internal pure returns (bytes memory) {
        if (value == 0) {
            return abi.encodePacked(bytes1(0x00));
        }

        // Count the number of output bytes needed
        uint256 temp = value;
        uint256 byteCount = 1;
        temp >>= 6;
        while (temp > 0) {
            temp >>= 7;
            byteCount++;
        }

        bytes memory result = new bytes(byteCount);

        // First byte: low 6 bits, sign=0, continuation if more
        uint8 b = uint8(value & 0x3f);
        value >>= 6;
        if (value > 0) {
            b |= 0x80;
        }
        result[0] = bytes1(b);

        for (uint256 i = 1; i < byteCount; i++) {
            b = uint8(value & 0x7f);
            value >>= 7;
            if (value > 0) {
                b |= 0x80;
            }
            result[i] = bytes1(b);
        }

        return result;
    }
}

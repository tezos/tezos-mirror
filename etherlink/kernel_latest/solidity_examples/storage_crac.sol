// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.24;

/// @title StorageCrac
/// @notice Exercises EVM storage preservation across a nested cross-runtime
///         round-trip, for both transient (Cancun's EIP-1153) and regular
///         (persistent) storage.
///
///         [start] writes a magic value to a transient slot and another to a
///         regular slot, then issues an EVM -> Michelson CRAC through the
///         gateway.  The Michelson destination is expected to call back into
///         [run] on *this same* contract (EVM -> Michelson -> EVM).  [run]
///         reads both slots and persists what it observed, so a test can assert
///         that values written before crossing into the Michelson runtime are
///         still visible when the same EVM contract is re-entered afterwards.
contract StorageCrac {
    address constant GATEWAY = 0xfF00000000000000000000000000000000000007;

    /// Slot 0: transient value observed by [run] on re-entry.  Stays 0 if the
    /// transient slot was cleared crossing into the Michelson runtime.
    uint256 public observedTransient;
    /// Slot 1: regular value observed by [run] on re-entry.  Stays 0 if the
    /// pre-hop persistent write was not visible on re-entry.
    uint256 public observedRegular;
    /// Slot 2: regular value written by [start] before the cross-runtime hop.
    uint256 public preHopRegular;

    /// Entry point.  [michelsonDestination] is the KT1 of the Michelson
    /// contract whose %run entrypoint calls back into [run] here.
    function start(string calldata michelsonDestination) external payable {
        assembly {
            // 0xdeadbeef is the transient magic value the callback must read
            // back.
            tstore(0, 0xdeadbeef)
        }
        // 0xcafebabe is the regular (persistent) magic value written before
        // the hop; the callback must read it back with SLOAD.
        preHopRegular = 0xcafebabe;
        // EVM -> Michelson: invoke the destination's %run entrypoint. Its
        // parameter is [unit], whose Micheline binary encoding is 0x030b.
        (bool ok, ) = GATEWAY.call{value: msg.value}(
            abi.encodeWithSignature(
                "callMichelson(string,string,bytes)",
                michelsonDestination,
                "run",
                hex"030b"
            )
        );
        require(ok, "EVM->Michelson CRAC failed");
    }

    /// Callback re-entered from the Michelson runtime (selector 0xc0406226).
    /// Reads the transient and regular slots written by [start] and persists
    /// what it observed.
    function run() external {
        uint256 t;
        assembly {
            t := tload(0)
        }
        observedTransient = t;
        observedRegular = preHopRegular;
    }
}

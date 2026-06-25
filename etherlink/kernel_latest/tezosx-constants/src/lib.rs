// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Cross-runtime constants shared between the EVM and Michelson runtimes.
//!
//! Each side of Tezos X has its own gas accounting unit (EVM gas vs.
//! Tezos milligas), but there is one underlying budget — the
//! per-transaction cap on the EVM side. The Michelson runtime's
//! per-operation hard cap and the EVM kernel's per-transaction cap are
//! both derived from this single value so the two sides can never
//! diverge silently:
//!
//! - the EVM side uses [`EVM_MAX_GAS_PER_TRANSACTION`] as-is;
//! - the Michelson side multiplies it by [`EVM_GAS_TO_MILLIGAS`] to get
//!   the matching cap in milligas.
//!
//! Keeping the constant in a leaf crate (no internal deps) avoids the
//! `kernel` ↔ `tezos-execution` dependency cycle that would arise if
//! the value lived in either of them.

#![cfg_attr(not(test), no_std)]

/// Maximum gas allowed for a single EVM transaction in Tezos X.
///
/// This is a network performance choice for Tezos X — not an
/// Ethereum-mainnet-derived value — picked to bound the worst-case
/// kernel work per transaction. It happens to match the value many
/// Ethereum tools default to when a per-tx limit is not configured,
/// which is convenient but incidental.
pub const EVM_MAX_GAS_PER_TRANSACTION: u64 = 30_000_000;

/// EVM gas → Michelson milligas conversion factor.
///
/// 1 Tezos gas is 45 EVM gas according to the kernel. So 1 EVM gas unit
/// corresponds to 1/45 Tezos gas = 1000/45 Tezos milligas = ~22.
/// The cross-runtime gateway uses this when forwarding `remaining_evm_gas` as
/// `X-Tezos-Gas-Limit`, and the Michelson per-operation hard cap is
/// derived from [`EVM_MAX_GAS_PER_TRANSACTION`] via this factor so the
/// two sides agree on the budget envelope.
pub const EVM_GAS_TO_MILLIGAS: u64 = 22;

/// Maximum gas (in milligas) for a single Michelson operation in
/// Tezos X.
///
/// Derived from [`EVM_MAX_GAS_PER_TRANSACTION`] via [`EVM_GAS_TO_MILLIGAS`]
/// so a cross-runtime call originated on the EVM side can propagate its
/// full remaining gas budget through `X-Tezos-Gas-Limit` without the
/// Michelson runtime rejecting it for exceeding its per-op cap.
///
/// 30_000_000 × 22 = 660_000_000 milligas. The Tezlink parametric
/// constants override `hard_gas_limit_per_operation` to the gas-unit
/// equivalent (3_000_000) — see
/// `etherlink/bin_node/lib_dev/tezlink/tezlink_constants.ml`.
pub const MICHELSON_MAX_MILLIGAS_PER_OPERATION: u64 =
    EVM_MAX_GAS_PER_TRANSACTION * EVM_GAS_TO_MILLIGAS;

// ── Cross-runtime gateway base costs (EVM gas) ───────────────────────────
//
// Canonical, single source of truth for the cross-runtime (CRAC) gateway
// surcharges, expressed in EVM gas. Both sides derive from these so they
// cannot desync: the EVM precompile charges them directly (re-exported in
// `revm::precompiles::constants`), while the Michelson runtime converts
// them to milligas via [`EVM_GAS_TO_MILLIGAS`]. Bump a value here and both
// runtimes follow.

/// Per 32-byte-word surcharge applied by the gateway to calldata, outgoing
/// body, and incoming response. Matches `G_copy` (3/word), the rate REVM
/// uses for CALLDATACOPY/RETURNDATACOPY — charged inside the precompile
/// because REVM does not meter the per-byte work of custom precompiles.
pub const RUNTIME_GATEWAY_PER_WORD_COST: u64 = 3;

/// Surcharge when a cross-runtime call carries value (`msg.value > 0`):
/// the precompile's balance burn after the transfer, equivalent to an
/// SSTORE non-zero→zero (EIP-2929 + YP).
pub const VALUE_TRANSFER_SURCHARGE: u64 = 5_000;

/// Per-hop cost of deriving an alias string (BLAKE2b-160 + base58check or
/// hex encoding). Conservative against the actual hashing + encoding work.
pub const DERIVE_ALIAS_STRING_COST: u64 = 1_500;

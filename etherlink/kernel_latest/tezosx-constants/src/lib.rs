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
/// 1 EVM gas unit corresponds to 100 Tezos milligas. The cross-runtime
/// gateway uses this when forwarding `remaining_evm_gas` as
/// `X-Tezos-Gas-Limit`, and the Michelson per-operation hard cap is
/// derived from [`EVM_MAX_GAS_PER_TRANSACTION`] via this factor so the
/// two sides agree on the budget envelope.
pub const EVM_GAS_TO_MILLIGAS: u64 = 100;

/// Maximum gas (in milligas) for a single Michelson operation in
/// Tezos X.
///
/// Derived from [`EVM_MAX_GAS_PER_TRANSACTION`] via [`EVM_GAS_TO_MILLIGAS`]
/// so a cross-runtime call originated on the EVM side can propagate its
/// full remaining gas budget through `X-Tezos-Gas-Limit` without the
/// Michelson runtime rejecting it for exceeding its per-op cap.
///
/// 30_000_000 × 100 = 3_000_000_000 milligas. The Tezlink parametric
/// constants override `hard_gas_limit_per_operation` to the gas-unit
/// equivalent (3_000_000) — see
/// `etherlink/bin_node/lib_dev/tezlink/tezlink_constants.ml`.
pub const MICHELSON_MAX_MILLIGAS_PER_OPERATION: u64 =
    EVM_MAX_GAS_PER_TRANSACTION * EVM_GAS_TO_MILLIGAS;

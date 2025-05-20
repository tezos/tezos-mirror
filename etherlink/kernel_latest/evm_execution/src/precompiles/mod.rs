// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Precompiles for the EVM
//!
//! This module defines the set of precompiled function for the
//! EVM interpreter to use instead of calling contracts.
//! Unfortunately, we cannot use the standard `PrecompileSet`
//! provided by SputnikVM, as we require the Host type and object
//! for writing to the log.

use std::vec;

mod blake2;
mod ecdsa;
mod fa_bridge;
mod hash;
mod identity;
mod modexp;
pub(crate) mod reentrancy_guard;
mod revert;
mod withdrawal;
mod zero_knowledge;

use crate::handler::{EvmHandler, Withdrawal};
use crate::EthereumError;
use alloc::collections::btree_map::BTreeMap;
use blake2::blake2f_precompile;
use ecdsa::ecrecover_precompile;
use evm::executor::stack::PrecompileFailure;
use evm::{Context, ExitReason, Handler, Transfer};
use fa_bridge::fa_bridge_precompile;
use hash::{ripemd160_precompile, sha256_precompile};
use identity::identity_precompile;
use modexp::modexp_precompile;
use primitive_types::H160;
use revert::revert_precompile;
use tezos_evm_runtime::runtime::Runtime;
use withdrawal::withdrawal_precompile;
use zero_knowledge::{ecadd_precompile, ecmul_precompile, ecpairing_precompile};

/// FA bridge precompile address
pub const FA_BRIDGE_PRECOMPILE_ADDRESS: H160 = H160([
    0xff, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2,
]);

// System (zero) account address, owns ticket table and withdrawal counter
pub const SYSTEM_ACCOUNT_ADDRESS: H160 = H160::zero();

/// Outcome of executing a precompiled contract. Covers both successful
/// return, stop and revert and additionally, it covers contract execution
/// failures (malformed input etc.). This is encoded using the `ExitReason`
/// same as with normal contract calls.
#[derive(PartialEq, Debug)]
pub struct PrecompileOutcome {
    /// Status after execution. This has the same semantics as with normal
    /// contract calls.
    pub exit_status: ExitReason,
    /// The return value of the call.
    pub output: Vec<u8>,
    /// Any withdrawals produced by the precompiled contract. This encodes
    /// withdrawals to Tezos Layer 1.
    pub withdrawals: Vec<Withdrawal>,
    /// Number of ticks estimated by the tick model of the precompiled contract.
    /// Note that the implementation of the contract is responsible for failing
    /// with EthereumError::OutOfTicks if the number of tricks would make the
    /// total number of ticks of the Handler go over the allocated number of
    /// ticks.
    pub estimated_ticks: u64,
}

/// Type for a single precompiled contract
pub type PrecompileFn<Host> = fn(
    _: &mut EvmHandler<Host>,
    _: &[u8],
    _: &Context,
    _: bool,
    _: Option<Transfer>,
) -> Result<PrecompileOutcome, EthereumError>;

/// Trait for encapsulating all precompiles
///
/// This is adapted from SputnikVM trait with same name. It has been
/// modified to take the Host into account, so that precompiles can
/// interact with log and durable storage and the rest of the kernel.
pub trait PrecompileSet<Host: Runtime> {
    /// Execute a single contract call to a precompiled contract. Should
    /// return None (and have no effect), if there is no precompiled contract
    /// at the address given.
    #[allow(clippy::too_many_arguments)]
    fn execute(
        &self,
        handler: &mut EvmHandler<Host>,
        address: H160,
        input: &[u8],
        context: &Context,
        is_static: bool,
        transfer: Option<Transfer>,
    ) -> Option<Result<PrecompileOutcome, EthereumError>>;

    /// Check if there is a precompiled contract at the given address.
    fn is_precompile(&self, address: H160) -> bool;
}

/// One implementation for PrecompileSet above. Adapted from SputnikVM.
pub type PrecompileBTreeMap<Host> = BTreeMap<H160, PrecompileFn<Host>>;

impl<Host: Runtime> PrecompileSet<Host> for PrecompileBTreeMap<Host> {
    fn execute(
        &self,
        handler: &mut EvmHandler<Host>,
        address: H160,
        input: &[u8],
        context: &Context,
        is_static: bool,
        transfer: Option<Transfer>,
    ) -> Option<Result<PrecompileOutcome, EthereumError>>
    where
        Host: Runtime,
    {
        self.get(&address)
            .map(|precompile| (*precompile)(handler, input, context, is_static, transfer))
    }

    /// Check if the given address is a precompile. Should only be called to
    /// perform the check while not executing the precompile afterward, since
    /// `execute` already performs a check internally.
    fn is_precompile(&self, address: H160) -> bool {
        self.contains_key(&address)
    }
}

type PrecompileWithoutGasDrainFn<Host> =
    fn(_: &mut EvmHandler<Host>, _: &[u8]) -> Result<PrecompileOutcome, EthereumError>;

pub fn call_precompile_with_gas_draining<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
    precompile_contract_without_gas: PrecompileWithoutGasDrainFn<Host>,
) -> Result<PrecompileOutcome, EthereumError> {
    match precompile_contract_without_gas(handler, input) {
        Ok(precompile_outcome) => Ok(precompile_outcome),
        Err(err) => {
            if let Err(record_err) = handler.record_cost(handler.gas_left().as_u64()) {
                Ok(PrecompileOutcome {
                    exit_status: ExitReason::Error(record_err),
                    output: vec![],
                    withdrawals: vec![],
                    estimated_ticks: 0,
                })
            } else {
                Err(err)
            }
        }
    }
}

// Prefixed by 'ff' to make sure we will not conflict with any
// upcoming Ethereum upgrades.
pub const WITHDRAWAL_ADDRESS: H160 = H160([
    0xff, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
]);

// This precompile is part of EIP-4844 which we don't support
// on Etherlink, as they are related to blobs.
// See: https://eips.ethereum.org/EIPS/eip-4844#point-evaluation-precompile
fn kzg_point_evaluation<Host: Runtime>(
    _handler: &mut EvmHandler<Host>,
    _input: &[u8],
    _context: &Context,
    _is_static: bool,
    _transfer: Option<Transfer>,
) -> Result<PrecompileOutcome, EthereumError> {
    Err(EthereumError::PrecompileFailed(PrecompileFailure::Fatal {
        exit_status: evm::ExitFatal::NotSupported,
    }))
}

pub fn evm_precompile_set<Host: Runtime>() -> PrecompileBTreeMap<Host> {
    BTreeMap::from([
        (
            H160::from_low_u64_be(1u64),
            ecrecover_precompile as PrecompileFn<Host>,
        ),
        (
            H160::from_low_u64_be(2u64),
            sha256_precompile as PrecompileFn<Host>,
        ),
        (
            H160::from_low_u64_be(3u64),
            ripemd160_precompile as PrecompileFn<Host>,
        ),
        (
            H160::from_low_u64_be(4u64),
            identity_precompile as PrecompileFn<Host>,
        ),
        (
            H160::from_low_u64_be(5u64),
            modexp_precompile as PrecompileFn<Host>,
        ),
        (
            H160::from_low_u64_be(6u64),
            ecadd_precompile as PrecompileFn<Host>,
        ),
        (
            H160::from_low_u64_be(7u64),
            ecmul_precompile as PrecompileFn<Host>,
        ),
        (
            H160::from_low_u64_be(8u64),
            ecpairing_precompile as PrecompileFn<Host>,
        ),
        (
            H160::from_low_u64_be(9u64),
            blake2f_precompile as PrecompileFn<Host>,
        ),
        (
            H160::from_low_u64_be(10u64),
            kzg_point_evaluation as PrecompileFn<Host>,
        ),
    ])
}

/// Factory function for generating the precompileset that the EVM kernel uses.
pub fn precompile_set<Host: Runtime>(
    enable_fa_withdrawals: bool,
) -> PrecompileBTreeMap<Host> {
    let mut precompiles = evm_precompile_set();

    precompiles.insert(
        WITHDRAWAL_ADDRESS,
        withdrawal_precompile as PrecompileFn<Host>,
    );

    if enable_fa_withdrawals {
        precompiles.insert(
            FA_BRIDGE_PRECOMPILE_ADDRESS,
            fa_bridge_precompile as PrecompileFn<Host>,
        );
    }
    precompiles
}

pub fn precompile_set_with_revert_withdrawals<Host: Runtime>(
    enable_fa_withdrawals: bool,
) -> PrecompileBTreeMap<Host> {
    let mut precompiles = evm_precompile_set();

    precompiles.insert(WITHDRAWAL_ADDRESS, revert_precompile as PrecompileFn<Host>);

    if enable_fa_withdrawals {
        precompiles.insert(
            FA_BRIDGE_PRECOMPILE_ADDRESS,
            revert_precompile as PrecompileFn<Host>,
        );
    }

    precompiles
}

mod tick_model {
    pub fn ticks_of_sha256(data_size: usize) -> u64 {
        let size = data_size as u64;
        75_000 + 30_000 * (size.div_euclid(64))
    }
    pub fn ticks_of_ripemd160(data_size: usize) -> u64 {
        let size = data_size as u64;
        70_000 + 20_000 * (size.div_euclid(64))
    }
    pub fn ticks_of_identity(data_size: usize) -> u64 {
        let size = data_size as u64;
        42_000 + 35 * size
    }
    pub fn ticks_of_withdraw() -> u64 {
        880_000
    }

    pub fn ticks_of_ecrecover() -> u64 {
        30_000_000
    }

    pub fn ticks_of_blake2f(rounds: u32) -> u64 {
        1_850_000 + 3_200 * rounds as u64
    }
}

#[cfg(test)]
mod test_helpers {
    use crate::account_storage::account_path;
    use crate::account_storage::init_account_storage as init_evm_account_storage;
    use crate::account_storage::EthereumAccountStorage;
    use crate::configuration::EVMVersion;
    use crate::handler::EvmHandler;
    use crate::handler::ExecutionOutcome;
    use crate::EthereumError;
    use crate::NATIVE_TOKEN_TICKETER_PATH;
    use evm::Transfer;
    use host::runtime::Runtime;
    use primitive_types::{H160, U256};
    use tezos_ethereum::access_list::empty_access_list;
    use tezos_ethereum::block::BlockConstants;
    use tezos_ethereum::block::BlockFees;
    use tezos_evm_runtime::runtime::MockKernelHost;

    use super::precompile_set;
    pub const DUMMY_TICKETER: &str = "KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5";

    pub fn set_balance(
        host: &mut MockKernelHost,
        evm_account_storage: &mut EthereumAccountStorage,
        address: &H160,
        balance: U256,
    ) {
        let mut account = evm_account_storage
            .get_or_create(host, &account_path(address).unwrap())
            .unwrap();
        let current_balance = account.balance(host).unwrap();
        if current_balance > balance {
            account
                .balance_remove(host, current_balance - balance)
                .unwrap();
        } else {
            account
                .balance_add(host, balance - current_balance)
                .unwrap();
        }
    }

    pub fn execute_precompiled(
        address: H160,
        input: &[u8],
        transfer: Option<Transfer>,
        gas_limit: Option<u64>,
        is_static: bool,
    ) -> Result<ExecutionOutcome, EthereumError> {
        let caller = H160::from_low_u64_be(118u64);
        let mut mock_runtime = MockKernelHost::default();
        let block_fees = BlockFees::new(
            U256::from(21000),
            U256::from(21000),
            U256::from(2_000_000_000_000u64),
        );
        let block = BlockConstants::first_block(
            U256::zero(),
            U256::one(),
            block_fees,
            u64::MAX,
            H160::zero(),
        );
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let precompiles = precompile_set::<MockKernelHost>(false);
        let config = EVMVersion::current_test_config();
        let gas_price = U256::from(21000);

        set_ticketer(&mut mock_runtime, DUMMY_TICKETER);

        if let Some(Transfer { source, value, .. }) = transfer {
            set_balance(
                &mut mock_runtime,
                &mut evm_account_storage,
                &source,
                value
                    + gas_limit
                        .map(U256::from)
                        .unwrap_or_default()
                        .saturating_mul(gas_price),
            );
        }

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            caller,
            &block,
            &config,
            &precompiles,
            gas_price,
            None,
            empty_access_list(),
        );

        let value = transfer.map(|t| t.value);

        handler.call_contract(
            caller,
            address,
            value,
            input.to_vec(),
            gas_limit,
            is_static,
        )
    }

    fn set_ticketer(host: &mut MockKernelHost, address: &str) {
        host.store_write(&NATIVE_TOKEN_TICKETER_PATH, address.as_bytes(), 0)
            .unwrap();
    }
}

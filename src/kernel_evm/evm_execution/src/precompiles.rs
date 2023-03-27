// SPDX-FileCopyrightText: 2022 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Precompiles for the EVM
//!
//! This module defines the set of precompiled function for the
//! EVM interpreter to use instead of calling contracts.
//! Unfortunately, we cannot use the standard `PrecompileSet`
//! provided by SputnikVM, as we require the Host type and object
//! for writing to the log.

use crate::handler::EvmHandler;
use alloc::collections::btree_map::BTreeMap;
use debug::debug_msg;
use evm::executor::stack::{PrecompileFailure, PrecompileOutput};
use evm::Context;
use evm::ExitSucceed;
use host::runtime::Runtime;
use primitive_types::H160;
use ripemd::Ripemd160;
use sha2::{Digest, Sha256};

/// Type for a single precompiled contract
pub type PrecompileFn<Host> = fn(
    _: &mut EvmHandler<Host>,
    _: &[u8],
    _: Option<u64>,
    _: &Context,
    _: bool,
) -> Result<PrecompileOutput, PrecompileFailure>;

/// Trait for encapsulating all precompiles
///
/// This is adapted from SputnikVM trait with same name. It has been
/// modified to take the Host into account, so that precompiles can
/// interact with log and durable storage and the rest of the kernel.
pub trait PrecompileSet<Host: Runtime> {
    /// Execute a single contract call to a precompiled contract. Should
    /// return None (and have no effect), if there is no precompiled contract
    /// at the address given.
    fn execute(
        &self,
        handler: &mut EvmHandler<Host>,
        address: H160,
        input: &[u8],
        gas_limit: Option<u64>,
        context: &Context,
        is_static: bool,
    ) -> Option<Result<PrecompileOutput, PrecompileFailure>>;

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
        gas_limit: Option<u64>,
        context: &Context,
        is_static: bool,
    ) -> Option<Result<PrecompileOutput, PrecompileFailure>>
    where
        Host: Runtime,
    {
        self.get(&address).map(|precompile| {
            (*precompile)(handler, input, gas_limit, context, is_static)
        })
    }

    /// Check if the given address is a precompile. Should only be called to
    /// perform the check while not executing the precompile afterward, since
    /// `execute` already performs a check internally.
    fn is_precompile(&self, address: H160) -> bool {
        self.contains_key(&address)
    }
}

// implmenetation of 0x02 precompiled (identity)
fn identity_precompile<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
    _gas_limit: Option<u64>,
    _context: &Context,
    _is_static: bool,
) -> Result<PrecompileOutput, PrecompileFailure> {
    debug_msg!(handler.borrow_host(), "Calling identity precompile");

    Ok(PrecompileOutput {
        exit_status: ExitSucceed::Returned,
        cost: 0u64,
        output: input.to_vec(),
        logs: vec![],
    })
}

// implmenetation of 0x03 precompiled (sha256)
fn sha256_precompile<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
    _gas_limit: Option<u64>,
    _context: &Context,
    _is_static: bool,
) -> Result<PrecompileOutput, PrecompileFailure> {
    debug_msg!(handler.borrow_host(), "Calling sha2-256 precompile");

    let output = Sha256::digest(input);

    let size = input.len() as u64;
    // nearest number of words rounded up
    let data_word_size = (31 + size) / 32;
    let cost = 60 + 12 * data_word_size;

    Ok(PrecompileOutput {
        exit_status: ExitSucceed::Returned,
        cost,
        output: output.to_vec(),
        logs: vec![],
    })
}

// implmenetation of 0x04 precompiled (ripemd160)
fn ripemd160_precompile<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
    _gas_limit: Option<u64>,
    _context: &Context,
    _is_static: bool,
) -> Result<PrecompileOutput, PrecompileFailure> {
    debug_msg!(handler.borrow_host(), "Calling ripemd-160 precompile");

    let hash = Ripemd160::digest(input);
    // The 20-byte hash is returned right aligned to 32 bytes
    let mut output = [0u8; 32];
    output[12..].clone_from_slice(&hash);

    let size = input.len() as u64;
    // nearest number of words rounded up
    let data_word_size = (31 + size) / 32;
    let cost = 600 + 120 * data_word_size;

    Ok(PrecompileOutput {
        exit_status: ExitSucceed::Returned,
        cost,
        output: output.to_vec(),
        logs: vec![],
    })
}

/// Factory function for generating the precompileset that the EVM kernel uses.
pub fn precompile_set<Host: Runtime>() -> PrecompileBTreeMap<Host> {
    BTreeMap::from([
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
    ])
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::account_storage::init_account_storage as init_evm_account_storage;
    use crate::block::BlockConstants;
    use evm::Config;
    use primitive_types::{H160, U256};
    use tezos_smart_rollup_mock::MockHost;

    fn execute_precompiled(
        address: H160,
        input: &[u8],
    ) -> Option<Result<evm::executor::stack::PrecompileOutput, PrecompileFailure>> {
        let caller = H160::from_low_u64_be(118u64);
        let mut mock_runtime = MockHost::default();
        let block = BlockConstants::first_block();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let precompiles = precompile_set::<MockHost>();
        let config = Config::london();
        let gas_limit = 1000_u64;

        let mut handler = EvmHandler::new(
            &mut mock_runtime,
            &mut evm_account_storage,
            &block,
            &config,
            &precompiles,
            gas_limit,
        );
        let context = Context {
            address,
            caller,
            apparent_value: U256::zero(),
        };

        let gas_limit = Some(1_000_000_000);
        let is_static = true;

        precompiles.execute(&mut handler, address, input, gas_limit, &context, is_static)
    }

    #[test]
    fn call_sha256() {
        // act
        let input: &[u8] = &[0xFF];
        let address = H160::from_low_u64_be(2u64);
        let result = execute_precompiled(address, input);

        // assert
        let expected_hash = hex::decode(
            "a8100ae6aa1940d0b663bb31cd466142ebbdbd5187131b92d93818987832eb89",
        )
        .expect("Result should be hex string");
        let expected_cost = 72;
        let expected = PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: expected_cost,
            output: expected_hash,
            logs: vec![],
        };

        assert_eq!(Some(Ok(expected)), result);
    }

    #[test]
    fn call_ripemd() {
        // act
        let input: &[u8] = &[0xFF];
        let address = H160::from_low_u64_be(3u64);
        let result = execute_precompiled(address, input);

        // assert
        let expected_hash = hex::decode(
            "0000000000000000000000002c0c45d3ecab80fe060e5f1d7057cd2f8de5e557",
        )
        .expect("Result should be hex string");
        let expected_cost = 720;
        let expected = PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: expected_cost,
            output: expected_hash,
            logs: vec![],
        };

        assert_eq!(Some(Ok(expected)), result);
    }
}

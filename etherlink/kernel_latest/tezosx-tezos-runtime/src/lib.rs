// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use mir::ast::big_map::BigMapId;
use primitive_types::U256;
use std::collections::BTreeMap;
use tezos_crypto_rs::{
    blake2b, hash::ContractKt1Hash, hash::HashTrait, hash::OperationHash,
};
use tezos_data_encoding::{enc::BinWriter, nom::NomReader, types::Narith};
use tezos_evm_runtime::runtime::Runtime;
use tezos_execution::{
    account_storage::TezlinkAccount,
    context::Context,
    mir_ctx::{OperationCtx, TcCtx},
    OriginationNonce, TezlinkOperationGas,
};
use tezos_protocol::contract::Contract;
use tezos_smart_rollup::types::PublicKeyHash;
// `Parameters` could come from `tezos_protocol::operation`, but we also need
// `tezos_tezlink` for types that live only there (OperationHash, BlockNumber,
// TransferError). To avoid the dependency altogether, those types would need
// to be moved to a shared crate.
use tezos_tezlink::operation::Parameters;
use tezosx_interfaces::{CrossCallResult, CrossRuntimeContext, TezosXRuntimeError};

use tezos_evm_runtime::safe_storage::ETHERLINK_SAFE_STORAGE_ROOT_PATH;

use crate::{
    account::{get_tezos_account_info_or_init, narith_to_u256, set_tezos_account_info},
    context::TezosRuntimeContext,
};

pub struct TezosRuntime;

pub mod account;
pub mod context;

impl tezosx_interfaces::RuntimeInterface for TezosRuntime {
    fn generate_alias<Host: Runtime>(
        &self,
        _registry: &impl tezosx_interfaces::Registry,
        _host: &mut Host,
        native_address: &[u8],
        _context: CrossRuntimeContext,
    ) -> Result<Vec<u8>, TezosXRuntimeError> {
        // TODO: Add code in this contract.
        let contract = Contract::Originated(ContractKt1Hash::from(blake2b::digest_160(
            native_address,
        )));
        contract.to_bytes().map_err(|e| {
            TezosXRuntimeError::ConversionError(format!(
                "Failed to encode address to bytes: {e}"
            ))
        })
    }

    /// Execute a cross-runtime call where the sender's balance was already
    /// debited by the calling runtime (e.g. EVM gateway). This handles both
    /// implicit and originated destinations, including Michelson code execution
    /// and internal operations.
    fn call<Host: Runtime>(
        &self,
        registry: &impl tezosx_interfaces::Registry,
        host: &mut Host,
        _from: &[u8],
        to: &[u8],
        amount: U256,
        _data: &[u8],
        _context: CrossRuntimeContext,
    ) -> Result<CrossCallResult, TezosXRuntimeError> {
        let dest = Contract::nom_read_exact(to).map_err(|e| {
            TezosXRuntimeError::ConversionError(format!(
                "Failed to decode address from bytes: {e:?}"
            ))
        })?;
        let amount_u64: u64 = amount.try_into().map_err(|_| {
            TezosXRuntimeError::ConversionError("Amount exceeds u64::MAX".to_string())
        })?;
        let context = TezosRuntimeContext::from_root(&ETHERLINK_SAFE_STORAGE_ROOT_PATH)?;

        // FIXME: We use the Tezos null address as sender because the alias of
        // the 0x EVM source account is a KT1, and Michelson doesn't allow KT1
        // as a source. If the original call was emitted from Michelson (i.e. it
        // hit EVM before re-entering the Michelson runtime), we might want to
        // retrieve the original tz source in the future.
        let source_account = context
            .implicit_from_public_key_hash(
                &PublicKeyHash::from_b58check("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU")
                    .map_err(|e| {
                        TezosXRuntimeError::ConversionError(format!(
                            "Failed to parse zero address: {e}"
                        ))
                    })?,
            )
            .map_err(|e| {
                TezosXRuntimeError::Custom(format!("Failed to fetch zero account: {e:?}"))
            })?;

        // FIXME: The following values are placeholders. A cross-runtime call
        // does not yet carry the real operation/block context. To fix, thread
        // the actual values through RuntimeInterface::call:
        //   - gas_limit (currently MAX_LIMIT)
        //   - next_temporary_id for big maps (currently 0)
        //   - block level, timestamp, chain_id (currently zeros)
        //   - operation hash, origination nonce, counter (currently zeros)

        let mut gas = TezlinkOperationGas::start(&Narith(
            // MAX_LIMIT is in milligas; `start` expects gas units, hence / 1000.
            (TezlinkOperationGas::MAX_LIMIT / 1000).into(),
        ))
        .map_err(|e| TezosXRuntimeError::Custom(format!("Failed to start gas: {e:?}")))?;
        let mut next_temp_id = BigMapId {
            value: tezos_data_encoding::types::Zarith(0.into()),
        };
        let mut tc_ctx = TcCtx {
            host,
            context: &context,
            operation_gas: &mut gas,
            big_map_diff: BTreeMap::new(),
            next_temporary_id: &mut next_temp_id,
        };
        let level = tezos_tezlink::enc_wrappers::BlockNumber { block_number: 0 };
        let now = tezos_smart_rollup::types::Timestamp::from(0i64);
        let chain_id = tezos_crypto_rs::hash::ChainId::try_from_bytes(&[0u8; 4]).unwrap();
        let mut nonce = OriginationNonce::initial(OperationHash::default());
        let mut counter = 0u128;
        let mut operation_ctx = OperationCtx {
            source: &source_account,
            origination_nonce: &mut nonce,
            counter: &mut counter,
            level: &level,
            now: &now,
            chain_id: &chain_id,
        };
        let parser = mir::parser::Parser::new();
        let parameters = Parameters::default();
        let mut internal_receipts = Vec::new();
        let amount = Narith(amount_u64.into());
        // FIXME: Currently transfer_external imposes the use of
        // operation_ctx.source as the sender when calling transfer.
        // We could/should expose the ability to provide the caller
        // (original source or last/cross-runtime caller) to be able to set
        // the KT1 alias of the calling EVM address.
        tezos_execution::transfer_external(
            &mut tc_ctx,
            &mut operation_ctx,
            registry,
            &amount,
            &dest,
            &parameters,
            &mut internal_receipts,
            &parser,
            true,
        )
        .map_err(|e| {
            TezosXRuntimeError::Custom(format!("Cross-runtime transfer failed: {e:?}"))
        })?;
        Ok(CrossCallResult::Success(vec![]))
    }

    fn address_from_string(
        &self,
        address_str: &str,
    ) -> Result<Vec<u8>, TezosXRuntimeError> {
        let contract = Contract::from_b58check(address_str).map_err(|e| {
            TezosXRuntimeError::ConversionError(format!(
                "Failed to parse address from string: {e}"
            ))
        })?;
        contract.to_bytes().map_err(|e| {
            TezosXRuntimeError::ConversionError(format!(
                "Failed to encode address to bytes: {e}"
            ))
        })
    }

    // Need to implement this only for IDE. Not needed in compilation or tests.
    #[cfg(feature = "testing")]
    fn get_balance<Host: Runtime>(
        &self,
        _host: &mut Host,
        _address: &[u8],
    ) -> Result<U256, TezosXRuntimeError> {
        unimplemented!("Use mocks if you are in tests")
    }

    // Need to implement this only for IDE. Not needed in compilation or tests.
    #[cfg(feature = "testing")]
    fn string_from_address(&self, _address: &[u8]) -> Result<String, TezosXRuntimeError> {
        unimplemented!("Use mocks if you are in tests")
    }
}

impl TezosRuntime {
    pub fn add_balance(
        host: &mut impl Runtime,
        pub_key_hash: &PublicKeyHash,
        amount: U256,
    ) -> Result<(), TezosXRuntimeError> {
        let mut info = get_tezos_account_info_or_init(host, pub_key_hash)?;
        info.balance = info
            .balance
            .checked_add(amount)
            .ok_or(TezosXRuntimeError::Custom("Balance overflow".to_string()))?;
        set_tezos_account_info(host, pub_key_hash, info)
    }

    // Used for debug while we don't have our own originated account implementation.
    pub fn get_originated_account_balance(
        host: &impl Runtime,
        kt1: &ContractKt1Hash,
    ) -> Result<U256, TezosXRuntimeError> {
        let context = TezosRuntimeContext::from_root(&ETHERLINK_SAFE_STORAGE_ROOT_PATH)?;
        let originated_account = context.originated_from_kt1(kt1)?;
        let balance = originated_account.balance(host)?;
        narith_to_u256(&balance)
    }
}

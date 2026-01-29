// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use mir::ast::{BinWriter, ByteReprTrait};
use mir::{
    ast::{Entrypoint, Micheline},
    context::CtxTrait,
};
use primitive_types::U256;
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_evm_runtime::runtime::Runtime;
use tezos_tezlink::operation_result::TransferError;
use tezosx_interfaces::{AliasCreationContext, Registry, RuntimeId};

use crate::alias::{get_alias, store_alias};

use crate::mir_ctx::HasHost;

#[derive(Debug, PartialEq)]
pub enum EnshrinedContracts {
    TezosXGateway,
}

/// prefix used to do a first quick check to eliminate most non enshrined contracts
const ENSHRINED_PREFIX: [u8; 6] = [2, 90, 121, 0, 0, 0];
// KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw
const GATEWAY_ADDRESS: &[u8] = &[
    2, 90, 121, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
];

// Should be as transparent/cheap as possible for none-native contract (the
// direct path)
pub fn from_kt1(kt1: &ContractKt1Hash) -> Option<EnshrinedContracts> {
    let bytes = kt1.to_bytes().ok()?;
    // let's escape early if possible
    if !bytes.starts_with(&ENSHRINED_PREFIX) {
        return None;
    }
    match bytes.as_slice() {
        GATEWAY_ADDRESS => Some(EnshrinedContracts::TezosXGateway),
        _ => None,
    }
}

pub fn is_enshrined(kt1: &ContractKt1Hash) -> bool {
    from_kt1(kt1).is_some()
}

fn extract_destination<'a>(value: Micheline<'a>) -> Result<String, TransferError> {
    match value {
        Micheline::String(address) => Ok(address.clone()),
        _ => Err(TransferError::MirAddressUnsupportedError),
    }
}

pub(crate) fn execute_enshrined_contract<'a, Host: Runtime>(
    contract: EnshrinedContracts,
    _entrypoint: &Entrypoint,
    value: Micheline<'a>,
    ctx: &mut (impl CtxTrait<'a> + HasHost<Host>),
    registry: &impl Registry,
) -> Result<(), TransferError> {
    match contract {
        EnshrinedContracts::TezosXGateway => {
            let dest = extract_destination(value)?;

            tezosx_transfer_tez(registry, ctx, dest.as_str())
        }
    }
}

fn bigint_to_u256(value: &num_bigint::BigInt) -> Result<U256, TransferError> {
    let (_, bytes) = value.to_bytes_le();
    if bytes.len() > 32 {
        return Err(TransferError::GatewayError(
            "Failed to convert to U256".into(),
        ));
    }
    Ok(U256::from_little_endian(&bytes))
}

fn biguint_to_u256(value: num_bigint::BigUint) -> Result<U256, TransferError> {
    let bytes = value.to_bytes_le();
    if bytes.len() > 32 {
        return Err(TransferError::GatewayError(
            "Failed to convert to U256".into(),
        ));
    }
    Ok(U256::from_little_endian(&bytes))
}

fn tezosx_transfer_tez<'a, Host: Runtime>(
    registry: &impl Registry,
    ctx: &mut (impl CtxTrait<'a> + HasHost<Host>),
    dest: &str,
) -> Result<(), TransferError> {
    let source = ctx.sender();
    let amount = ctx.amount();
    let block_number = ctx.level();
    let timestamp = ctx.now();
    let host = ctx.host();

    if amount < 0 {
        return Err(TransferError::GatewayError("Negative amount".into()));
    }
    // the sender has been debited before the execution of the contract
    let alias = match get_alias(host, &source, RuntimeId::Ethereum)? {
        Some(alias) => alias,
        None => {
            let source_bytes = source.to_bytes_vec();
            let context = AliasCreationContext {
                gas_limit: u64::MAX,
                timestamp: bigint_to_u256(&timestamp)?,
                block_number: biguint_to_u256(block_number)?,
            };
            let alias = registry
                .generate_alias(host, &source_bytes, RuntimeId::Ethereum, context)
                .map_err(|e| TransferError::GatewayError(e.to_string()))?;
            store_alias(host, &source, RuntimeId::Ethereum, &alias)?;
            alias
        }
    };
    let destination_contract = registry
        .address_from_string(dest, RuntimeId::Ethereum)
        .map_err(|e| TransferError::GatewayError(e.to_string()))?;
    let _res = registry
        .bridge(
            host,
            RuntimeId::Ethereum,
            &destination_contract,
            &alias,
            U256::from(amount as u64),
            &[0u8; 0],
        )
        .map_err(|e| TransferError::GatewayError(e.to_string()))?;
    Ok(())
}

pub(crate) fn get_enshrined_contract_entrypoint(
    _contract: EnshrinedContracts,
) -> Option<std::collections::HashMap<Entrypoint, mir::ast::Type>> {
    // TODO L2-819
    None
}

#[cfg(test)]
mod tests {
    use tezos_crypto_rs::hash::{ContractKt1Hash, HashTrait};

    use crate::enshrined_contracts::*;

    const GATEWAY_KT1: &str = "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw";

    #[test]
    fn test_gateway() {
        let contract = ContractKt1Hash::try_from_bytes(GATEWAY_ADDRESS).unwrap();
        assert![is_enshrined(&contract)];
        assert![contract.to_base58_check().as_str() == GATEWAY_KT1];
        assert![from_kt1(&contract) == Some(EnshrinedContracts::TezosXGateway)];
    }
}

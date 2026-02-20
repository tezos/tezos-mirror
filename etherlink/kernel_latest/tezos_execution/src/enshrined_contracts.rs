// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use mir::ast::{BinWriter, ByteReprTrait};
use mir::lexer::Prim;
use mir::{
    ast::{Entrypoint, Micheline},
    context::CtxTrait,
};
use primitive_types::U256;
use sha3::{Digest, Keccak256};
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_evm_runtime::runtime::Runtime;
use tezos_tezlink::operation_result::TransferError;
use tezosx_interfaces::{CrossCallResult, CrossRuntimeContext, Registry, RuntimeId};

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
    entrypoint: &Entrypoint,
    value: Micheline<'a>,
    ctx: &mut (impl CtxTrait<'a> + HasHost<Host>),
    registry: &impl Registry,
) -> Result<(), TransferError> {
    match contract {
        EnshrinedContracts::TezosXGateway => {
            if entrypoint.is_default() {
                let dest = extract_destination(value)?;
                tezosx_cross_runtime_call(registry, ctx, dest.as_str(), &[])
            } else if entrypoint.as_str() == "call" {
                let (dest, method_sig, abi_params) = extract_call_params(value)?;
                let selector = compute_selector(&method_sig);
                let mut calldata = Vec::with_capacity(4 + abi_params.len());
                calldata.extend_from_slice(&selector);
                calldata.extend_from_slice(&abi_params);
                tezosx_cross_runtime_call(registry, ctx, &dest, &calldata)
            } else {
                Err(TransferError::GatewayError(format!(
                    "Unknown entrypoint: {entrypoint}"
                )))
            }
        }
    }
}

/// Extract (destination, method_signature, abi_parameters) from a Micheline
/// Pair(String(destination), Pair(String(method_sig), Bytes(abi_params))).
fn extract_call_params(
    value: Micheline<'_>,
) -> Result<(String, String, Vec<u8>), TransferError> {
    match value {
        Micheline::App(Prim::Pair, args, _) if args.len() == 2 => {
            let dest = match &args[0] {
                Micheline::String(s) => s.clone(),
                _ => {
                    return Err(TransferError::GatewayError(
                        "Expected string destination in call parameters".into(),
                    ))
                }
            };
            match &args[1] {
                Micheline::App(Prim::Pair, inner_args, _) if inner_args.len() == 2 => {
                    let method_sig = match &inner_args[0] {
                        Micheline::String(s) => s.clone(),
                        _ => {
                            return Err(TransferError::GatewayError(
                                "Expected string method signature".into(),
                            ))
                        }
                    };
                    let abi_params = match &inner_args[1] {
                        Micheline::Bytes(b) => b.clone(),
                        _ => {
                            return Err(TransferError::GatewayError(
                                "Expected bytes for ABI parameters".into(),
                            ))
                        }
                    };
                    Ok((dest, method_sig, abi_params))
                }
                _ => Err(TransferError::GatewayError(
                    "Expected Pair(string, bytes) for method signature and parameters"
                        .into(),
                )),
            }
        }
        _ => Err(TransferError::GatewayError(
            "Expected Pair(string, Pair(string, bytes)) for call parameters".into(),
        )),
    }
}

/// Compute the 4-byte Keccak256 function selector from a method signature.
fn compute_selector(method_signature: &str) -> [u8; 4] {
    let hash = Keccak256::digest(method_signature.as_bytes());
    [hash[0], hash[1], hash[2], hash[3]]
}

fn tezosx_cross_runtime_call<'a, Host: Runtime>(
    registry: &impl Registry,
    ctx: &mut (impl CtxTrait<'a> + HasHost<Host>),
    dest: &str,
    data: &[u8],
) -> Result<(), TransferError> {
    let source = ctx.sender();
    let amount = ctx.amount();
    let block_number = ctx.level();
    let timestamp = ctx.now();
    let host = ctx.host();

    if amount < 0 {
        return Err(TransferError::GatewayError("Negative amount".into()));
    }

    let context = CrossRuntimeContext {
        gas_limit: u64::MAX,
        timestamp: bigint_to_u256(&timestamp)?,
        block_number: biguint_to_u256(block_number)?,
    };

    let alias = match get_alias(host, &source, RuntimeId::Ethereum)? {
        Some(alias) => alias,
        None => {
            let source_b58 = source.to_base58_check();
            let alias = registry
                .generate_alias(
                    host,
                    source_b58.as_bytes(),
                    RuntimeId::Ethereum,
                    context.clone(),
                )
                .map_err(|e| TransferError::GatewayError(e.to_string()))?;
            store_alias(host, &source, RuntimeId::Ethereum, &alias)?;
            alias
        }
    };
    let destination_contract = registry
        .address_from_string(dest, RuntimeId::Ethereum)
        .map_err(|e| TransferError::GatewayError(e.to_string()))?;

    let result = registry
        .bridge(
            host,
            RuntimeId::Ethereum,
            &destination_contract,
            &alias,
            U256::from(amount as u64),
            data,
            context,
        )
        .map_err(|e| TransferError::GatewayError(e.to_string()))?;
    match result {
        CrossCallResult::Success(_) => Ok(()),
        CrossCallResult::Revert(data) => Err(TransferError::GatewayError(format!(
            "Cross-runtime call reverted: {}",
            hex::encode(&data)
        ))),
        CrossCallResult::Halt(data) => Err(TransferError::GatewayError(format!(
            "Cross-runtime call halted: {}",
            hex::encode(&data)
        ))),
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

pub(crate) fn get_enshrined_contract_entrypoint(
    _contract: EnshrinedContracts,
) -> Option<std::collections::HashMap<Entrypoint, mir::ast::Type>> {
    // TODO L2-819
    None
}

#[cfg(test)]
mod tests {
    use mir::ast::AddressHash;
    use primitive_types::U256;
    use tezos_crypto_rs::hash::{ContractKt1Hash, HashTrait};
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezosx_interfaces::RuntimeId;

    use crate::enshrined_contracts::*;
    use crate::mir_ctx::mock::MockCtx;
    use crate::test_utils::MockRegistry;

    const GATEWAY_KT1: &str = "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw";

    #[test]
    fn test_gateway() {
        let contract = ContractKt1Hash::try_from_bytes(GATEWAY_ADDRESS).unwrap();
        assert![is_enshrined(&contract)];
        assert![contract.to_base58_check().as_str() == GATEWAY_KT1];
        assert![from_kt1(&contract) == Some(EnshrinedContracts::TezosXGateway)];
    }

    #[test]
    fn test_compute_selector() {
        // keccak256("transfer(address,uint256)") starts with 0xa9059cbb
        let selector = compute_selector("transfer(address,uint256)");
        assert_eq!(selector, [0xa9, 0x05, 0x9c, 0xbb]);
    }

    #[test]
    fn test_tezosx_cross_runtime_call_passes_calldata() {
        let mut host = MockKernelHost::default();
        let generated_alias = vec![0x01, 0x02, 0x03, 0x04];
        let registry = MockRegistry::new(generated_alias.clone());

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";
        let method_sig = "transfer(address,uint256)";
        let abi_params = vec![0xAA, 0xBB, 0xCC, 0xDD];
        let amount = 500i64;

        let selector = compute_selector(method_sig);
        let mut calldata = Vec::with_capacity(4 + abi_params.len());
        calldata.extend_from_slice(&selector);
        calldata.extend_from_slice(&abi_params);

        let mut ctx = MockCtx::new(&mut host, source, amount);
        let result = tezosx_cross_runtime_call(&registry, &mut ctx, dest, &calldata);
        assert!(result.is_ok());

        let bridge_calls = registry.bridge_calls.borrow();
        assert_eq!(bridge_calls.len(), 1);
        assert_eq!(bridge_calls[0].0, RuntimeId::Ethereum);
        assert_eq!(bridge_calls[0].4, calldata);
    }

    #[test]
    fn test_tezosx_transfer_creates_alias_when_absent() {
        let mut host = MockKernelHost::default();
        let generated_alias = vec![0x01, 0x02, 0x03, 0x04];
        let registry = MockRegistry::new(generated_alias.clone());

        // tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb as AddressHash
        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";
        let amount = 1000i64;

        let mut ctx = MockCtx::new(&mut host, source, amount);
        let result = tezosx_cross_runtime_call(&registry, &mut ctx, dest, &[]);
        assert!(result.is_ok());

        // Verify generate_alias was called
        let alias_calls = registry.generate_alias_calls.borrow();
        assert_eq!(alias_calls.len(), 1);
        assert_eq!(alias_calls[0].1, RuntimeId::Ethereum);

        // Verify bridge was called with correct parameters
        let bridge_calls = registry.bridge_calls.borrow();
        assert_eq!(bridge_calls.len(), 1);
        assert_eq!(bridge_calls[0].0, RuntimeId::Ethereum);
        assert_eq!(bridge_calls[0].1, dest.as_bytes().to_vec());
        assert_eq!(bridge_calls[0].2, generated_alias);
        assert_eq!(bridge_calls[0].3, U256::from(1000));
    }

    #[test]
    fn test_tezosx_transfer_reuses_existing_alias() {
        let mut host = MockKernelHost::default();
        let generated_alias = vec![0x01, 0x02, 0x03, 0x04];
        let registry = MockRegistry::new(generated_alias.clone());

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";
        let amount = 1000i64;

        let mut ctx = MockCtx::new(&mut host, source, amount);

        // First transfer creates alias
        let result1 = tezosx_cross_runtime_call(&registry, &mut ctx, dest, &[]);
        assert!(result1.is_ok());

        // Second transfer should reuse alias
        let result2 = tezosx_cross_runtime_call(&registry, &mut ctx, dest, &[]);
        assert!(result2.is_ok());

        // generate_alias should only have been called once
        let alias_calls = registry.generate_alias_calls.borrow();
        assert_eq!(alias_calls.len(), 1);

        // bridge should have been called twice
        let bridge_calls = registry.bridge_calls.borrow();
        assert_eq!(bridge_calls.len(), 2);
    }
}

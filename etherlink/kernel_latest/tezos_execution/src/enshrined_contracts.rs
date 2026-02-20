// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use mir::ast::Type;
use mir::ast::{BinWriter, ByteReprTrait};
use mir::lexer::Prim;
use mir::{
    ast::{Entrypoint, Micheline},
    context::CtxTrait,
};
use num_bigint::BigInt;
use primitive_types::U256;
use sha3::{Digest, Keccak256};
use std::collections::HashMap;
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_evm_runtime::runtime::Runtime;
use tezos_tezlink::operation_result::TransferError;
use tezosx_interfaces::{CrossCallResult, CrossRuntimeContext, Registry, RuntimeId};

use crate::alias::{get_alias, store_alias};

use crate::account_storage::TezlinkAccount;
use crate::mir_ctx::{HasContractAccount, HasHost};

#[derive(Debug, PartialEq)]
pub enum EnshrinedContracts {
    TezosXGateway,
    ERC20Wrapper,
}

/// prefix used to do a first quick check to eliminate most non enshrined contracts
const ENSHRINED_PREFIX: [u8; 6] = [2, 90, 121, 0, 0, 0];
// KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw
const GATEWAY_ADDRESS: &[u8] = &[
    2, 90, 121, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
];
// KT18oDJJKXMKhfE1bSuAPGp92pYcwVKvCChb
const ERC20_WRAPPER_ADDRESS: &[u8] = &[
    2, 90, 121, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2,
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
        ERC20_WRAPPER_ADDRESS => Some(EnshrinedContracts::ERC20Wrapper),
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
    ctx: &mut (impl CtxTrait<'a> + HasHost<Host> + HasContractAccount),
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
        EnshrinedContracts::ERC20Wrapper => {
            let ep = entrypoint.as_str();
            let method_sig = match ep {
                "transfer" => "transfer(address,uint256)",
                "approve" => "approve(address,uint256)",
                _ => {
                    return Err(TransferError::GatewayError(format!(
                        "Unknown ERC-20 wrapper entrypoint: {entrypoint}"
                    )))
                }
            };
            let (evm_contract, addr_bytes, amount) =
                extract_erc20_address_uint256_params(value)?;
            let calldata = abi_encode_address_uint256(method_sig, &addr_bytes, &amount)?;
            tezosx_cross_runtime_call(registry, ctx, &evm_contract, &calldata)
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

/// Extract (evm_contract, address_bytes, value) from a Micheline
/// Pair(String(evm_contract), Pair(Bytes(address), Int(value))).
fn extract_erc20_address_uint256_params(
    value: Micheline<'_>,
) -> Result<(String, Vec<u8>, BigInt), TransferError> {
    match value {
        Micheline::App(Prim::Pair, args, _) if args.len() == 2 => {
            let evm_contract = match &args[0] {
                Micheline::String(s) => s.clone(),
                _ => {
                    return Err(TransferError::GatewayError(
                        "Expected string EVM contract address".into(),
                    ))
                }
            };
            match &args[1] {
                Micheline::App(Prim::Pair, inner_args, _) if inner_args.len() == 2 => {
                    let addr_bytes = match &inner_args[0] {
                        Micheline::Bytes(b) => b.clone(),
                        _ => {
                            return Err(TransferError::GatewayError(
                                "Expected bytes for EVM address".into(),
                            ))
                        }
                    };
                    let value = match &inner_args[1] {
                        Micheline::Int(n) => n.clone(),
                        _ => {
                            return Err(TransferError::GatewayError(
                                "Expected int for token amount".into(),
                            ))
                        }
                    };
                    Ok((evm_contract, addr_bytes, value))
                }
                _ => Err(TransferError::GatewayError(
                    "Expected Pair(bytes, int) for address and amount".into(),
                )),
            }
        }
        _ => Err(TransferError::GatewayError(
            "Expected Pair(string, Pair(bytes, int)) for ERC-20 parameters".into(),
        )),
    }
}

/// ABI-encode a call to an ERC-20 function with signature `method_sig` and
/// arguments `(address, uint256)`. Returns the full calldata including the
/// 4-byte selector.
fn abi_encode_address_uint256(
    method_sig: &str,
    address_bytes: &[u8],
    value: &BigInt,
) -> Result<Vec<u8>, TransferError> {
    if address_bytes.len() > 20 {
        return Err(TransferError::GatewayError(
            "EVM address exceeds 20 bytes".into(),
        ));
    }
    let (sign, value_bytes) = value.to_bytes_be();
    if sign == num_bigint::Sign::Minus {
        return Err(TransferError::GatewayError(
            "Token amount must be non-negative".into(),
        ));
    }
    if value_bytes.len() > 32 {
        return Err(TransferError::GatewayError(
            "Token amount exceeds uint256".into(),
        ));
    }
    let selector = compute_selector(method_sig);
    let mut calldata = Vec::with_capacity(4 + 64);
    calldata.extend_from_slice(&selector);
    // ABI-encode address: left-pad to 32 bytes
    let addr_padding = 32 - address_bytes.len();
    calldata.extend_from_slice(&vec![0u8; addr_padding]);
    calldata.extend_from_slice(address_bytes);
    // ABI-encode uint256: left-pad to 32 bytes
    let val_padding = 32 - value_bytes.len();
    calldata.extend_from_slice(&vec![0u8; val_padding]);
    calldata.extend_from_slice(&value_bytes);
    Ok(calldata)
}

/// Compute the 4-byte Keccak256 function selector from a method signature.
fn compute_selector(method_signature: &str) -> [u8; 4] {
    let hash = Keccak256::digest(method_signature.as_bytes());
    [hash[0], hash[1], hash[2], hash[3]]
}

fn tezosx_cross_runtime_call<'a, Host: Runtime>(
    registry: &impl Registry,
    ctx: &mut (impl CtxTrait<'a> + HasHost<Host> + HasContractAccount),
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
        CrossCallResult::Success(_) => {
            // Debit the gateway: after a successful bridge call, the gateway
            // forwarded the funds to EVM so its balance should be reset to 0.
            let account = ctx.contract_account().clone();
            let host = ctx.host();
            account
                .set_balance(host, &0u64.into())
                .map_err(|_| TransferError::FailedToApplyBalanceChanges)?;
            Ok(())
        }
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
    contract: EnshrinedContracts,
) -> Option<HashMap<Entrypoint, Type>> {
    match contract {
        EnshrinedContracts::TezosXGateway => {
            let mut entrypoints = HashMap::new();
            // default %default: string (destination address for simple transfers)
            entrypoints.insert(Entrypoint::default(), Type::String);
            // %call: pair string (pair string bytes)
            //   (destination, (method_signature, abi_parameters))
            entrypoints.insert(
                Entrypoint::try_from("call").ok()?,
                Type::new_pair(Type::String, Type::new_pair(Type::String, Type::Bytes)),
            );
            Some(entrypoints)
        }
        EnshrinedContracts::ERC20Wrapper => {
            let mut entrypoints = HashMap::new();
            // %transfer: pair string (pair bytes int)
            //   (evm_contract, (recipient_address, amount))
            entrypoints.insert(
                Entrypoint::try_from("transfer").ok()?,
                Type::new_pair(Type::String, Type::new_pair(Type::Bytes, Type::Int)),
            );
            // %approve: pair string (pair bytes int)
            //   (evm_contract, (spender_address, amount))
            entrypoints.insert(
                Entrypoint::try_from("approve").ok()?,
                Type::new_pair(Type::String, Type::new_pair(Type::Bytes, Type::Int)),
            );
            Some(entrypoints)
        }
    }
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
    const ERC20_WRAPPER_KT1: &str = "KT18oDJJKXMKhfE1bSuAPGp92pYcwVKvCChb";

    #[test]
    fn test_gateway() {
        let contract = ContractKt1Hash::try_from_bytes(GATEWAY_ADDRESS).unwrap();
        assert![is_enshrined(&contract)];
        assert![contract.to_base58_check().as_str() == GATEWAY_KT1];
        assert![from_kt1(&contract) == Some(EnshrinedContracts::TezosXGateway)];
    }

    #[test]
    fn test_erc20_wrapper() {
        let contract = ContractKt1Hash::try_from_bytes(ERC20_WRAPPER_ADDRESS).unwrap();
        assert![is_enshrined(&contract)];
        assert![contract.to_base58_check().as_str() == ERC20_WRAPPER_KT1];
        assert![from_kt1(&contract) == Some(EnshrinedContracts::ERC20Wrapper)];
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

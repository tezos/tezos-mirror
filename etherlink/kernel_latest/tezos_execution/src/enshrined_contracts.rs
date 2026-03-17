// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use mir::ast::Type;
use mir::ast::{AddressHash, BinWriter, ByteReprTrait, TypedValue};
use mir::context::PushableTypecheckingContext;
use mir::gas::Gas;
use mir::typechecker::typecheck_value;
use mir::{
    ast::{Entrypoint, Micheline},
    context::CtxTrait,
};
use num_bigint::{BigInt, BigUint};
use num_traits::ToPrimitive;
use primitive_types::U256;
use sha3::{Digest, Keccak256};
use std::collections::HashMap;
use std::rc::Rc;
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_evm_logging::Logging;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_tezlink::operation_result::TransferError;
use tezosx_interfaces::headers::format_tez_from_mutez;
use tezosx_interfaces::{
    gas::convert as convert_gas, CrossRuntimeContext, Registry, RuntimeId,
    ERR_FORBIDDEN_TEZOS_HEADER, X_TEZOS_AMOUNT, X_TEZOS_BLOCK_NUMBER,
    X_TEZOS_GAS_CONSUMED, X_TEZOS_GAS_LIMIT, X_TEZOS_SENDER, X_TEZOS_SOURCE,
    X_TEZOS_TIMESTAMP,
};
use tezosx_journal::TezosXJournal;

use crate::alias::{get_alias, store_alias};

use crate::account_storage::TezlinkAccount;
use crate::mir_ctx::{HasContractAccount, HasHost, HasOperationGas};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EnshrinedContracts {
    TezosXGateway,
    ERC20Wrapper,
}

/// prefix used to do a first quick check to eliminate most non enshrined contracts
const ENSHRINED_PREFIX: [u8; 6] = [2, 90, 121, 0, 0, 0];
// KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw
const GATEWAY_ADDRESS: [u8; 20] = [
    2, 90, 121, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
];
// KT18oDJJKXMKhfE1bSuAPGp92pYcwVKvCChb
const ERC20_WRAPPER_ADDRESS: [u8; 20] = [
    2, 90, 121, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2,
];

// Should be as transparent/cheap as possible for none-native contract (the
// direct path)
impl EnshrinedContracts {
    /// Returns the 22-byte `AddressHash` binary encoding for this contract:
    /// `[0x01][20-byte KT1 hash][0x00]`.
    pub fn address_hash_bytes(&self) -> [u8; 22] {
        let raw: &[u8; 20] = match self {
            Self::TezosXGateway => &GATEWAY_ADDRESS,
            Self::ERC20Wrapper => &ERC20_WRAPPER_ADDRESS,
        };
        let mut bytes = [0u8; 22];
        bytes[0] = 0x01;
        bytes[1..21].copy_from_slice(raw);
        // bytes[21] stays 0x00
        bytes
    }
}

pub fn from_kt1(kt1: &ContractKt1Hash) -> Option<EnshrinedContracts> {
    let bytes = kt1.to_bytes().ok()?;
    // let's escape early if possible
    if !bytes.starts_with(&ENSHRINED_PREFIX) {
        return None;
    }
    if bytes.as_slice() == GATEWAY_ADDRESS {
        Some(EnshrinedContracts::TezosXGateway)
    } else if bytes.as_slice() == ERC20_WRAPPER_ADDRESS {
        Some(EnshrinedContracts::ERC20Wrapper)
    } else {
        None
    }
}

pub fn is_enshrined(kt1: &ContractKt1Hash) -> bool {
    from_kt1(kt1).is_some()
}

/// Unwrap an `Rc<TypedValue>`, consuming it if possible to avoid cloning.
fn unwrap_rc(rc: Rc<TypedValue<'_>>) -> TypedValue<'_> {
    Rc::try_unwrap(rc).unwrap_or_else(|rc| (*rc).clone())
}

/// Typecheck a Micheline value against the expected type for the given
/// enshrined contract entrypoint. Returns the typed value, or an error
/// if the entrypoint is unknown or the value doesn't match the type.
fn typecheck_entrypoint_value<'a>(
    contract: EnshrinedContracts,
    entrypoint: &Entrypoint,
    value: &Micheline<'a>,
) -> Result<TypedValue<'a>, TransferError> {
    let entrypoints = get_enshrined_contract_entrypoint(contract).ok_or_else(|| {
        TransferError::GatewayError("Failed to build entrypoint map".into())
    })?;
    let ty = entrypoints.get(entrypoint).ok_or_else(|| {
        TransferError::GatewayError(format!("Unknown entrypoint: {entrypoint}"))
    })?;
    let mut gas = Gas::default();
    let mut tc_ctx = PushableTypecheckingContext { gas: &mut gas };
    typecheck_value(value, &mut tc_ctx, ty)
        .map_err(|e| TransferError::GatewayError(format!("Invalid parameters: {e}")))
}

pub(crate) fn execute_enshrined_contract<'a, Host>(
    contract: EnshrinedContracts,
    entrypoint: &Entrypoint,
    value: Micheline<'a>,
    ctx: &mut (impl CtxTrait<'a> + HasHost<Host> + HasContractAccount + HasOperationGas),
    registry: &impl Registry,
    journal: &mut TezosXJournal,
) -> Result<(), TransferError>
where
    Host: StorageV1 + Logging,
{
    let typed = typecheck_entrypoint_value(contract, entrypoint, &value)?;
    match contract {
        EnshrinedContracts::TezosXGateway => {
            if entrypoint.is_default() {
                let TypedValue::String(dest) = typed else {
                    return Err(TransferError::GatewayError(
                        "Expected string for default entrypoint".into(),
                    ));
                };
                tezosx_cross_runtime_call(registry, journal, ctx, &dest, &[])
            } else if entrypoint.as_str() == "call_evm" {
                let (dest, method_sig, abi_params) = extract_call_params(typed)?;
                let selector = compute_selector(&method_sig);
                let mut calldata = Vec::with_capacity(4 + abi_params.len());
                calldata.extend_from_slice(&selector);
                calldata.extend_from_slice(&abi_params);
                tezosx_cross_runtime_call(registry, journal, ctx, &dest, &calldata)
            } else if entrypoint.as_str() == "call" {
                let mut request = extract_http_call_request(typed)?;
                let target_host = request.uri().host().map(str::to_string);
                inject_context_headers(
                    request.headers_mut(),
                    target_host.as_deref(),
                    ctx,
                    journal,
                    registry,
                )?;
                let response = registry
                    .serve(ctx.host(), journal, request)
                    .map_err(|err| TransferError::GatewayError(err.to_string()))?;
                let consumed_milligas =
                    extract_gas_consumed(&response, target_host.as_deref())?;
                ctx.operation_gas()
                    .cast_and_consume_milligas(consumed_milligas)
                    .map_err(|_| TransferError::OutOfGas)?;
                Ok(())
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
                extract_erc20_address_uint256_params(typed)?;
            let calldata = abi_encode_address_uint256(method_sig, &addr_bytes, &amount)?;
            tezosx_cross_runtime_call(registry, journal, ctx, &evm_contract, &calldata)
        }
    }
}

/// Extract (destination, method_signature, abi_parameters) from a typed
/// Pair(String, Pair(String, Bytes)) value.
fn extract_call_params(
    typed: TypedValue<'_>,
) -> Result<(String, String, Vec<u8>), TransferError> {
    let TypedValue::Pair(dest_rc, inner_rc) = typed else {
        return Err(TransferError::GatewayError(
            "call: expected pair (destination, (method_sig, abi_params))".into(),
        ));
    };
    let TypedValue::String(dest) = unwrap_rc(dest_rc) else {
        return Err(TransferError::GatewayError(
            "call: expected string for destination".into(),
        ));
    };
    let TypedValue::Pair(sig_rc, params_rc) = unwrap_rc(inner_rc) else {
        return Err(TransferError::GatewayError(
            "call: expected pair (method_sig, abi_params)".into(),
        ));
    };
    let TypedValue::String(method_sig) = unwrap_rc(sig_rc) else {
        return Err(TransferError::GatewayError(
            "call: expected string for method signature".into(),
        ));
    };
    let TypedValue::Bytes(abi_params) = unwrap_rc(params_rc) else {
        return Err(TransferError::GatewayError(
            "call: expected bytes for ABI parameters".into(),
        ));
    };
    Ok((dest, method_sig, abi_params))
}

/// Build an `http::Request<Vec<u8>>` from a typed
/// Pair(String, Pair(List(Pair(String, String)), Pair(Bytes, Nat))) value.
///
/// Method mapping: 0 = GET, 1 = POST. Other values default to POST.
fn extract_http_call_request(
    typed: TypedValue<'_>,
) -> Result<http::Request<Vec<u8>>, TransferError> {
    let TypedValue::Pair(url_rc, inner_rc) = typed else {
        return Err(TransferError::GatewayError(
            "http_call: expected pair (url, (headers, (body, method)))".into(),
        ));
    };
    let TypedValue::String(url) = unwrap_rc(url_rc) else {
        return Err(TransferError::GatewayError(
            "http_call: expected string for URL".into(),
        ));
    };
    let TypedValue::Pair(headers_rc, body_method_rc) = unwrap_rc(inner_rc) else {
        return Err(TransferError::GatewayError(
            "http_call: expected pair (headers, (body, method))".into(),
        ));
    };
    let TypedValue::List(headers_list) = unwrap_rc(headers_rc) else {
        return Err(TransferError::GatewayError(
            "http_call: expected list for headers".into(),
        ));
    };
    let headers: Vec<(String, String)> = headers_list
        .into_iter()
        .map(|item| {
            let TypedValue::Pair(name_rc, val_rc) = unwrap_rc(item) else {
                return Err(TransferError::GatewayError(
                    "http_call: expected pair (name, value) in headers list".into(),
                ));
            };
            let TypedValue::String(name) = unwrap_rc(name_rc) else {
                return Err(TransferError::GatewayError(
                    "http_call: expected string for header name".into(),
                ));
            };
            let TypedValue::String(val) = unwrap_rc(val_rc) else {
                return Err(TransferError::GatewayError(
                    "http_call: expected string for header value".into(),
                ));
            };
            Ok((name, val))
        })
        .collect::<Result<_, _>>()?;
    let TypedValue::Pair(body_rc, method_rc) = unwrap_rc(body_method_rc) else {
        return Err(TransferError::GatewayError(
            "http_call: expected pair (body, method)".into(),
        ));
    };
    let TypedValue::Bytes(body) = unwrap_rc(body_rc) else {
        return Err(TransferError::GatewayError(
            "http_call: expected bytes for body".into(),
        ));
    };
    let TypedValue::Nat(method) = unwrap_rc(method_rc) else {
        return Err(TransferError::GatewayError(
            "http_call: expected nat for method".into(),
        ));
    };
    build_http_request(&url, &headers, &body, method)
}

/// Build an `http::Request<Vec<u8>>` from extracted parameters.
fn build_http_request(
    url: &str,
    headers: &[(String, String)],
    body: &[u8],
    method: BigUint,
) -> Result<http::Request<Vec<u8>>, TransferError> {
    let http_method = match method.to_u64() {
        Some(0) => http::Method::GET,
        Some(1) => http::Method::POST,
        _ => http::Method::POST,
    };

    let mut builder = http::Request::builder().method(http_method).uri(url);
    for (name, value) in headers {
        if name.to_ascii_lowercase().starts_with("x-tezos-") {
            return Err(TransferError::GatewayError(format!(
                "{ERR_FORBIDDEN_TEZOS_HEADER}: {name}"
            )));
        }
        builder = builder.header(name.as_str(), value.as_str());
    }

    builder.body(body.to_vec()).map_err(|e| {
        TransferError::GatewayError(format!(
            "http_call: failed to build HTTP request: {e}"
        ))
    })
}

/// Inject trusted X-Tezos-* context headers into `headers`, overwriting any
/// existing values with the same name.
///
/// `amount_mutez` is in mutez (10^-6 TEZ). It is formatted as a canonical
/// TEZ decimal string in the header (see L2-969).
/// `timestamp` is a Unix timestamp in seconds (must be non-negative).
fn inject_context_headers_raw(
    headers: &mut http::HeaderMap,
    sender_alias: &[u8],
    source_alias: &[u8],
    amount_mutez: u64,
    gas_limit: u64,
    timestamp: u64,
    block_number: u32,
) -> Result<(), TransferError> {
    let parse_value = |v: &str| -> Result<http::HeaderValue, TransferError> {
        v.parse().map_err(|e| {
            TransferError::GatewayError(format!("invalid header value: {e}"))
        })
    };
    //TODO: Avoid ethereum specific formatting with https://linear.app/tezos/issue/L2-954/read-string-for-alias-on-durable-storage
    headers.insert(
        X_TEZOS_SENDER,
        parse_value(&format!("0x{}", hex::encode(sender_alias)))?,
    );
    headers.insert(
        X_TEZOS_SOURCE,
        parse_value(&format!("0x{}", hex::encode(source_alias)))?,
    );
    headers.insert(
        X_TEZOS_AMOUNT,
        parse_value(&format_tez_from_mutez(amount_mutez))?,
    );
    headers.insert(X_TEZOS_GAS_LIMIT, parse_value(&format!("{gas_limit}"))?);
    headers.insert(X_TEZOS_TIMESTAMP, parse_value(&format!("{timestamp}"))?);
    headers.insert(
        X_TEZOS_BLOCK_NUMBER,
        parse_value(&format!("{block_number}"))?,
    );
    Ok(())
}

/// Inject trusted X-Tezos-* context headers derived from `ctx` into `headers`.
///
/// `target_host` is the URL host of the destination runtime (e.g. `"ethereum"`,
/// `"tezos"`). Gas is converted from Tezos milligas to the target runtime's
/// units before being written to `X-Tezos-Gas-Limit`.
fn inject_context_headers<'a, Host>(
    headers: &mut http::HeaderMap,
    target_host: Option<&str>,
    ctx: &mut (impl CtxTrait<'a> + HasHost<Host>),
    journal: &mut TezosXJournal,
    registry: &impl Registry,
) -> Result<(), TransferError>
where
    Host: StorageV1 + Logging,
{
    let sender = ctx.sender();
    let source = AddressHash::from(ctx.source());
    let amount_mutez: u64 = ctx
        .amount()
        .try_into()
        .map_err(|_| TransferError::GatewayError("Negative amount".into()))?;
    let block_number = ctx.level();
    let timestamp = ctx.now();
    let block_number_u32 = block_number
        .to_u32()
        .ok_or_else(|| TransferError::GatewayError("Block number out of range".into()))?;
    let timestamp_u64 = timestamp
        .to_i64()
        .ok_or_else(|| TransferError::GatewayError("Timestamp out of range".into()))
        .and_then(|t| {
            u64::try_from(t)
                .map_err(|_| TransferError::GatewayError("Negative timestamp".into()))
        })?;
    let context = cross_runtime_ctx_from_ctx(ctx)?;
    let tezos_gas_limit = context.gas_limit;
    let sender_alias =
        get_or_create_alias(ctx.host(), journal, &sender, context.clone(), registry)?;
    let source_alias =
        get_or_create_alias(ctx.host(), journal, &source, context, registry)?;
    // Convert gas from Tezos milligas to the target runtime's units.
    // Unknown host is a user error (bad URL); overflow is unlikely but
    // possible for very large gas limits. Both are gateway errors.
    let target_runtime = target_host.and_then(RuntimeId::from_host).ok_or_else(|| {
        TransferError::GatewayError(
            "http_call: unknown or missing target runtime in URL host".into(),
        )
    })?;
    let gas_limit = convert_gas(RuntimeId::Tezos, target_runtime, tezos_gas_limit)
        .ok_or_else(|| {
            TransferError::GatewayError(
                "http_call: Tezos gas limit overflows target runtime units".into(),
            )
        })?;
    inject_context_headers_raw(
        headers,
        &sender_alias,
        &source_alias,
        amount_mutez,
        gas_limit,
        timestamp_u64,
        block_number_u32,
    )
}

/// Extract (evm_contract, address_bytes, value) from a typed
/// Pair(String, Pair(Bytes, Int)) value.
fn extract_erc20_address_uint256_params(
    typed: TypedValue<'_>,
) -> Result<(String, Vec<u8>, BigInt), TransferError> {
    let TypedValue::Pair(contract_rc, inner_rc) = typed else {
        return Err(TransferError::GatewayError(
            "ERC-20: expected pair (contract, (address, amount))".into(),
        ));
    };
    let TypedValue::String(evm_contract) = unwrap_rc(contract_rc) else {
        return Err(TransferError::GatewayError(
            "ERC-20: expected string for EVM contract address".into(),
        ));
    };
    let TypedValue::Pair(addr_rc, amount_rc) = unwrap_rc(inner_rc) else {
        return Err(TransferError::GatewayError(
            "ERC-20: expected pair (address, amount)".into(),
        ));
    };
    let TypedValue::Bytes(addr_bytes) = unwrap_rc(addr_rc) else {
        return Err(TransferError::GatewayError(
            "ERC-20: expected bytes for EVM address".into(),
        ));
    };
    let TypedValue::Int(amount) = unwrap_rc(amount_rc) else {
        return Err(TransferError::GatewayError(
            "ERC-20: expected int for token amount".into(),
        ));
    };
    Ok((evm_contract, addr_bytes, amount))
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

/// Look up the Ethereum alias for `address`. If none exists, generate one via
/// `registry`, persist it, and return it.
fn get_or_create_alias<Host>(
    host: &mut Host,
    journal: &mut TezosXJournal,
    address: &AddressHash,
    context: CrossRuntimeContext,
    registry: &impl Registry,
) -> Result<Vec<u8>, TransferError>
where
    Host: StorageV1 + Logging,
{
    if let Some(alias) = get_alias(host, address, RuntimeId::Ethereum)? {
        return Ok(alias);
    }
    let address_b58 = address.to_base58_check();
    let alias = registry
        .generate_alias(
            host,
            journal,
            address_b58.as_bytes(),
            RuntimeId::Ethereum,
            context,
        )
        .map_err(|e| TransferError::GatewayError(e.to_string()))?;
    store_alias(host, address, RuntimeId::Ethereum, &alias)?;
    Ok(alias)
}

/// Build a `CrossRuntimeContext` from the current execution context.
fn cross_runtime_ctx_from_ctx<'a, Host>(
    ctx: &mut (impl CtxTrait<'a> + HasHost<Host>),
) -> Result<CrossRuntimeContext, TransferError>
where
    Host: StorageV1 + Logging,
{
    // Remaining milligas is the gas budget for the cross-runtime call, in Tezos
    // milligas. `inject_context_headers` converts to the target runtime's units
    // on the way out.
    let gas_limit = ctx.gas().milligas() as u64;
    Ok(CrossRuntimeContext {
        gas_limit,
        timestamp: bigint_to_u256(&ctx.now())?,
        block_number: biguint_to_u256(ctx.level())?,
    })
}

fn tezosx_cross_runtime_call<'a, Host>(
    registry: &impl Registry,
    journal: &mut TezosXJournal,
    ctx: &mut (impl CtxTrait<'a> + HasHost<Host> + HasContractAccount),
    dest: &str,
    data: &[u8],
) -> Result<(), TransferError>
where
    Host: StorageV1 + Logging,
{
    if ctx.amount() < 0 {
        return Err(TransferError::GatewayError("Negative amount".into()));
    }

    // Build an HTTP POST request targeting the Ethereum runtime.
    let url = format!("http://ethereum/{dest}");
    let mut request = http::Request::builder()
        .method(http::Method::POST)
        .uri(&url)
        .body(data.to_vec())
        .map_err(|e| {
            TransferError::GatewayError(format!("Failed to build HTTP request: {e}"))
        })?;

    // Inject trusted X-Tezos-* context headers (sender alias, amount, etc.).
    inject_context_headers(
        request.headers_mut(),
        Some("ethereum"),
        ctx,
        journal,
        registry,
    )?;

    let response = registry
        .serve(ctx.host(), journal, request)
        .map_err(|err| TransferError::GatewayError(err.to_string()))?;

    if response.status().is_success() {
        // Debit the gateway: after a successful call, the gateway
        // forwarded the funds to EVM so its balance should be reset to 0.
        let account = ctx.contract_account().clone();
        let host = ctx.host();
        account
            .set_balance(host, &0u64.into())
            .map_err(|_| TransferError::FailedToApplyBalanceChanges)?;
        Ok(())
    } else {
        Err(TransferError::GatewayError(format!(
            "Cross-runtime call failed with status {}: {}",
            response.status(),
            String::from_utf8_lossy(response.body())
        )))
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

/// Extract the gas consumed from an HTTP response's `X-Tezos-Gas-Consumed`
/// header and convert it from the target runtime's units to Tezos milligas.
fn extract_gas_consumed(
    response: &http::Response<Vec<u8>>,
    target_host: Option<&str>,
) -> Result<u64, TransferError> {
    let target_runtime = target_host.and_then(RuntimeId::from_host).ok_or_else(|| {
        TransferError::GatewayError(
            "http_call: unknown or missing target runtime in URL host".into(),
        )
    })?;
    let consumed_in_target_units = response
        .headers()
        .get(X_TEZOS_GAS_CONSUMED)
        .and_then(|v| v.to_str().ok())
        .and_then(|s| s.parse::<u64>().ok())
        .ok_or_else(|| {
            TransferError::GatewayError(
                "http_call: missing or invalid X-Tezos-Gas-Consumed header in response"
                    .into(),
            )
        })?;
    convert_gas(target_runtime, RuntimeId::Tezos, consumed_in_target_units).ok_or_else(
        || {
            TransferError::GatewayError(
                "http_call: gas consumed overflows Tezos milligas".into(),
            )
        },
    )
}

pub(crate) fn get_enshrined_contract_entrypoint(
    contract: EnshrinedContracts,
) -> Option<HashMap<Entrypoint, Type>> {
    match contract {
        EnshrinedContracts::TezosXGateway => {
            let mut entrypoints = HashMap::new();
            // default %default: string (destination address for simple transfers)
            entrypoints.insert(Entrypoint::default(), Type::String);
            // %call_evm: pair string (pair string bytes)
            //   (destination, (method_signature, abi_parameters))
            entrypoints.insert(
                Entrypoint::try_from("call_evm").ok()?,
                Type::new_pair(Type::String, Type::new_pair(Type::String, Type::Bytes)),
            );
            // %call: pair string (pair (list (pair string string)) (pair bytes nat))
            //   (url, (headers, (body, method)))
            entrypoints.insert(
                Entrypoint::try_from("call").ok()?,
                Type::new_pair(
                    Type::String,
                    Type::new_pair(
                        Type::List(Rc::new(Type::new_pair(Type::String, Type::String))),
                        Type::new_pair(Type::Bytes, Type::Nat),
                    ),
                ),
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
    use mir::lexer::Prim;
    use num_bigint::BigInt;
    use tezos_crypto_rs::hash::{ContractKt1Hash, HashTrait};
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezosx_interfaces::RuntimeId;

    use crate::enshrined_contracts::*;
    use crate::mir_ctx::mock::MockCtx;
    use crate::test_utils::{MockRegistry, MockRegistryWithStatus};

    const GATEWAY_KT1: &str = "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw";
    const ERC20_WRAPPER_KT1: &str = "KT18oDJJKXMKhfE1bSuAPGp92pYcwVKvCChb";

    #[test]
    fn test_gateway() {
        let contract = ContractKt1Hash::try_from_bytes(&GATEWAY_ADDRESS).unwrap();
        assert![is_enshrined(&contract)];
        assert![contract.to_base58_check().as_str() == GATEWAY_KT1];
        assert![from_kt1(&contract) == Some(EnshrinedContracts::TezosXGateway)];
    }

    #[test]
    fn test_erc20_wrapper() {
        let contract = ContractKt1Hash::try_from_bytes(&ERC20_WRAPPER_ADDRESS).unwrap();
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

        let mut journal = TezosXJournal::new();
        let mut ctx = MockCtx::new(&mut host, source, amount);
        let result =
            tezosx_cross_runtime_call(&registry, &mut journal, &mut ctx, dest, &calldata);
        assert!(result.is_ok());

        let serve_calls = registry.serve_calls.borrow();
        assert_eq!(serve_calls.len(), 1);
        assert_eq!(
            serve_calls[0].uri().to_string(),
            format!("http://ethereum/{dest}")
        );
        assert_eq!(serve_calls[0].body(), &calldata);
        assert_eq!(
            serve_calls[0].headers().get(X_TEZOS_SENDER).unwrap(),
            "0x01020304"
        );
        assert_eq!(
            serve_calls[0].headers().get(X_TEZOS_SOURCE).unwrap(),
            "0x01020304"
        );
        assert_eq!(
            serve_calls[0].headers().get(X_TEZOS_AMOUNT).unwrap(),
            "0.0005"
        );
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
        let amount = 1000u64;

        let mut journal = TezosXJournal::new();
        let mut ctx = MockCtx::new(&mut host, source, amount as i64);
        let result =
            tezosx_cross_runtime_call(&registry, &mut journal, &mut ctx, dest, &[]);
        assert!(result.is_ok());

        // Verify generate_alias was called for both sender and source
        let alias_calls = registry.generate_alias_calls.borrow();
        assert_eq!(alias_calls.len(), 2);
        assert_eq!(alias_calls[0].1, RuntimeId::Ethereum);
        assert_eq!(alias_calls[1].1, RuntimeId::Ethereum);

        // Verify serve was called with correct URL and headers
        let serve_calls = registry.serve_calls.borrow();
        assert_eq!(serve_calls.len(), 1);
        assert_eq!(
            serve_calls[0].uri().to_string(),
            format!("http://ethereum/{dest}")
        );
        assert_eq!(serve_calls[0].method(), http::Method::POST);
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

        let mut journal = TezosXJournal::new();
        let mut ctx = MockCtx::new(&mut host, source, amount);

        // First transfer creates aliases (sender + source)
        let result1 =
            tezosx_cross_runtime_call(&registry, &mut journal, &mut ctx, dest, &[]);
        assert!(result1.is_ok());

        // Second transfer should reuse aliases
        let result2 =
            tezosx_cross_runtime_call(&registry, &mut journal, &mut ctx, dest, &[]);
        assert!(result2.is_ok());

        // generate_alias should have been called twice (sender + source on first call)
        let alias_calls = registry.generate_alias_calls.borrow();
        assert_eq!(alias_calls.len(), 2);

        // serve should have been called twice
        let serve_calls = registry.serve_calls.borrow();
        assert_eq!(serve_calls.len(), 2);
    }

    /// Helper to build a Micheline call value:
    /// Pair(String(url), Pair(Seq(headers), Pair(Bytes(body), Int(method))))
    fn build_http_call_micheline<'a>(
        arena: &'a typed_arena::Arena<Micheline<'a>>,
        url: &str,
        headers: &[(&str, &str)],
        body: &[u8],
        method: i64,
    ) -> Micheline<'a> {
        let header_pairs: Vec<Micheline<'a>> = headers
            .iter()
            .map(|(name, value)| {
                Micheline::prim2(
                    arena,
                    Prim::Pair,
                    name.to_string().into(),
                    value.to_string().into(),
                )
            })
            .collect();
        let headers_seq = Micheline::Seq(arena.alloc_extend(header_pairs));
        let body_method = Micheline::prim2(
            arena,
            Prim::Pair,
            body.to_vec().into(),
            num_bigint::BigInt::from(method).into(),
        );
        let inner_pair = Micheline::prim2(arena, Prim::Pair, headers_seq, body_method);
        Micheline::prim2(arena, Prim::Pair, url.to_string().into(), inner_pair)
    }

    /// Typecheck a Micheline call value into a TypedValue.
    fn typecheck_call<'a>(
        value: &Micheline<'a>,
    ) -> Result<TypedValue<'a>, TransferError> {
        typecheck_entrypoint_value(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("call").unwrap(),
            value,
        )
    }

    #[test]
    fn test_http_call_parses_valid_parameters() {
        let arena = typed_arena::Arena::new();
        let value = build_http_call_micheline(
            &arena,
            "http://michelson/KT1abc/transfer",
            &[("Content-Type", "application/micheline")],
            &[0x01, 0x02],
            1,
        );
        let typed = typecheck_call(&value).unwrap();
        let request = extract_http_call_request(typed).unwrap();
        assert_eq!(request.uri(), "http://michelson/KT1abc/transfer");
        assert_eq!(request.method(), http::Method::POST);
        assert_eq!(
            request.headers().get("Content-Type").unwrap(),
            "application/micheline"
        );
        assert_eq!(request.body(), &vec![0x01, 0x02]);
    }

    #[test]
    fn test_http_call_empty_headers() {
        let arena = typed_arena::Arena::new();
        let value =
            build_http_call_micheline(&arena, "http://michelson/KT1abc", &[], &[], 0);
        let typed = typecheck_call(&value).unwrap();
        let request = extract_http_call_request(typed).unwrap();
        assert_eq!(request.uri(), "http://michelson/KT1abc");
        assert_eq!(request.method(), http::Method::GET);
        assert!(request.headers().is_empty());
        assert!(request.body().is_empty());
    }

    #[test]
    fn test_http_call_multiple_headers() {
        let arena = typed_arena::Arena::new();
        let value = build_http_call_micheline(
            &arena,
            "http://michelson/KT1abc",
            &[
                ("Content-Type", "application/micheline"),
                ("X-Custom", "some-value"),
            ],
            &[0xDE, 0xAD],
            42,
        );
        let typed = typecheck_call(&value).unwrap();
        let request = extract_http_call_request(typed).unwrap();
        // Unknown method defaults to POST
        assert_eq!(request.method(), http::Method::POST);
        assert_eq!(
            request.headers().get("Content-Type").unwrap(),
            "application/micheline"
        );
        assert_eq!(request.headers().get("X-Custom").unwrap(), "some-value");
        assert_eq!(request.body(), &vec![0xDE, 0xAD]);
    }

    #[test]
    fn test_http_call_malformed_parameters() {
        let result =
            typecheck_call(&Micheline::String("not a valid http_call".to_string()));
        assert!(result.is_err());
    }

    #[test]
    fn test_inject_context_headers_raw_sets_values() {
        let mut headers = http::HeaderMap::new();
        inject_context_headers_raw(
            &mut headers,
            b"sender_alias",
            b"source_alias",
            42u64, // 42 mutez = 0.000042 TEZ
            1000,
            1700000000u64,
            5,
        )
        .unwrap();
        assert_eq!(
            headers.get("X-Tezos-Sender").unwrap(),
            "0x73656e6465725f616c696173"
        );
        assert_eq!(
            headers.get("X-Tezos-Source").unwrap(),
            "0x736f757263655f616c696173"
        );
        assert_eq!(headers.get("X-Tezos-Amount").unwrap(), "0.000042");
        assert_eq!(headers.get("X-Tezos-Gas-Limit").unwrap(), "1000");
        assert_eq!(headers.get("X-Tezos-Timestamp").unwrap(), "1700000000");
        assert_eq!(headers.get("X-Tezos-Block-Number").unwrap(), "5");
    }

    #[test]
    fn test_inject_context_headers_raw_overwrites_existing() {
        let mut headers = http::HeaderMap::new();
        headers.insert(
            http::header::HeaderName::from_static("x-tezos-sender"),
            "old-value".parse().unwrap(),
        );
        inject_context_headers_raw(
            &mut headers,
            b"new_alias",
            b"source_alias",
            0u64,
            0,
            0u64,
            0,
        )
        .unwrap();
        assert_eq!(
            headers.get("X-Tezos-Sender").unwrap(),
            "0x6e65775f616c696173"
        );
    }

    #[test]
    fn test_http_call_rejects_x_tezos_headers() {
        let arena = typed_arena::Arena::new();
        let value = build_http_call_micheline(
            &arena,
            "http://michelson/KT1abc",
            &[("X-Tezos-Sender", "attacker")],
            &[],
            0,
        );
        let typed = typecheck_call(&value).unwrap();
        let result = extract_http_call_request(typed);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("X-Tezos-"),
            "error should mention the header name: {err}"
        );
    }

    #[test]
    fn test_http_call_rejects_x_tezos_headers_case_insensitive() {
        let arena = typed_arena::Arena::new();
        let value = build_http_call_micheline(
            &arena,
            "http://michelson/KT1abc",
            &[("x-tezos-amount", "999")],
            &[],
            0,
        );
        let typed = typecheck_call(&value).unwrap();
        let result = extract_http_call_request(typed);
        assert!(result.is_err());
    }

    // --- Cross-runtime call: amount edge cases ---

    #[test]
    fn test_cross_runtime_call_zero_amount() {
        let mut host = MockKernelHost::default();
        let generated_alias = vec![0x01, 0x02, 0x03, 0x04];
        let registry = MockRegistry::new(generated_alias);

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";
        let amount = 0i64;

        let mut journal = TezosXJournal::new();
        let mut ctx = MockCtx::new(&mut host, source, amount);
        let result =
            tezosx_cross_runtime_call(&registry, &mut journal, &mut ctx, dest, &[]);
        assert!(result.is_ok());

        let serve_calls = registry.serve_calls.borrow();
        assert_eq!(serve_calls.len(), 1);
        assert_eq!(serve_calls[0].headers().get(X_TEZOS_AMOUNT).unwrap(), "0");
    }

    #[test]
    fn test_cross_runtime_call_negative_amount_rejected() {
        let mut host = MockKernelHost::default();
        let generated_alias = vec![0x01, 0x02, 0x03, 0x04];
        let registry = MockRegistry::new(generated_alias);

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";
        let amount = -1i64;

        let mut journal = TezosXJournal::new();
        let mut ctx = MockCtx::new(&mut host, source, amount);
        let result =
            tezosx_cross_runtime_call(&registry, &mut journal, &mut ctx, dest, &[]);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("Negative amount"),
            "error should mention negative amount: {err}"
        );
    }

    #[test]
    fn test_cross_runtime_call_non_success_response() {
        let mut host = MockKernelHost::default();
        let generated_alias = vec![0x01, 0x02, 0x03, 0x04];
        let registry = MockRegistryWithStatus::new(
            generated_alias,
            500,
            b"internal server error".to_vec(),
        );

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";
        let amount = 100i64;

        let mut journal = TezosXJournal::new();
        let mut ctx = MockCtx::new(&mut host, source, amount);
        let result =
            tezosx_cross_runtime_call(&registry, &mut journal, &mut ctx, dest, &[]);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("500"),
            "error should contain status code: {err}"
        );
    }

    #[test]
    fn test_cross_runtime_call_400_response() {
        let mut host = MockKernelHost::default();
        let generated_alias = vec![0x01, 0x02, 0x03, 0x04];
        let registry =
            MockRegistryWithStatus::new(generated_alias, 400, b"bad request".to_vec());

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";

        let mut journal = TezosXJournal::new();
        let mut ctx = MockCtx::new(&mut host, source, 0);
        let result =
            tezosx_cross_runtime_call(&registry, &mut journal, &mut ctx, dest, &[]);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("bad request"),
            "error should contain response body: {err}"
        );
    }

    // --- address_hash_bytes encoding ---

    #[test]
    fn test_address_hash_bytes_gateway() {
        let bytes = EnshrinedContracts::TezosXGateway.address_hash_bytes();
        assert_eq!(bytes[0], 0x01);
        assert_eq!(&bytes[1..21], &GATEWAY_ADDRESS);
        assert_eq!(bytes[21], 0x00);
    }

    #[test]
    fn test_address_hash_bytes_erc20_wrapper() {
        let bytes = EnshrinedContracts::ERC20Wrapper.address_hash_bytes();
        assert_eq!(bytes[0], 0x01);
        assert_eq!(&bytes[1..21], &ERC20_WRAPPER_ADDRESS);
        assert_eq!(bytes[21], 0x00);
    }

    // --- bigint_to_u256 / biguint_to_u256 edge cases ---

    #[test]
    fn test_bigint_to_u256_zero() {
        let result = bigint_to_u256(&BigInt::from(0));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), U256::zero());
    }

    #[test]
    fn test_bigint_to_u256_positive() {
        let result = bigint_to_u256(&BigInt::from(42));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), U256::from(42));
    }

    #[test]
    fn test_bigint_to_u256_large_value() {
        // U256::MAX = 2^256 - 1
        let max_u256_bytes = [0xFFu8; 32];
        let value =
            num_bigint::BigInt::from_bytes_le(num_bigint::Sign::Plus, &max_u256_bytes);
        let result = bigint_to_u256(&value);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), U256::MAX);
    }

    #[test]
    fn test_bigint_to_u256_overflow() {
        // Value larger than 32 bytes overflows
        let mut bytes = vec![0xFF; 33];
        bytes[32] = 0x01;
        let value = num_bigint::BigInt::from_bytes_le(num_bigint::Sign::Plus, &bytes);
        let result = bigint_to_u256(&value);
        assert!(result.is_err());
    }

    #[test]
    fn test_biguint_to_u256_zero() {
        let result = biguint_to_u256(num_bigint::BigUint::from(0u32));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), U256::zero());
    }

    #[test]
    fn test_biguint_to_u256_overflow() {
        let mut bytes = vec![0xFF; 33];
        bytes[32] = 0x01;
        let value = num_bigint::BigUint::from_bytes_le(&bytes);
        let result = biguint_to_u256(value);
        assert!(result.is_err());
    }

    // --- inject_context_headers_raw edge cases ---

    #[test]
    fn test_inject_context_headers_raw_zero_amount() {
        let mut headers = http::HeaderMap::new();
        inject_context_headers_raw(&mut headers, b"sender", b"source", 0u64, 0, 0u64, 0)
            .unwrap();
        assert_eq!(headers.get("X-Tezos-Amount").unwrap(), "0");
        assert_eq!(headers.get("X-Tezos-Gas-Limit").unwrap(), "0");
        assert_eq!(headers.get("X-Tezos-Timestamp").unwrap(), "0");
        assert_eq!(headers.get("X-Tezos-Block-Number").unwrap(), "0");
    }

    #[test]
    fn test_inject_context_headers_raw_max_values() {
        let mut headers = http::HeaderMap::new();
        inject_context_headers_raw(
            &mut headers,
            b"sender",
            b"source",
            u64::MAX,
            u64::MAX,
            u64::MAX,
            u32::MAX,
        )
        .unwrap();
        // u64::MAX mutez is a very large TEZ amount
        let amount = headers.get("X-Tezos-Amount").unwrap().to_str().unwrap();
        assert!(!amount.is_empty());
        assert_eq!(
            headers.get("X-Tezos-Gas-Limit").unwrap(),
            &u64::MAX.to_string()
        );
        assert_eq!(
            headers.get("X-Tezos-Timestamp").unwrap(),
            &u64::MAX.to_string()
        );
        assert_eq!(
            headers.get("X-Tezos-Block-Number").unwrap(),
            &u32::MAX.to_string()
        );
    }

    // --- ERC20 wrapper edge cases ---

    #[test]
    fn test_abi_encode_address_uint256_zero_amount() {
        let addr = vec![0x11; 20];
        let amount = BigInt::from(0);
        let result =
            abi_encode_address_uint256("transfer(address,uint256)", &addr, &amount);
        assert!(result.is_ok());
        let calldata = result.unwrap();
        // 4 bytes selector + 32 bytes address + 32 bytes value
        assert_eq!(calldata.len(), 68);
        // The uint256 part should be all zeros
        assert!(calldata[36..68].iter().all(|&b| b == 0));
    }

    #[test]
    fn test_abi_encode_address_uint256_negative_amount() {
        let addr = vec![0x11; 20];
        let amount = BigInt::from(-1);
        let result =
            abi_encode_address_uint256("transfer(address,uint256)", &addr, &amount);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("non-negative"),
            "error should mention non-negative: {err}"
        );
    }

    #[test]
    fn test_abi_encode_address_exceeding_20_bytes() {
        let addr = vec![0x11; 21]; // 21 bytes — too long
        let amount = BigInt::from(100);
        let result =
            abi_encode_address_uint256("transfer(address,uint256)", &addr, &amount);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("20 bytes"),
            "error should mention 20 bytes: {err}"
        );
    }

    #[test]
    fn test_abi_encode_amount_exceeding_uint256() {
        let addr = vec![0x11; 20];
        // 33 bytes — exceeds uint256 (32 bytes)
        let bytes = vec![0xFF; 33];
        let amount = num_bigint::BigInt::from_bytes_be(num_bigint::Sign::Plus, &bytes);
        let result =
            abi_encode_address_uint256("transfer(address,uint256)", &addr, &amount);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("uint256"),
            "error should mention uint256: {err}"
        );
    }

    #[test]
    fn test_abi_encode_address_short_address() {
        // Address shorter than 20 bytes should be left-padded
        let addr = vec![0xAB; 4]; // Only 4 bytes
        let amount = BigInt::from(1);
        let result =
            abi_encode_address_uint256("transfer(address,uint256)", &addr, &amount);
        assert!(result.is_ok());
        let calldata = result.unwrap();
        // First 4 bytes are selector, next 32 bytes are left-padded address
        // 28 zero bytes followed by 4 bytes of 0xAB
        assert!(calldata[4..32].iter().all(|&b| b == 0));
        assert!(calldata[32..36].iter().all(|&b| b == 0xAB));
    }

    // --- Non-enshrined contract detection ---

    #[test]
    fn test_non_enshrined_contract() {
        let non_enshrined = ContractKt1Hash::try_from_bytes(&[0x01; 20]).unwrap();
        assert!(!is_enshrined(&non_enshrined));
        assert_eq!(from_kt1(&non_enshrined), None);
    }

    #[test]
    fn test_enshrined_prefix_mismatch() {
        // Starts with ENSHRINED_PREFIX but doesn't match either contract
        let mut bytes = ENSHRINED_PREFIX.to_vec();
        bytes.extend_from_slice(&[0x99; 14]);
        let contract = ContractKt1Hash::try_from_bytes(&bytes).unwrap();
        assert!(!is_enshrined(&contract));
        assert_eq!(from_kt1(&contract), None);
    }

    // --- Gateway entrypoint edge cases ---

    #[test]
    fn test_gateway_unknown_entrypoint() {
        let entrypoints =
            get_enshrined_contract_entrypoint(EnshrinedContracts::TezosXGateway).unwrap();
        let unknown = Entrypoint::try_from("nonexistent").unwrap();
        assert!(!entrypoints.contains_key(&unknown));
    }

    #[test]
    fn test_erc20_wrapper_entrypoints() {
        let entrypoints =
            get_enshrined_contract_entrypoint(EnshrinedContracts::ERC20Wrapper).unwrap();
        assert!(entrypoints.contains_key(&Entrypoint::try_from("transfer").unwrap()));
        assert!(entrypoints.contains_key(&Entrypoint::try_from("approve").unwrap()));
        assert!(!entrypoints.contains_key(&Entrypoint::default()));
    }

    // --- HTTP call method mapping ---

    #[test]
    fn test_http_call_method_0_is_get() {
        let arena = typed_arena::Arena::new();
        let value =
            build_http_call_micheline(&arena, "http://michelson/KT1abc", &[], &[], 0);
        let typed = typecheck_call(&value).unwrap();
        let request = extract_http_call_request(typed).unwrap();
        assert_eq!(request.method(), http::Method::GET);
    }

    #[test]
    fn test_http_call_method_1_is_post() {
        let arena = typed_arena::Arena::new();
        let value =
            build_http_call_micheline(&arena, "http://michelson/KT1abc", &[], &[], 1);
        let typed = typecheck_call(&value).unwrap();
        let request = extract_http_call_request(typed).unwrap();
        assert_eq!(request.method(), http::Method::POST);
    }

    #[test]
    fn test_http_call_unknown_method_defaults_to_post() {
        let arena = typed_arena::Arena::new();
        let value =
            build_http_call_micheline(&arena, "http://michelson/KT1abc", &[], &[], 99);
        let typed = typecheck_call(&value).unwrap();
        let request = extract_http_call_request(typed).unwrap();
        assert_eq!(request.method(), http::Method::POST);
    }

    // --- Amount header formatting in cross-runtime call ---

    #[test]
    fn test_cross_runtime_call_large_amount_header() {
        let mut host = MockKernelHost::default();
        let generated_alias = vec![0x01, 0x02, 0x03, 0x04];
        let registry = MockRegistry::new(generated_alias);

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";
        // 1_000_000 mutez = 1 TEZ
        let amount = 1_000_000i64;

        let mut journal = TezosXJournal::new();
        let mut ctx = MockCtx::new(&mut host, source, amount);
        let result =
            tezosx_cross_runtime_call(&registry, &mut journal, &mut ctx, dest, &[]);
        assert!(result.is_ok());

        let serve_calls = registry.serve_calls.borrow();
        assert_eq!(serve_calls[0].headers().get(X_TEZOS_AMOUNT).unwrap(), "1");
    }

    #[test]
    fn test_cross_runtime_call_fractional_amount_header() {
        let mut host = MockKernelHost::default();
        let generated_alias = vec![0x01, 0x02, 0x03, 0x04];
        let registry = MockRegistry::new(generated_alias);

        let source = AddressHash::from_bytes(&[
            0x00, 0x00, 0x6b, 0x82, 0x19, 0x8e, 0xb6, 0x4a, 0x5f, 0x10, 0x19, 0x24, 0x42,
            0x40, 0xe0, 0x7c, 0xb2, 0x85, 0x22, 0x76, 0xa0, 0x05,
        ])
        .unwrap();
        let dest = "0x1234567890123456789012345678901234567890";
        // 1 mutez = 0.000001 TEZ
        let amount = 1i64;

        let mut journal = TezosXJournal::new();
        let mut ctx = MockCtx::new(&mut host, source, amount);
        let result =
            tezosx_cross_runtime_call(&registry, &mut journal, &mut ctx, dest, &[]);
        assert!(result.is_ok());

        let serve_calls = registry.serve_calls.borrow();
        assert_eq!(
            serve_calls[0].headers().get(X_TEZOS_AMOUNT).unwrap(),
            "0.000001"
        );
    }

    // --- Typecheck entrypoint value errors ---

    #[test]
    fn test_typecheck_unknown_entrypoint_returns_error() {
        let value = Micheline::String("hello".to_string());
        let result = typecheck_entrypoint_value(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::try_from("nonexistent").unwrap(),
            &value,
        );
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("Unknown entrypoint"),
            "error should mention unknown entrypoint: {err}"
        );
    }

    #[test]
    fn test_typecheck_wrong_type_for_default_entrypoint() {
        // Default entrypoint expects a string, not bytes
        let value = Micheline::Bytes(vec![0x01, 0x02]);
        let result = typecheck_entrypoint_value(
            EnshrinedContracts::TezosXGateway,
            &Entrypoint::default(),
            &value,
        );
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.to_string().contains("Invalid parameters"),
            "error should mention invalid parameters: {err}"
        );
    }
}

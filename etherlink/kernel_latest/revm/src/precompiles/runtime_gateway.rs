use alloy_sol_types::{sol, SolError, SolInterface, SolValue};
use evm_types::{CustomPrecompileAbort, CustomPrecompileError};
use http::header::HeaderMap;
use revm::{
    context::{Block, ContextTr, JournalTr, Transaction},
    context_interface::journaled_state::account::JournaledAccountTr,
    interpreter::{CallInputs, Gas, InstructionResult, InterpreterResult},
    primitives::{
        alloy_primitives::{hex::FromHex, IntoLogData},
        Address, Bytes, Log, U256,
    },
};
use tezos_ethereum::wei::{mutez_to_evm_gas, Wei};
use tezosx_interfaces::{
    canonicalize_native_address, gas,
    headers::{format_tez_from_wei, parse_u64_opt},
    translate_original_source, AliasInfo, Classification, Origin, Registry, RuntimeId,
    ALIAS_LOOKUP_COST, ERR_FORBIDDEN_TEZOS_HEADER, X_TEZOS_AMOUNT, X_TEZOS_BLOCK_NUMBER,
    X_TEZOS_CRAC_DEPTH, X_TEZOS_CRAC_ID, X_TEZOS_GAS_CONSUMED, X_TEZOS_GAS_LIMIT,
    X_TEZOS_SENDER, X_TEZOS_SOURCE, X_TEZOS_SOURCE_RUNTIME, X_TEZOS_STORAGE_COST,
    X_TEZOS_TIMESTAMP,
};

use crate::{
    database::EtherlinkVMDB,
    helpers::legacy::alloy_to_u256,
    journal::{CrossRuntimeCall, Journal},
    precompiles::{
        constants::{
            DERIVE_ALIAS_STRING_COST, HEADER_VALIDATION_PER_HEADER, ORIGIN_OF_BASE_COST,
            RESOLVE_ADDRESS_BASE_COST, RUNTIME_GATEWAY_BASE_COST,
            RUNTIME_GATEWAY_PER_WORD_COST, RUNTIME_GATEWAY_PRECOMPILE_ADDRESS,
            VALUE_TRANSFER_SURCHARGE,
        },
        guard::charge,
        runtime_gateway::RuntimeGateway::RuntimeGatewayCalls,
    },
};
use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_journal::OriginalSource;

sol! {
    contract RuntimeGateway {
        function callMichelson(
            string destination,
            string entrypoint,
            bytes parameters,
        ) external;

        function callMichelsonView(
            string destination,
            string viewName,
            bytes input,
        ) external returns (bytes memory response);

        function call(
            string url,
            (string, string)[] headers,
            bytes body,
            uint8 method,
        ) external returns (bytes memory response);

        /// Translate `addr` (canonical printable form in `sourceRuntime`) into
        /// `targetRuntime`'s representation.
        ///
        /// classified == false: addr is malformed for sourceRuntime, OR no
        ///                      /origin record exists under sourceRuntime.
        /// classified == true:  res in {0=Recorded, 1=Derived}; translated is
        ///                      the target's canonical form.
        function resolveAddress(
            string addr,
            uint8 sourceRuntime,
            uint8 targetRuntime,
        ) external view returns (bool classified, uint8 res, string translated);

        /// Look up `addr`'s classification under `sourceRuntime`.
        ///
        /// kind:  0=Unknown, 1=Native, 2=Alias.
        /// homeRuntime / nativeAddress are zero-initialized when kind == Unknown.
        /// When kind == Native, nativeAddress echoes addr.
        function originOf(
            string addr,
            uint8 sourceRuntime,
        ) external view returns (uint8 kind, uint8 homeRuntime, string nativeAddress);

        error InvalidRuntimeId(uint8 received);
    }

    /// Emitted on every outgoing CRAC (EVM -> other runtime).
    /// `crossRuntimeCallId` allows indexers to correlate operations across
    /// derived blocks.
    event CrossRuntimeCallSent(
        string crossRuntimeCallId,
        string targetRuntime,
        string targetAddress,
        uint256 amount
    );
}

// Resolution kind constants for resolveAddress.res
// Encoded as u16 because alloy's SolValue is not implemented for u8.
const RESOLUTION_RECORDED: u16 = 0;
const RESOLUTION_DERIVED: u16 = 1;

// Origin kind constants for originOf.kind
const ORIGIN_KIND_UNKNOWN: u16 = 0;
const ORIGIN_KIND_NATIVE: u16 = 1;
const ORIGIN_KIND_ALIAS: u16 = 2;

/// Charge `RUNTIME_GATEWAY_PER_WORD_COST * ceil(bytes / 32)` for a chunk
/// of gateway payload (calldata, outgoing body, or incoming response).
fn charge_payload(gas: &mut Gas, bytes: usize) -> Result<(), CustomPrecompileError> {
    let words = (bytes as u64).div_ceil(32);
    let cost = RUNTIME_GATEWAY_PER_WORD_COST.saturating_mul(words);
    if cost > 0 {
        charge(gas, cost)?;
    }
    Ok(())
}

/// Charge the EVM caller for `consumed` gas reported in `from` runtime
/// units. The conversion is rounded UP (L2-1751) so the charge always
/// covers the consumed amount instead of leaking the sub-`EVM_GAS_TO_MILLIGAS`
/// remainder; a conversion overflow charges the full budget (OOG).
fn charge_consumed_gas(
    gas: &mut Gas,
    from: RuntimeId,
    consumed: u64,
) -> Result<(), CustomPrecompileError> {
    let consumed_evm =
        gas::convert_ceil(from, RuntimeId::Ethereum, consumed).unwrap_or(u64::MAX);
    if consumed_evm > 0 {
        charge(gas, consumed_evm)?;
    }
    Ok(())
}

/// Charge the gateway's flat base cost plus the per-word payload
/// surcharge on the inbound calldata and the outgoing request body.
///
/// Shared by every CRAC entrypoint (`callMichelson`,
/// `callMichelsonView`, `call`) so the upfront gas model stays identical
/// across them. The per-word helper charges nothing for an empty body.
fn charge_gateway_request(
    gas: &mut Gas,
    calldata_len: usize,
    body_len: usize,
) -> Result<(), CustomPrecompileError> {
    charge(gas, RUNTIME_GATEWAY_BASE_COST)?;
    charge_payload(gas, calldata_len)?;
    charge_payload(gas, body_len)?;
    Ok(())
}

/// Classify the CRAC response (charging the converted callee gas), then
/// charge the per-word surcharge on the returned response body and
/// ABI-encode it for return to the EVM caller.
///
/// Shared by the entrypoints that surface the response body to the
/// caller (`callMichelsonView`, `call`). Entrypoints that discard the
/// body (`callMichelson`) call
/// [`classify_and_charge_crac_response`] directly instead.
fn charge_and_encode_crac_response(
    response: http::Response<Vec<u8>>,
    target_runtime: RuntimeId,
    gas: &mut Gas,
    base_fee_per_gas: u64,
) -> Result<Vec<u8>, CustomPrecompileError> {
    let body = classify_and_charge_crac_response(
        response,
        target_runtime,
        gas,
        base_fee_per_gas,
    )?;
    charge_payload(gas, body.len())?;
    Ok((body,).abi_encode_params())
}

/// Build an `http::Request<Vec<u8>>` from ABI-decoded parameters.
fn build_http_request(
    url: &str,
    headers: &[(String, String)],
    body: &[u8],
    method_u8: u8,
    gas: &mut Gas,
) -> Result<http::Request<Vec<u8>>, CustomPrecompileError> {
    // Charge per-header validation cost for user-supplied headers
    let header_cost = HEADER_VALIDATION_PER_HEADER
        .checked_mul(headers.len().try_into().map_err(|_| {
            CustomPrecompileError::Revert("header cost overflow".into(), *gas)
        })?)
        .ok_or_else(|| {
            CustomPrecompileError::Revert("header cost overflow".into(), *gas)
        })?;
    if header_cost > 0 {
        charge(gas, header_cost)?;
    }
    let method = match method_u8 {
        0 => http::Method::GET,
        1 => http::Method::POST,
        _ => {
            return Err(CustomPrecompileError::Revert(
                format!("unsupported HTTP method: {method_u8}"),
                *gas,
            ))
        }
    };

    let mut builder = http::Request::builder().method(method).uri(url);

    for (name, value) in headers {
        // Zero-alloc case-insensitive prefix check (avoid
        // `to_ascii_lowercase()` which allocates a new `String` per
        // header).
        let bytes = name.as_bytes();
        if bytes.len() >= 8 && bytes[..8].eq_ignore_ascii_case(b"x-tezos-") {
            return Err(CustomPrecompileError::Revert(
                format!("{ERR_FORBIDDEN_TEZOS_HEADER}: {name}"),
                *gas,
            ));
        }
        builder = builder.header(name.as_str(), value.as_str());
    }

    builder.body(body.to_vec()).map_err(|e| {
        CustomPrecompileError::Revert(format!("failed to build HTTP request: {e}"), *gas)
    })
}

/// Classify a CRAC HTTP response, charge gas, and return the body on success.
///
/// Status is classified before any charge, so a bad gas value on a failure
/// cannot flip a catchable revert into an out-of-gas.
///
/// - 2xx: the header is mandatory; charge callee gas and the storage cost,
///   then return the body.
/// - 4xx (incl. 429): catchable revert; the callee gas and body surcharge
///   are charged best-effort, so charging never escalates to an out-of-gas.
///   An unmetered failure reports the op limit, which drains the budget here.
/// - anything else: block abort.
fn classify_and_charge_crac_response(
    response: http::Response<Vec<u8>>,
    target_runtime: RuntimeId,
    gas: &mut Gas,
    base_fee_per_gas: u64,
) -> Result<Vec<u8>, CustomPrecompileError> {
    let callee_gas = response
        .headers()
        .get(X_TEZOS_GAS_CONSUMED)
        .and_then(|v| v.to_str().ok())
        .and_then(|s| s.parse::<u64>().ok())
        .and_then(|c| gas::convert_ceil(target_runtime, RuntimeId::Ethereum, c));

    if response.status().is_success() {
        let Some(evm_consumed) = callee_gas else {
            return Err(CustomPrecompileError::Revert(
                "X-Tezos-Gas-Consumed header missing or invalid in cross-runtime call response".into(),
                *gas,
            ));
        };
        charge(gas, evm_consumed)?;
        let delegated_storage_cost_mutez =
            parse_u64_opt(response.headers(), X_TEZOS_STORAGE_COST)
                .map_err(|e| CustomPrecompileError::Revert(e.to_string(), *gas))?;
        charge_delegated_storage_cost(
            gas,
            delegated_storage_cost_mutez,
            base_fee_per_gas,
        )?;
        Ok(response.into_body())
    } else if response.status().is_client_error() {
        let status = response.status();
        let decoded_body = String::from_utf8_lossy(response.body()).into_owned();
        // Charge callee gas and the body surcharge best-effort: the status is
        // already a catchable revert, so charging must never escalate it into
        // an out-of-gas; on shortfall the revert still goes out.
        if let Some(evm_consumed) = callee_gas {
            let _ = charge(gas, evm_consumed);
        }
        let _ = charge_payload(gas, decoded_body.len());
        Err(CustomPrecompileError::Revert(
            format!("Cross-runtime call failed with status {status}: {decoded_body}"),
            *gas,
        ))
    } else {
        Err(CustomPrecompileError::Abort(CustomPrecompileAbort::Crac(
            format!(
                "Cross-runtime call returned status {}: {}",
                response.status(),
                String::from_utf8_lossy(response.body())
            ),
        )))
    }
}

/// Charge the EVM caller, in gas, for the storage-fee cost (in mutez)
/// a CRAC callee delegated to it.
///
/// Special cases:
/// - `None` / `Some(0)`: nothing delegated, no charge.
/// - `base_fee_per_gas == 0` with a non-zero cost: reverts (impossible
///   on production blocks per the EIP-1559 invariant, but reachable on
///   test harnesses with a default `BlockEnv`).
/// - a cost whose gas equivalent overflows `u64`: unaffordable by
///   construction, surfaced as `OutOfGas`.
fn charge_delegated_storage_cost(
    gas: &mut Gas,
    cost_mutez: Option<u64>,
    base_fee_per_gas: u64,
) -> Result<(), CustomPrecompileError> {
    let Some(v) = cost_mutez else { return Ok(()) };
    if v == 0 {
        return Ok(());
    }
    if base_fee_per_gas == 0 {
        return Err(CustomPrecompileError::Revert(
            "storage cost: cannot convert to gas, base_fee_per_gas is zero".into(),
            *gas,
        ));
    }
    let g2 = mutez_to_evm_gas(v, Wei::from(base_fee_per_gas))
        .ok_or(CustomPrecompileError::OutOfGas)?;
    charge(gas, g2)
}

/// Core logic for the `originOf` selector, extracted for unit-testability.
///
/// Handles: `read_origin` (which includes alias-lookup cost and the
/// code-presence back-stop internally), gas conversion, and ABI-encoding
/// of the return tuple.  Does **not** handle the non-payable check, the
/// DELEGATECALL/CALLCODE guard, or the initial `ORIGIN_OF_BASE_COST`
/// charge — those stay in the outer dispatch arm.
fn dispatch_origin_of<Host: StorageV1, R: Registry>(
    host: &Host,
    registry: &R,
    addr_str: String,
    source_runtime: RuntimeId,
    staged_source: Option<Origin>,
    gas: &mut Gas,
) -> Result<Vec<u8>, CustomPrecompileError> {
    // An alias staged earlier in this operation is not yet durable.
    // Consult the overlay first and skip the durable read on a hit,
    // charging the lookup cost the recorded path would charge anyway.
    let (classification, consumed_source) = match staged_source {
        Some(origin) => (Classification::from(origin), ALIAS_LOOKUP_COST),
        None => {
            let budget =
                gas::convert(RuntimeId::Ethereum, source_runtime, gas.remaining())
                    .ok_or_else(|| {
                        CustomPrecompileError::Revert(
                            "originOf: gas budget overflow".into(),
                            *gas,
                        )
                    })?;
            registry
                .read_origin(host, source_runtime, &addr_str, budget)
                .map_err(|e| {
                    CustomPrecompileError::Revert(format!("originOf: {e}"), *gas)
                })?
        }
    };
    // Convert consumed back to EVM gas and charge.
    charge_consumed_gas(gas, source_runtime, consumed_source)?;

    let output: Vec<u8> = match classification {
        Classification::Unknown => {
            (ORIGIN_KIND_UNKNOWN, 0u16, String::new()).abi_encode_params()
        }
        Classification::Native => (
            ORIGIN_KIND_NATIVE,
            u16::from(u8::from(source_runtime)),
            canonicalize_native_address(source_runtime, &addr_str),
        )
            .abi_encode_params(),
        Classification::Alias(info) => {
            let runtime = info.runtime;
            let native_str = info.into_native_address_string().map_err(|e| {
                CustomPrecompileError::Revert(
                    format!("originOf: alias native_address is not UTF-8: {e}"),
                    *gas,
                )
            })?;
            (ORIGIN_KIND_ALIAS, u16::from(u8::from(runtime)), native_str)
                .abi_encode_params()
        }
    };
    Ok(output)
}

/// Core logic for the `resolveAddress` selector, extracted for unit-testability.
///
/// Handles: runtime validation, same-source short-circuit (with addr validation),
/// alias-lookup gas charging, `read_origin` (which includes the code-presence
/// back-stop internally), derivation via `compute_alias`, destination check, and
/// ABI-encoding.  Does **not** handle the non-payable check, DELEGATECALL/CALLCODE
/// guard, or the initial `RESOLVE_ADDRESS_BASE_COST` charge.
fn dispatch_resolve_address<Host: StorageV1, R: Registry>(
    host: &Host,
    registry: &R,
    addr_str: String,
    source_runtime: RuntimeId,
    target_runtime: RuntimeId,
    staged_source: Option<Origin>,
    gas: &mut Gas,
) -> Result<Vec<u8>, CustomPrecompileError> {
    // Same-runtime short-circuit: no storage reads needed.
    // Malformed `addr` must still return (false, 0, "") even
    // when source == target.
    if source_runtime == target_runtime {
        let valid = registry
            .address_from_string(&addr_str, source_runtime)
            .is_ok();
        if !valid {
            return Ok((false, 0u16, String::new()).abi_encode_params());
        }
        return Ok((true, RESOLUTION_RECORDED, addr_str).abi_encode_params());
    }

    // An alias staged earlier in this operation is not yet durable.
    // Consult the overlay first and skip the durable read on a hit,
    // charging the lookup cost the recorded path would charge anyway.
    let (source_classification, consumed_source) = match staged_source {
        Some(origin) => (Classification::from(origin), ALIAS_LOOKUP_COST),
        None => {
            let budget =
                gas::convert(RuntimeId::Ethereum, source_runtime, gas.remaining())
                    .ok_or_else(|| {
                        CustomPrecompileError::Revert(
                            "resolveAddress: gas budget overflow".into(),
                            *gas,
                        )
                    })?;
            registry
                .read_origin(host, source_runtime, &addr_str, budget)
                .map_err(|e| {
                    CustomPrecompileError::Revert(format!("resolveAddress: {e}"), *gas)
                })?
        }
    };
    charge_consumed_gas(gas, source_runtime, consumed_source)?;

    let output: Vec<u8> = match source_classification {
        Classification::Unknown => {
            // Unknown — cannot translate.
            (false, 0u16, String::new()).abi_encode_params()
        }
        Classification::Alias(info) if info.runtime == target_runtime => {
            // Direct recorded lookup — target address is in the Alias record.
            let native_str = info.into_native_address_string().map_err(|e| {
                CustomPrecompileError::Revert(
                    format!("resolveAddress: alias native_address is not UTF-8: {e}"),
                    *gas,
                )
            })?;
            (true, RESOLUTION_RECORDED, native_str).abi_encode_params()
        }
        source_class => {
            // Derivation path: either Native or Alias pointing to a third
            // runtime (vacuous in two-runtime mode).
            let basis: Vec<u8> = match &source_class {
                Classification::Native => {
                    canonicalize_native_address(source_runtime, &addr_str).into_bytes()
                }
                Classification::Alias(info) => info.native_address.clone(),
                Classification::Unknown => unreachable!("Unknown handled above"),
            };

            // Derive the target alias.
            charge(gas, DERIVE_ALIAS_STRING_COST)?;
            let derived = registry
                .compute_alias(AliasInfo {
                    runtime: target_runtime,
                    native_address: basis.clone(),
                })
                .map_err(|e| {
                    CustomPrecompileError::Revert(
                        format!("resolveAddress: derivation error: {e}"),
                        *gas,
                    )
                })?;

            // Destination check: is the inverse already materialized on
            // the target side?
            let dest_budget =
                gas::convert(RuntimeId::Ethereum, target_runtime, gas.remaining())
                    .ok_or_else(|| {
                        CustomPrecompileError::Revert(
                            "resolveAddress: destination gas budget overflow".into(),
                            *gas,
                        )
                    })?;
            let (inverse_class, consumed_dest) = registry
                .read_origin(host, target_runtime, &derived, dest_budget)
                .map_err(|e| {
                    CustomPrecompileError::Revert(
                        format!("resolveAddress: destination check error: {e}"),
                        *gas,
                    )
                })?;
            charge_consumed_gas(gas, target_runtime, consumed_dest)?;

            let resolution = match inverse_class {
                Classification::Alias(info_back)
                    if info_back.runtime == source_runtime
                        && info_back.native_address == basis =>
                {
                    RESOLUTION_RECORDED
                }
                _ => RESOLUTION_DERIVED,
            };

            (true, resolution, derived).abi_encode_params()
        }
    };
    Ok(output)
}

/// Reset the gateway precompile's EVM balance to zero, charging the
/// value-transfer surcharge, but only when the current balance is
/// non-zero. Otherwise no SSTORE happens and no surcharge is levied.
///
/// Called after each CRAC that may have left truncation residue on the
/// precompile's balance.
fn burn_gateway_residual<'j, CTX, Host, R>(
    context: &mut CTX,
    gas: &mut Gas,
) -> Result<(), CustomPrecompileError>
where
    Host: StorageV1 + 'j,
    R: Registry + 'j,
    CTX: ContextTr<Db = EtherlinkVMDB<'j, Host, R>, Journal = Journal<'j, Host, R>>,
{
    let snapshot = *gas;
    let is_zero = {
        let account_load = context
            .journal_mut()
            .load_account_mut_skip_cold_load(RUNTIME_GATEWAY_PRECOMPILE_ADDRESS, true)
            .map_err(|_| {
                CustomPrecompileError::Revert(
                    "failed to load precompile account".into(),
                    snapshot,
                )
            })?;
        account_load.data.balance().is_zero()
    };
    if !is_zero {
        charge(gas, VALUE_TRANSFER_SURCHARGE)?;
        let snapshot = *gas;
        let mut account_load = context
            .journal_mut()
            .load_account_mut_skip_cold_load(RUNTIME_GATEWAY_PRECOMPILE_ADDRESS, true)
            .map_err(|_| {
                CustomPrecompileError::Revert(
                    "failed to load precompile account".into(),
                    snapshot,
                )
            })?;
        account_load.data.set_balance(U256::ZERO);
    }
    Ok(())
}

/// Emit a `CrossRuntimeCallSent` log for an outgoing CRAC.
///
/// Shared by the state-mutating gateway entries (`callMichelson`,
/// generic `call(POST)`) so the event schema and the
/// meaning of each field stay identical across surfaces. `target_address`
/// is always the bare target *contract* — callers strip any
/// entrypoint/view segment before passing it (L2-1456).
fn emit_crac_sent<'j, CTX, Host, R>(
    context: &mut CTX,
    crac_id: String,
    target_runtime: &str,
    target_address: String,
    amount: U256,
) where
    Host: StorageV1 + 'j,
    R: Registry + 'j,
    CTX: ContextTr<Db = EtherlinkVMDB<'j, Host, R>, Journal = Journal<'j, Host, R>>,
{
    let crac_log = Log {
        address: RUNTIME_GATEWAY_PRECOMPILE_ADDRESS,
        data: CrossRuntimeCallSent {
            crossRuntimeCallId: crac_id,
            targetRuntime: target_runtime.to_string(),
            targetAddress: target_address,
            amount,
        }
        .into_log_data(),
    };
    context.journal_mut().log(crac_log);
}

/// Build an [`OriginalSource`] for `source_addr` under the current
/// originating runtime.
///
/// Records the originator's address in its *own* native runtime plus that
/// runtime, never the per-target aliases (those translate on demand via
/// [`translate_original_source`], a pure derivation). `source_addr` is the
/// originator's EVM-form address — the handle every EVM frame has
/// (`cross_runtime_originator()` / `tx().caller()`):
/// - when the originator is Ethereum-native its native address *is*
///   `source_addr`, so capture touches no storage; this is the nominal
///   single-CRAC flow, kept read-free.
/// - when the originator is native to another runtime (a transaction that
///   originated there and crossed into EVM), its native address is
///   resolved from `source_addr` once, here — the only durable read, and
///   it lands solely on the genuinely cross-runtime path.
fn build_original_source<'j, CTX, Host, R>(
    context: &CTX,
    source_addr: Address,
    remaining_evm_gas: u64,
) -> Result<OriginalSource, CustomPrecompileError>
where
    Host: StorageV1 + 'j,
    R: Registry + 'j,
    CTX: ContextTr<Db = EtherlinkVMDB<'j, Host, R>, Journal = Journal<'j, Host, R>>,
{
    let runtime = context.journal().crac_origin_runtime();
    let original_address = if runtime == RuntimeId::Ethereum {
        source_addr.to_string().to_lowercase()
    } else {
        context.journal().tezosx_resolve_source_alias_readonly(
            source_addr,
            runtime,
            remaining_evm_gas,
        )?
    };
    Ok(OriginalSource::new(runtime, original_address))
}

/// Resolve the originator's EVM-form address as an [`Address`] for the
/// state-mutating resolver, which keys alias materialization off the
/// source address. Translates the stored native address into Ethereum
/// (identity when the originator is Ethereum-native, a pure
/// `compute_alias` otherwise) and parses the result, which only fails on
/// internal corruption.
fn original_source_evm_address<R: Registry>(
    source: &OriginalSource,
    registry: &R,
    gas: Gas,
) -> Result<Address, CustomPrecompileError> {
    let evm_str = translate_original_source(registry, source, RuntimeId::Ethereum)
        .map_err(|e| {
            CustomPrecompileError::Revert(
                format!("failed to translate originator to EVM address: {e:?}"),
                gas,
            )
        })?;
    evm_str.parse::<Address>().map_err(|e| {
        CustomPrecompileError::Revert(
            format!("invalid originator EVM address '{evm_str}': {e}"),
            gas,
        )
    })
}

/// Capture the original source for a gateway entry — both the
/// state-mutating entries and the read-only ones
/// (`callMichelsonView`, generic `call(..., GET)`) go through
/// [`resolve_original_source`], which persists the result so it
/// propagates across re-entrant frames.
///
/// Prefers the inbound CRAC's transitive originator
/// ([`CrossRuntimeCall::cross_runtime_originator`]) when one is present,
/// falling back to `tx().caller()` for a direct EVM transaction — the
/// exact precedence the custom `ORIGIN` opcode applies (`etherlink_origin`
/// in `revm/src/lib.rs`). The outgoing `X-Tezos-Source` therefore agrees
/// with `tx.origin` as observed in the same frame; without it, a
/// Michelson → EVM CRAC frame would forward the immediate sender alias as
/// `X-Tezos-Source` while `ORIGIN` reported the real transitive source.
/// `X-Tezos-Sender` stays on the immediate caller.
fn capture_original_source<'j, CTX, Host, R>(
    context: &CTX,
    remaining_evm_gas: u64,
) -> Result<OriginalSource, CustomPrecompileError>
where
    Host: StorageV1 + 'j,
    R: Registry + 'j,
    CTX: ContextTr<Db = EtherlinkVMDB<'j, Host, R>, Journal = Journal<'j, Host, R>>,
{
    let source_addr = context
        .journal()
        .cross_runtime_originator()
        .unwrap_or_else(|| context.tx().caller());
    build_original_source(context, source_addr, remaining_evm_gas)
}

/// Return the captured original source, building and persisting it on
/// the first call. Re-entrant frames (EVM → TEZ → EVM → …) re-read
/// the depth-0 capture instead of falling back to their own
/// `tx().caller()` (which would be the alias of the Michelson contract
/// that re-entered the EVM). The read-only entries persist through here
/// too, so a nested Michelson `staticcall_evm` view sees the same
/// originator on the shared journal.
fn resolve_original_source<'j, CTX, Host, R>(
    context: &mut CTX,
    remaining_evm_gas: u64,
) -> Result<OriginalSource, CustomPrecompileError>
where
    Host: StorageV1 + 'j,
    R: Registry + 'j,
    CTX: ContextTr<Db = EtherlinkVMDB<'j, Host, R>, Journal = Journal<'j, Host, R>>,
{
    if let Some(src) = context.journal().original_source() {
        return Ok(src.clone());
    }
    let source = capture_original_source(context, remaining_evm_gas)?;
    context.journal_mut().set_original_source(source.clone());
    Ok(source)
}

/// Resolves sender and source aliases, charging gas for lookups and any
/// alias generation triggered on cache miss. OOG propagates as
/// `Err(CustomPrecompileError::OutOfGas(..))`.
///
/// When `sender == source` (an EOA directly calling the gateway, the
/// common case), the resolution is performed once and the resulting
/// alias is reused for both slots — a single `ALIAS_LOOKUP_COST` is
/// billed instead of two.
fn resolve_aliases<'j, CTX, Host, R>(
    context: &mut CTX,
    gas: &mut Gas,
    target_runtime: RuntimeId,
    sender: Address,
    source: Address,
) -> Result<(String, String), CustomPrecompileError>
where
    Host: StorageV1 + 'j,
    R: Registry + 'j,
    CTX: ContextTr<Db = EtherlinkVMDB<'j, Host, R>, Journal = Journal<'j, Host, R>>,
{
    // --- sender alias ---
    charge(gas, ALIAS_LOOKUP_COST)?;
    let (sender_alias, sender_resolution) = context
        .journal_mut()
        .tezosx_resolve_source_alias(sender, target_runtime, gas.remaining())?;
    charge_consumed_gas(gas, target_runtime, sender_resolution.consumed_gas)?;
    charge_delegated_storage_cost(
        gas,
        sender_resolution.delegated_storage_cost,
        context.block().basefee(),
    )?;

    // --- source alias ---
    // Fast path: if sender == source, reuse the resolved alias and skip
    // the second storage lookup (and its cache-hit charge).
    if sender == source {
        return Ok((sender_alias.clone(), sender_alias));
    }

    charge(gas, ALIAS_LOOKUP_COST)?;
    let (source_alias, source_resolution) = context
        .journal_mut()
        .tezosx_resolve_source_alias(source, target_runtime, gas.remaining())?;
    charge_consumed_gas(gas, target_runtime, source_resolution.consumed_gas)?;
    charge_delegated_storage_cost(
        gas,
        source_resolution.delegated_storage_cost,
        context.block().basefee(),
    )?;

    Ok((sender_alias, source_alias))
}

/// Inject X-Tezos-* headers carrying the trusted execution context.
///
/// - `X-Tezos-Sender`: The resolved alias of the immediate caller (UTF-8 string).
/// - `X-Tezos-Source`: The resolved alias of the transaction originator (UTF-8 string).
/// - `X-Tezos-Source-Runtime`: The native runtime of `X-Tezos-Source`,
///   as the decimal `RuntimeId` tag. On a same-runtime `EVM -> EVM`
///   round-trip the source is an EVM origin, so the receiving frame
///   reports `sourceRuntime = ethereum` rather than a
///   hardcoded `tezos`.
/// - `X-Tezos-Amount`: The value attached to the call, as a TEZ decimal string.
/// - `X-Tezos-Gas-Limit`: The gas limit forwarded to the call (decimal string).
/// - `X-Tezos-Timestamp`: The current block timestamp in seconds (decimal string).
/// - `X-Tezos-Block-Number`: The current block number (decimal string).
#[allow(clippy::too_many_arguments)]
fn inject_tezos_headers(
    headers: &mut HeaderMap,
    sender_alias: &str,
    source_alias: &str,
    source_runtime: RuntimeId,
    amount: U256,
    gas_limit: u64,
    timestamp: U256,
    block_number: U256,
    crac_id: &str,
    crac_depth: u32,
    gas: Gas,
) -> Result<(), CustomPrecompileError> {
    let parse_value = |v: &str| -> Result<http::HeaderValue, CustomPrecompileError> {
        v.parse().map_err(|e| {
            CustomPrecompileError::Revert(format!("invalid header value: {e}"), gas)
        })
    };
    headers.insert(X_TEZOS_SENDER, parse_value(sender_alias)?);
    headers.insert(X_TEZOS_SOURCE, parse_value(source_alias)?);
    // Numeric `RuntimeId` tag, same encoding as the `originOf` /
    // `resolveAddress` ABI.
    headers.insert(
        X_TEZOS_SOURCE_RUNTIME,
        parse_value(&u8::from(source_runtime).to_string())?,
    );
    // Format the amount (in wei) as a TEZ decimal string.
    // The receiving runtime truncates to its own precision (ADR L2-1004).
    headers.insert(
        X_TEZOS_AMOUNT,
        parse_value(&format_tez_from_wei(alloy_to_u256(&amount)))?,
    );
    headers.insert(X_TEZOS_GAS_LIMIT, parse_value(&format!("{gas_limit}"))?);
    headers.insert(X_TEZOS_TIMESTAMP, parse_value(&format!("{timestamp}"))?);
    headers.insert(
        X_TEZOS_BLOCK_NUMBER,
        parse_value(&format!("{block_number}"))?,
    );
    headers.insert(X_TEZOS_CRAC_ID, parse_value(crac_id)?);
    headers.insert(X_TEZOS_CRAC_DEPTH, parse_value(&format!("{crac_depth}"))?);
    Ok(())
}

/// Read the context-driven `X-Tezos-*` values (timestamp, block number,
/// CRAC-ID, outgoing chain depth) from `context` and write them — plus
/// the per-call `sender_alias`/`source_alias`/`amount`/`gas_limit` — onto
/// `headers`. `crac_depth` is `inbound + 1`: counts CRAC hops only, never
/// REVM CALL frames.
#[allow(clippy::too_many_arguments)]
fn inject_tezos_headers_from_context<'j, CTX, Host, R>(
    context: &CTX,
    headers: &mut HeaderMap,
    sender_alias: &str,
    source_alias: &str,
    source_runtime: RuntimeId,
    amount: U256,
    gas_limit: u64,
    gas: Gas,
) -> Result<(), CustomPrecompileError>
where
    Host: StorageV1 + 'j,
    R: Registry + 'j,
    CTX: ContextTr<Db = EtherlinkVMDB<'j, Host, R>, Journal = Journal<'j, Host, R>>,
{
    let timestamp = context.block().timestamp();
    let block_number = context.block().number();
    let crac_id = context.journal().crac_id();
    let crac_depth = context.journal().crac_chain_depth().saturating_add(1);
    inject_tezos_headers(
        headers,
        sender_alias,
        source_alias,
        source_runtime,
        amount,
        gas_limit,
        timestamp,
        block_number,
        &crac_id,
        crac_depth,
        gas,
    )
}

pub(crate) fn runtime_gateway_precompile<'j, CTX, Host, R>(
    calldata: &[u8],
    context: &mut CTX,
    inputs: &CallInputs,
) -> Result<InterpreterResult, CustomPrecompileError>
where
    Host: StorageV1 + 'j,
    R: Registry + 'j,
    CTX: ContextTr<Db = EtherlinkVMDB<'j, Host, R>, Journal = Journal<'j, Host, R>>,
{
    // Reject DELEGATECALL and CALLCODE gateway-wide. Under those
    // opcodes the precompile code runs in the caller's context, which
    // would silently re-wire the sender identity used for alias
    // resolution, repurpose the caller's `msg.value` for value
    // transfers, and more generally break the assumption that the
    // gateway speaks on behalf of its direct caller. For the
    // state-mutating entries (`callMichelson`, `call`)
    // the confusion is around identity and value; for the read-only
    // `callMichelsonView` it's the same plus it sidesteps the
    // STATICCALL guarantees of the typed entry. In all three cases
    // there is no legitimate reason to reach the gateway through a
    // delegated frame — callers should use `CALL` or `STATICCALL`.
    //
    // The pattern `target_address != bytecode_address` selects
    // DELEGATECALL/CALLCODE exclusively: under `CALL` and
    // `STATICCALL` both point at the precompile address.
    let mut gas = Gas::new(inputs.gas_limit);

    if inputs.target_address != inputs.bytecode_address {
        return Err(CustomPrecompileError::Revert(
            "runtime gateway: DELEGATECALL and CALLCODE are not allowed".into(),
            gas,
        ));
    }

    let Ok(function_call) = RuntimeGatewayCalls::abi_decode(calldata) else {
        return Err(CustomPrecompileError::Revert(
            String::from("invalid input encoding"),
            gas,
        ));
    };

    // Reject STATICCALL on state-mutating selectors. The
    // `target_address != bytecode_address` guard above already covers
    // DELEGATECALL/CALLCODE; this complements it by rejecting
    // STATICCALL on the entries that would write to the journal —
    // alias persistence, `CrossRuntimeCallSent` log emission, residual balance
    // burn. REVM commits a custom precompile's journal writes
    // unconditionally on `Ok` return (`revm-handler::frame::Frame::
    // make_call_frame`), so the static-call contract is not enforced
    // for us; the check has to live here.
    //
    // The two read-only entries (`callMichelsonView` and `call` with
    // `method == GET`) intentionally bypass this guard — they thread
    // through the read-only journal API, refuse value transfer, and
    // never emit a log.
    match function_call {
        RuntimeGatewayCalls::callMichelson(call) => {
            if inputs.is_static {
                return Err(CustomPrecompileError::Revert(
                    "runtime gateway: STATICCALL not allowed on callMichelson".into(),
                    gas,
                ));
            }
            charge_gateway_request(&mut gas, calldata.len(), call.parameters.len())?;

            let destination = call.destination;
            let entrypoint = call.entrypoint;
            let parameters = call.parameters;
            let amount = inputs.value.get();

            // Build HTTP request targeting the Tezos runtime.
            let url = if entrypoint.is_empty() {
                format!("http://tezos/{destination}")
            } else {
                format!("http://tezos/{destination}/{entrypoint}")
            };
            let mut request = http::Request::builder()
                .method(http::Method::POST)
                .uri(&url)
                .body(parameters.to_vec())
                .map_err(|e| {
                    CustomPrecompileError::Revert(
                        format!("failed to build HTTP request: {e}"),
                        gas,
                    )
                })?;

            let source = resolve_original_source(context, gas.remaining())?;
            let source_addr =
                original_source_evm_address(&source, context.db().registry, gas)?;
            let (sender_alias, source_alias) = resolve_aliases(
                context,
                &mut gas,
                RuntimeId::Tezos,
                inputs.caller,
                source_addr,
            )?;

            let gas_limit =
                gas::convert(RuntimeId::Ethereum, RuntimeId::Tezos, gas.remaining())
                    .ok_or_else(|| {
                        CustomPrecompileError::Revert(
                            "callMichelson: EVM gas limit overflows Tezos milligas"
                                .into(),
                            gas,
                        )
                    })?;
            let crac_id = context.journal().crac_id();
            inject_tezos_headers_from_context(
                context,
                request.headers_mut(),
                &sender_alias,
                &source_alias,
                source.runtime(),
                amount,
                gas_limit,
                gas,
            )?;

            let response = context.journal_mut().tezosx_call_http(request);
            let _body = classify_and_charge_crac_response(
                response,
                RuntimeId::Tezos,
                &mut gas,
                context.block().basefee(),
            )?;

            emit_crac_sent(context, crac_id, "tezos", destination, amount);
        }
        RuntimeGatewayCalls::callMichelsonView(call) => {
            charge_gateway_request(&mut gas, calldata.len(), call.input.len())?;

            let destination = call.destination;
            let view_name = call.viewName;
            let input = call.input;

            // Views are read-only: no value transfer is accepted.
            if !inputs.value.get().is_zero() {
                return Err(CustomPrecompileError::Revert(
                    "callMichelsonView: view calls cannot carry value".into(),
                    gas,
                ));
            }

            if view_name.is_empty() {
                return Err(CustomPrecompileError::Revert(
                    "callMichelsonView: view name must not be empty".into(),
                    gas,
                ));
            }

            // Build a GET request targeting the Tezos runtime. The URL
            // shape `http://tezos/<kt1>/<view_name>` matches the
            // entrypoint form; the GET method is what routes it to the
            // view-execution path on the server side.
            let url = format!("http://tezos/{destination}/{view_name}");
            let mut request = http::Request::builder()
                .method(http::Method::GET)
                .uri(&url)
                .body(input.to_vec())
                .map_err(|e| {
                    CustomPrecompileError::Revert(
                        format!("failed to build HTTP request: {e}"),
                        gas,
                    )
                })?;

            // Capture and persist the top-level originator on the shared
            // journal so any nested gateway call this view triggers can
            // read it back — in particular a Michelson `staticcall_evm`
            // re-entering the EVM resolves `tx.origin` from this shared
            // value rather than a durable origin record, which a read-only
            // call never writes. Persisting a journal field is not an EVM
            // state change, so it is STATICCALL-safe.
            let source = resolve_original_source(context, gas.remaining())?;
            let sender_alias = context.journal().tezosx_resolve_source_alias_readonly(
                inputs.caller,
                RuntimeId::Tezos,
                gas.remaining(),
            )?;
            // Translate the captured originator's native address into its
            // Tezos representation on demand — identity when the originator
            // is Tezos-native, a pure `compute_alias` derivation otherwise.
            // No durable write, so it stays STATICCALL-safe.
            let source_alias = translate_original_source(
                context.db().registry,
                &source,
                RuntimeId::Tezos,
            )
            .map_err(|e| {
                CustomPrecompileError::Revert(
                    format!("callMichelsonView: failed to translate source: {e:?}"),
                    gas,
                )
            })?;

            let gas_limit =
                gas::convert(RuntimeId::Ethereum, RuntimeId::Tezos, gas.remaining())
                    .ok_or(CustomPrecompileError::Revert(
                        "callMichelsonView: EVM gas limit overflows Tezos milligas"
                            .into(),
                        gas,
                    ))?;
            // Amount is always zero (checked above), reuse the header
            // injector for uniform context propagation.
            inject_tezos_headers_from_context(
                context,
                request.headers_mut(),
                &sender_alias,
                &source_alias,
                source.runtime(),
                U256::ZERO,
                gas_limit,
                gas,
            )?;

            let response = context.journal_mut().tezosx_call_http(request);
            let output = charge_and_encode_crac_response(
                response,
                RuntimeId::Tezos,
                &mut gas,
                context.block().basefee(),
            )?;

            // Intentionally no log emission. A dedicated
            // `MichelsonViewCalled` event was considered for indexer
            // visibility, but emitting a log would make this entry
            // incompatible with STATICCALL — REVM reverts on `LOG*` in
            // a static context. Indexers that need to observe view
            // reads can subscribe to the cross-runtime HTTP trace on
            // the node side, which already carries richer structured
            // data than a flat event would.

            return Ok(InterpreterResult {
                result: InstructionResult::Return,
                gas,
                output: output.into(),
            });
        }
        RuntimeGatewayCalls::call(call) => {
            charge_gateway_request(&mut gas, calldata.len(), call.body.len())?;

            let mut request = build_http_request(
                &call.url,
                &call.headers,
                &call.body,
                call.method,
                &mut gas,
            )?;

            let target_runtime = request
                .uri()
                .host()
                .and_then(RuntimeId::from_host)
                .ok_or_else(|| {
                    CustomPrecompileError::Revert(
                        "httpCall: unknown or missing target runtime in URL host".into(),
                        gas,
                    )
                })?;

            // GET requests target read-only handlers on the other
            // runtime (Michelson view / EVM STATICCALL), so this arm
            // mirrors the STATICCALL-compatibility contract of
            // `callMichelsonView`: no value transfer, no log
            // emission, and alias resolution goes through the
            // read-only path that never writes to storage. POST keeps
            // the existing state-mutating behavior.
            let is_get = request.method() == http::Method::GET;
            let amount = inputs.value.get();

            let (sender_alias, source_alias, source_runtime) = if is_get {
                if !amount.is_zero() {
                    return Err(CustomPrecompileError::Revert(
                        "call: GET requests cannot carry value".into(),
                        gas,
                    ));
                }
                // Capture and persist the originator on the shared
                // journal, as `callMichelsonView` does, then translate its
                // native address into the target runtime on demand — a pure
                // derivation, no durable write, so it stays STATICCALL-safe.
                let source = resolve_original_source(context, gas.remaining())?;
                let source_alias = translate_original_source(
                    context.db().registry,
                    &source,
                    target_runtime,
                )
                .map_err(|e| {
                    CustomPrecompileError::Revert(
                        format!("call: failed to translate source: {e:?}"),
                        gas,
                    )
                })?;
                (
                    context.journal().tezosx_resolve_source_alias_readonly(
                        inputs.caller,
                        target_runtime,
                        gas.remaining(),
                    )?,
                    source_alias,
                    source.runtime(),
                )
            } else {
                if inputs.is_static {
                    return Err(CustomPrecompileError::Revert(
                        "runtime gateway: STATICCALL not allowed on call with non-GET method".into(),
                        gas,
                    ));
                }
                let source = resolve_original_source(context, gas.remaining())?;
                let source_addr =
                    original_source_evm_address(&source, context.db().registry, gas)?;
                let (sender_alias, source_alias) = resolve_aliases(
                    context,
                    &mut gas,
                    target_runtime,
                    inputs.caller,
                    source_addr,
                )?;
                (sender_alias, source_alias, source.runtime())
            };

            let gas_limit =
                gas::convert(RuntimeId::Ethereum, target_runtime, gas.remaining())
                    .ok_or_else(|| {
                        CustomPrecompileError::Revert(
                            "httpCall: EVM gas limit overflows target runtime units"
                                .into(),
                            gas,
                        )
                    })?;
            let crac_id = context.journal().crac_id();
            // Inject X-Tezos-* headers with trusted execution context.
            // These carry the call context that the target runtime's `serve`
            // implementation needs (sender, source, amount, gas, block info).
            inject_tezos_headers_from_context(
                context,
                request.headers_mut(),
                &sender_alias,
                &source_alias,
                source_runtime,
                amount,
                gas_limit,
                gas,
            )?;

            // Extract URI info before the request is consumed by the call.
            let uri = request.uri().clone();
            let target_rt = uri.host().unwrap_or("unknown").to_string();
            // `targetAddress` identifies the target *contract*. A target
            // URI is `http://<runtime>/<address>[/<entrypoint-or-view>]`,
            // so keep only the first path segment — the address — and
            // drop any trailing entrypoint/view. This matches the typed
            // `callMichelson` entry, which emits the bare
            // destination; reporting the full path here would split one
            // contract into two indexer identities depending on the ABI
            // surface used (L2-1456).
            let target_addr = uri
                .path()
                .trim_start_matches('/')
                .split('/')
                .next()
                .unwrap_or("")
                .to_string();

            let response = context.journal_mut().tezosx_call_http(request);
            let output = charge_and_encode_crac_response(
                response,
                target_runtime,
                &mut gas,
                context.block().basefee(),
            )?;

            // POST-only state effects: burn any residual precompile
            // balance from a bridged value, and emit the CrossRuntimeCallSent
            // event. Both are incompatible with STATICCALL and
            // intentionally skipped on GET — for which the entry is
            // read-only end-to-end.
            if !is_get {
                burn_gateway_residual(context, &mut gas)?;
                emit_crac_sent(context, crac_id, &target_rt, target_addr, amount);
            }

            return Ok(InterpreterResult {
                result: InstructionResult::Return,
                gas,
                output: output.into(),
            });
        }
        // View-only: no log, no balance touch, no journal write — safe under STATICCALL.
        RuntimeGatewayCalls::originOf(call) => {
            charge(&mut gas, ORIGIN_OF_BASE_COST)?;

            // View-only: non-payable.
            if !inputs.value.get().is_zero() {
                return Err(CustomPrecompileError::Revert(
                    "originOf: non-payable selector".into(),
                    gas,
                ));
            }

            let source_runtime =
                RuntimeId::try_from(call.sourceRuntime).map_err(|_| {
                    let payload = RuntimeGateway::InvalidRuntimeId {
                        received: call.sourceRuntime,
                    }
                    .abi_encode();
                    CustomPrecompileError::RevertWithData(payload, gas)
                })?;

            // The staged overlay only holds EVM aliases, so it is
            // consulted only when the source runtime is Ethereum.
            let staged_source = if source_runtime == RuntimeId::Ethereum {
                Address::from_hex(&call.addr).ok().and_then(|addr| {
                    context
                        .journal()
                        .journal
                        .evm
                        .layered_state
                        .pending_alias_origin(&addr)
                })
            } else {
                None
            };
            let output = dispatch_origin_of(
                context.db().host,
                context.db().registry,
                call.addr,
                source_runtime,
                staged_source,
                &mut gas,
            )?;

            return Ok(InterpreterResult {
                result: InstructionResult::Return,
                gas,
                output: output.into(),
            });
        }
        // View-only: no log, no balance touch, no journal write — safe under STATICCALL.
        RuntimeGatewayCalls::resolveAddress(call) => {
            charge(&mut gas, RESOLVE_ADDRESS_BASE_COST)?;

            // View-only: non-payable.
            if !inputs.value.get().is_zero() {
                return Err(CustomPrecompileError::Revert(
                    "resolveAddress: non-payable selector".into(),
                    gas,
                ));
            }

            let source_runtime =
                RuntimeId::try_from(call.sourceRuntime).map_err(|_| {
                    let payload = RuntimeGateway::InvalidRuntimeId {
                        received: call.sourceRuntime,
                    }
                    .abi_encode();
                    CustomPrecompileError::RevertWithData(payload, gas)
                })?;
            let target_runtime =
                RuntimeId::try_from(call.targetRuntime).map_err(|_| {
                    let payload = RuntimeGateway::InvalidRuntimeId {
                        received: call.targetRuntime,
                    }
                    .abi_encode();
                    CustomPrecompileError::RevertWithData(payload, gas)
                })?;

            // The staged overlay only holds EVM aliases, so it is
            // consulted only when the source runtime is Ethereum.
            let staged_source = if source_runtime == RuntimeId::Ethereum {
                Address::from_hex(&call.addr).ok().and_then(|addr| {
                    context
                        .journal()
                        .journal
                        .evm
                        .layered_state
                        .pending_alias_origin(&addr)
                })
            } else {
                None
            };
            let output = dispatch_resolve_address(
                context.db().host,
                context.db().registry,
                call.addr,
                source_runtime,
                target_runtime,
                staged_source,
                &mut gas,
            )?;

            return Ok(InterpreterResult {
                result: InstructionResult::Return,
                gas,
                output: output.into(),
            });
        }
    }

    // The value (if any) was bridged to Tezos — erase any residual
    // balance from the precompile.
    burn_gateway_residual(context, &mut gas)?;

    Ok(InterpreterResult {
        result: InstructionResult::Return,
        gas,
        output: Bytes::new(),
    })
}

#[cfg(test)]
mod tests {
    use alloy_sol_types::SolCall;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_protocol::contract::Contract;
    use tezosx_interfaces::testing::StubRegistry;
    use tezosx_interfaces::{AliasInfo, Classification, RuntimeId};

    use super::*;

    // A failure with an oversized gas header stays a catchable revert:
    // charging the value is best-effort and never escalates to an
    // out-of-gas.
    #[test]
    fn classify_4xx_with_oversized_header_still_reverts() {
        let response = http::Response::builder()
            .status(http::StatusCode::BAD_REQUEST)
            .header(X_TEZOS_GAS_CONSUMED, u64::MAX.to_string())
            .body(b"revert reason".to_vec())
            .unwrap();
        let mut gas = Gas::new(100_000);
        let result =
            classify_and_charge_crac_response(response, RuntimeId::Tezos, &mut gas, 1);
        assert!(matches!(result, Err(CustomPrecompileError::Revert(_, _))));
    }

    // An unmetered failure reports the op limit as its consumed gas. The
    // value round-trips to the caller's remaining budget, so charging it
    // drains the budget and the call stays a catchable revert.
    #[test]
    fn classify_4xx_with_op_limit_header_drains_and_reverts() {
        let remaining = 100_000u64;
        let op_limit_milligas =
            gas::convert(RuntimeId::Ethereum, RuntimeId::Tezos, remaining).unwrap();
        let response = http::Response::builder()
            .status(http::StatusCode::BAD_REQUEST)
            .header(X_TEZOS_GAS_CONSUMED, op_limit_milligas.to_string())
            .body(b"revert reason".to_vec())
            .unwrap();
        let mut gas = Gas::new(remaining);
        let result =
            classify_and_charge_crac_response(response, RuntimeId::Tezos, &mut gas, 1);
        assert!(matches!(result, Err(CustomPrecompileError::Revert(_, _))));
        assert_eq!(gas.remaining(), 0, "the op limit must drain the budget");
    }

    // These tests call the dispatch helpers (dispatch_origin_of /
    // dispatch_resolve_address) directly. Going through
    // runtime_gateway_precompile would require constructing a
    // CTX: ContextTr<Db = EtherlinkVMDB<…>>, which needs a full revm
    // Context + Journal wiring not practical in this crate. The real
    // code-presence back-stop is tested in tezosx-ethereum-runtime.

    // ── originOf: Classification rows via dispatch_origin_of ─────────────────
    //
    // Note: alloy's SolValue is not implemented for u8, so the uint8 fields
    // are encoded as u16.  Both types produce the same 32-byte ABI word and
    // the low byte is what Solidity reads.

    #[test]
    fn origin_of_abi_encoding_unknown() {
        let output: Vec<u8> =
            (ORIGIN_KIND_UNKNOWN, 0u16, String::new()).abi_encode_params();
        // (uint16, uint16, string) with empty string:
        // slot 1: kind (32 bytes)
        // slot 2: homeRuntime (32 bytes)
        // slot 3: offset to string data (32 bytes)
        // slot 4: string length = 0 (32 bytes)
        // total: 4 * 32 = 128 bytes
        assert_eq!(output.len(), 128);
        // kind == 0 at byte 31
        assert_eq!(output[31], ORIGIN_KIND_UNKNOWN as u8);
    }

    #[test]
    fn origin_of_abi_encoding_native() {
        let addr = "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".to_string();
        let output: Vec<u8> = (
            ORIGIN_KIND_NATIVE,
            u16::from(RuntimeId::Ethereum as u8),
            addr.clone(),
        )
            .abi_encode_params();
        assert_eq!(output[31], ORIGIN_KIND_NATIVE as u8);
        assert_eq!(output[63], RuntimeId::Ethereum as u8);
    }

    #[test]
    fn origin_of_abi_encoding_alias() {
        let native = "KT1_X".to_string();
        let output: Vec<u8> =
            (ORIGIN_KIND_ALIAS, u16::from(RuntimeId::Tezos as u8), native)
                .abi_encode_params();
        assert_eq!(output[31], ORIGIN_KIND_ALIAS as u8);
        assert_eq!(output[63], RuntimeId::Tezos as u8);
    }

    // ── ABI encoding: resolveAddress tuple shape ──────────────────────────────

    #[test]
    fn resolve_address_abi_encoding_not_classified() {
        let output: Vec<u8> = (false, 0u16, String::new()).abi_encode_params();
        // (bool, uint16, string): bool at [31]=0
        assert_eq!(output[31], 0); // false
    }

    #[test]
    fn resolve_address_abi_encoding_recorded() {
        let translated = "KT1_X".to_string();
        let output: Vec<u8> = (true, RESOLUTION_RECORDED, translated).abi_encode_params();
        assert_eq!(output[31], 1); // true
        assert_eq!(output[63], RESOLUTION_RECORDED as u8);
    }

    #[test]
    fn resolve_address_abi_encoding_derived() {
        let translated = "KT1_derived".to_string();
        let output: Vec<u8> = (true, RESOLUTION_DERIVED, translated).abi_encode_params();
        assert_eq!(output[31], 1); // true
        assert_eq!(output[63], RESOLUTION_DERIVED as u8);
    }

    // ── resolveAddress logic rows — via dispatch_resolve_address ─────────────
    //
    // All tests below call dispatch_resolve_address directly so that the full
    // dispatch logic (including compute_alias) is exercised by the test suite.
    // This approach catches bugs like passing source_runtime instead of
    // target_runtime to compute_alias (the BLOCKER fixed in this MR).

    // Use a realistic gas limit that can be safely converted to milligas
    // (milligas = evm_gas * 100; u64::MAX * 100 overflows, so cap at
    // a value safely below u64::MAX / 100).
    const GAS_LIMIT: u64 = 100_000_000_000;

    /// Same-runtime short-circuit: source == target, valid addr
    /// → (true, Recorded, addr).
    #[test]
    fn resolve_address_same_runtime_short_circuit() {
        let host = MockKernelHost::default();
        let addr = "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".to_string();
        let registry = StubRegistry::with_classification(Classification::Unknown);
        let output = dispatch_resolve_address(
            &host,
            &registry,
            addr.clone(),
            RuntimeId::Ethereum,
            RuntimeId::Ethereum,
            None,
            &mut Gas::new(GAS_LIMIT),
        )
        .unwrap();
        assert_eq!(output[31], 1); // classified = true
        assert_eq!(output[63], RESOLUTION_RECORDED as u8);
    }

    /// Same-runtime short-circuit: source == target, malformed addr
    /// → (false, 0, "").
    #[test]
    fn resolve_address_same_runtime_malformed_addr() {
        let host = MockKernelHost::default();
        let registry = StubRegistry::with_classification(Classification::Unknown);
        let output = dispatch_resolve_address(
            &host,
            &registry,
            "not-a-tz1".to_string(),
            RuntimeId::Tezos,
            RuntimeId::Tezos,
            None,
            &mut Gas::new(GAS_LIMIT),
        )
        .unwrap();
        assert_eq!(output[31], 0); // classified = false
        assert_eq!(output[63], 0); // res = 0
    }

    /// Alias{target, native}: direct lookup → (true, Recorded, native).
    #[test]
    fn resolve_address_alias_direct_lookup() {
        let host = MockKernelHost::default();
        let native_addr = "KT1_NATIVE".to_string();
        let alias_info = AliasInfo {
            runtime: RuntimeId::Tezos,
            native_address: native_addr.as_bytes().to_vec(),
        };
        let registry =
            StubRegistry::with_classification(Classification::Alias(alias_info));

        let output = dispatch_resolve_address(
            &host,
            &registry,
            "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".to_string(),
            RuntimeId::Ethereum,
            RuntimeId::Tezos,
            None,
            &mut Gas::new(GAS_LIMIT),
        )
        .unwrap();
        // classified=true at byte 31, res=RECORDED at byte 63
        assert_eq!(output[31], 1);
        assert_eq!(output[63], RESOLUTION_RECORDED as u8);
        // Decode the string portion: offset is at bytes 64-95, length at
        // bytes 96-127, data starts at 128.
        let str_len = u32::from_be_bytes(output[124..128].try_into().unwrap()) as usize;
        assert_eq!(&output[128..128 + str_len], b"KT1_NATIVE");
    }

    /// Native source → derivation → destination has materialized inverse
    /// → RECORDED.  The StubRegistry asserts that compute_alias is called with
    /// target_runtime (Tezos), which would fail if source_runtime were passed.
    #[test]
    fn resolve_address_native_to_derived_recorded() {
        let host = MockKernelHost::default();
        let source_addr = "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
        let derived_alias = "KT1_DERIVED";

        let destination_classification = Some(Classification::Alias(AliasInfo {
            runtime: RuntimeId::Ethereum,
            native_address: source_addr.as_bytes().to_vec(),
        }));
        // expected_derivation_runtime = Tezos (the target).  If the code
        // passes source_runtime (Ethereum) instead, the assert fires.
        let registry = StubRegistry::with_alias_and_expected_runtime(
            Classification::Native,
            derived_alias,
            destination_classification,
            RuntimeId::Tezos, // expected = target_runtime
        );

        let output = dispatch_resolve_address(
            &host,
            &registry,
            source_addr.to_string(),
            RuntimeId::Ethereum,
            RuntimeId::Tezos,
            None,
            &mut Gas::new(GAS_LIMIT),
        )
        .unwrap();
        assert_eq!(output[31], 1); // classified = true
        assert_eq!(output[63], RESOLUTION_RECORDED as u8);
    }

    /// Native source → derivation → destination has NO inverse → DERIVED.
    #[test]
    fn resolve_address_native_to_derived_not_recorded() {
        let host = MockKernelHost::default();
        let source_addr = "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
        let derived_alias = "KT1_DERIVED";

        let registry = StubRegistry::with_alias_and_expected_runtime(
            Classification::Native,
            derived_alias,
            None, // no destination → DERIVED
            RuntimeId::Tezos,
        );

        let output = dispatch_resolve_address(
            &host,
            &registry,
            source_addr.to_string(),
            RuntimeId::Ethereum,
            RuntimeId::Tezos,
            None,
            &mut Gas::new(GAS_LIMIT),
        )
        .unwrap();
        assert_eq!(output[31], 1); // classified = true
        assert_eq!(output[63], RESOLUTION_DERIVED as u8);
    }

    /// Native source (back-stop result) falls through to the derivation path.
    ///
    /// The real code-presence back-stop is tested in tezosx-ethereum-runtime.
    /// Here the stub cues directly as Native — exercising that dispatch_resolve_address
    /// takes the derivation path for a Native classification.
    #[test]
    fn resolve_address_evm_native_uses_derivation_path() {
        let host = MockKernelHost::default();
        let addr_str = "0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb";
        let registry = StubRegistry::with_alias_and_expected_runtime(
            Classification::Native,
            "KT1_DERIVED",
            None,
            RuntimeId::Tezos,
        );
        let output = dispatch_resolve_address(
            &host,
            &registry,
            addr_str.to_string(),
            RuntimeId::Ethereum,
            RuntimeId::Tezos,
            None,
            &mut Gas::new(GAS_LIMIT),
        )
        .unwrap();
        assert_eq!(output[31], 1); // classified = true
    }

    /// EVM Unknown → (false, 0, "").
    #[test]
    fn resolve_address_evm_unknown() {
        let host = MockKernelHost::default();
        let registry = StubRegistry::with_classification(Classification::Unknown);
        let output = dispatch_resolve_address(
            &host,
            &registry,
            "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".to_string(),
            RuntimeId::Ethereum,
            RuntimeId::Tezos,
            None,
            &mut Gas::new(GAS_LIMIT),
        )
        .unwrap();
        assert_eq!(output[31], 0); // classified = false
    }

    // ── dispatch_origin_of logic rows ─────────────────────────────────────────

    /// originOf: Unknown → (0, 0, "").
    #[test]
    fn dispatch_origin_of_unknown() {
        let host = MockKernelHost::default();
        let registry = StubRegistry::with_classification(Classification::Unknown);
        let output = dispatch_origin_of(
            &host,
            &registry,
            "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx".to_string(),
            RuntimeId::Tezos,
            None,
            &mut Gas::new(GAS_LIMIT),
        )
        .unwrap();
        assert_eq!(output[31], ORIGIN_KIND_UNKNOWN as u8);
    }

    /// originOf: Native → (1, source_runtime, addr).
    #[test]
    fn dispatch_origin_of_native() {
        let host = MockKernelHost::default();
        let registry = StubRegistry::with_classification(Classification::Native);
        let addr = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx".to_string();
        let output = dispatch_origin_of(
            &host,
            &registry,
            addr,
            RuntimeId::Tezos,
            None,
            &mut Gas::new(GAS_LIMIT),
        )
        .unwrap();
        assert_eq!(output[31], ORIGIN_KIND_NATIVE as u8);
        assert_eq!(output[63], RuntimeId::Tezos as u8);
    }

    /// originOf: Alias → (2, home_runtime, native_addr).
    #[test]
    fn dispatch_origin_of_alias() {
        let host = MockKernelHost::default();
        let alias_info = AliasInfo {
            runtime: RuntimeId::Tezos,
            native_address: b"KT1_X".to_vec(),
        };
        let registry =
            StubRegistry::with_classification(Classification::Alias(alias_info));
        let output = dispatch_origin_of(
            &host,
            &registry,
            "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".to_string(),
            RuntimeId::Ethereum,
            None,
            &mut Gas::new(GAS_LIMIT),
        )
        .unwrap();
        assert_eq!(output[31], ORIGIN_KIND_ALIAS as u8);
        assert_eq!(output[63], RuntimeId::Tezos as u8);
    }

    #[test]
    fn dispatch_origin_of_prefers_staged_overlay() {
        // Durable storage would classify this address as Unknown; a
        // staged overlay entry must take precedence, skip the durable
        // read, and charge the lookup cost.
        let host = MockKernelHost::default();
        let registry = StubRegistry::with_classification(Classification::Unknown);
        let staged = Origin::Alias(AliasInfo {
            runtime: RuntimeId::Tezos,
            native_address: b"KT1_STAGED".to_vec(),
        });
        let mut gas = Gas::new(GAS_LIMIT);
        let output = dispatch_origin_of(
            &host,
            &registry,
            "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".to_string(),
            RuntimeId::Ethereum,
            Some(staged),
            &mut gas,
        )
        .unwrap();
        assert_eq!(output[31], ORIGIN_KIND_ALIAS as u8);
        assert_eq!(output[63], RuntimeId::Tezos as u8);
        assert_eq!(gas.spent(), ALIAS_LOOKUP_COST);
    }

    // ── ABI selector decode/encode round-trips ───────────────────────────────

    #[test]
    fn resolve_address_call_abi_round_trip() {
        let call = RuntimeGateway::resolveAddressCall {
            addr: "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".to_string(),
            sourceRuntime: RuntimeId::Ethereum as u8,
            targetRuntime: RuntimeId::Tezos as u8,
        };
        let encoded = call.abi_encode();
        let decoded = RuntimeGatewayCalls::abi_decode(&encoded).unwrap();
        match decoded {
            RuntimeGatewayCalls::resolveAddress(c) => {
                assert_eq!(c.addr, "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
                assert_eq!(c.sourceRuntime, RuntimeId::Ethereum as u8);
                assert_eq!(c.targetRuntime, RuntimeId::Tezos as u8);
            }
            _ => panic!("expected resolveAddress variant"),
        }
    }

    #[test]
    fn origin_of_call_abi_round_trip() {
        let call = RuntimeGateway::originOfCall {
            addr: "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx".to_string(),
            sourceRuntime: RuntimeId::Tezos as u8,
        };
        let encoded = call.abi_encode();
        let decoded = RuntimeGatewayCalls::abi_decode(&encoded).unwrap();
        match decoded {
            RuntimeGatewayCalls::originOf(c) => {
                assert_eq!(c.addr, "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx");
                assert_eq!(c.sourceRuntime, RuntimeId::Tezos as u8);
            }
            _ => panic!("expected originOf variant"),
        }
    }

    #[test]
    fn invalid_runtime_id_payload_shape() {
        use alloy_sol_types::SolError;
        let payload = RuntimeGateway::InvalidRuntimeId { received: 9u8 }.abi_encode();
        // 4-byte selector + 32-byte uint256
        assert_eq!(payload.len(), 36);
        assert_eq!(&payload[0..4], &RuntimeGateway::InvalidRuntimeId::SELECTOR,);
        // received = 9 in the low byte of the uint256
        assert_eq!(payload[35], 9);
    }

    #[test]
    fn test_build_http_request_get() {
        let request = build_http_request(
            "http://michelson/KT1abc/transfer",
            &[(
                "Content-Type".to_string(),
                "application/micheline".to_string(),
            )],
            &[],
            0, // GET
            &mut Gas::new(u64::MAX),
        )
        .unwrap();

        assert_eq!(request.method(), http::Method::GET);
        assert_eq!(request.uri(), "http://michelson/KT1abc/transfer");
        assert_eq!(
            request.headers().get("Content-Type").unwrap(),
            "application/micheline"
        );
        assert!(request.body().is_empty());
    }

    #[test]
    fn test_build_http_request_post_with_body() {
        let body = vec![0x01, 0x02, 0x03];
        let request = build_http_request(
            "http://michelson/KT1abc/transfer",
            &[],
            &body,
            1, // POST
            &mut Gas::new(u64::MAX),
        )
        .unwrap();

        assert_eq!(request.method(), http::Method::POST);
        assert_eq!(request.body(), &body);
    }

    #[test]
    fn test_build_http_request_multiple_headers() {
        let headers = vec![
            (
                "Content-Type".to_string(),
                "application/micheline".to_string(),
            ),
            ("X-Custom".to_string(), "some-value".to_string()),
        ];
        let request = build_http_request(
            "http://michelson/KT1abc",
            &headers,
            &[],
            1,
            &mut Gas::new(u64::MAX),
        )
        .unwrap();

        assert_eq!(
            request.headers().get("Content-Type").unwrap(),
            "application/micheline"
        );
        assert_eq!(request.headers().get("X-Custom").unwrap(), "some-value");
    }

    #[test]
    fn test_build_http_request_unsupported_method() {
        let result = build_http_request(
            "http://michelson/KT1abc",
            &[],
            &[],
            42,
            &mut Gas::new(u64::MAX),
        );
        assert!(matches!(
            result,
            Err(CustomPrecompileError::Revert(msg, _)) if msg.contains("unsupported HTTP method")
        ));
    }

    #[test]
    fn test_call_abi_decode() {
        use alloy_sol_types::SolCall;

        let call = RuntimeGateway::callCall {
            url: "http://michelson/KT1abc/transfer".to_string(),
            headers: vec![(
                "Content-Type".to_string(),
                "application/micheline".to_string(),
            )],
            body: vec![0x01, 0x02].into(),
            method: 1,
        };
        let encoded = call.abi_encode();

        // Decode via the dispatcher
        let decoded = RuntimeGatewayCalls::abi_decode(&encoded).unwrap();
        match decoded {
            RuntimeGatewayCalls::call(decoded_call) => {
                assert_eq!(decoded_call.url, "http://michelson/KT1abc/transfer");
                assert_eq!(decoded_call.headers.len(), 1);
                assert_eq!(decoded_call.headers[0].0, "Content-Type");
                assert_eq!(decoded_call.headers[0].1, "application/micheline");
                assert_eq!(decoded_call.body.as_ref(), &[0x01, 0x02]);
                assert_eq!(decoded_call.method, 1);
            }
            _ => panic!("expected call variant"),
        }
    }

    #[test]
    fn test_call_return_encoding() {
        let output: Vec<u8> = (true, Vec::<u8>::new()).abi_encode_params();
        // ABI encoding of (bool true, bytes empty):
        // 32 bytes: bool (padded, 1)
        // 32 bytes: offset to bytes (0x40 = 64)
        // 32 bytes: length of bytes (0)
        assert_eq!(output.len(), 96);
        // bool = true at byte 31
        assert_eq!(output[31], 1);
    }

    #[test]
    fn test_inject_tezos_headers() {
        let mut request = build_http_request(
            "http://tezos/KT1abc/transfer",
            &[(
                "Content-Type".to_string(),
                "application/micheline".to_string(),
            )],
            &[0xCA, 0xFE],
            1,
            &mut Gas::new(u64::MAX),
        )
        .unwrap();

        let sender_alias =
            Contract::from_b58check("KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2").unwrap();
        let source_alias =
            Contract::from_b58check("KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2").unwrap();
        // 42 TEZ in wei
        let amount = U256::from(42u64) * U256::from(10u64).pow(U256::from(18u64));
        let gas_limit = 100_000u64;
        let timestamp = U256::from(1_700_000_000u64);
        let block_number = U256::from(12345u64);

        inject_tezos_headers(
            request.headers_mut(),
            &sender_alias.to_b58check(),
            &source_alias.to_b58check(),
            RuntimeId::Tezos,
            amount,
            gas_limit,
            timestamp,
            block_number,
            "test-crac-id",
            0,
            Gas::new(u64::MAX),
        )
        .unwrap();

        // User header is preserved
        assert_eq!(
            request.headers().get("Content-Type").unwrap(),
            "application/micheline"
        );
        // Body is preserved
        assert_eq!(request.body(), &[0xCA, 0xFE]);

        // X-Tezos headers are injected with UTF-8 alias strings
        assert_eq!(
            request
                .headers()
                .get(X_TEZOS_SENDER)
                .unwrap()
                .to_str()
                .unwrap(),
            "KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2"
        );
        assert_eq!(
            request
                .headers()
                .get(X_TEZOS_SOURCE)
                .unwrap()
                .to_str()
                .unwrap(),
            "KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2"
        );
        assert_eq!(
            request
                .headers()
                .get(X_TEZOS_AMOUNT)
                .unwrap()
                .to_str()
                .unwrap(),
            "42"
        );
        assert_eq!(
            request
                .headers()
                .get(X_TEZOS_GAS_LIMIT)
                .unwrap()
                .to_str()
                .unwrap(),
            "100000"
        );
        assert_eq!(
            request
                .headers()
                .get(X_TEZOS_TIMESTAMP)
                .unwrap()
                .to_str()
                .unwrap(),
            "1700000000"
        );
        assert_eq!(
            request
                .headers()
                .get(X_TEZOS_BLOCK_NUMBER)
                .unwrap()
                .to_str()
                .unwrap(),
            "12345"
        );
        assert_eq!(
            request
                .headers()
                .get(X_TEZOS_CRAC_ID)
                .unwrap()
                .to_str()
                .unwrap(),
            "test-crac-id"
        );
        assert_eq!(
            request
                .headers()
                .get(X_TEZOS_SOURCE_RUNTIME)
                .unwrap()
                .to_str()
                .unwrap(),
            u8::from(RuntimeId::Tezos).to_string()
        );
    }

    /// Non-zero `crac_depth` materialises on the outgoing header.
    /// Exercises the gateway's contribution to the `X-Tezos-CRAC-Depth`
    /// propagation chain: the receiving runtime parses this back into
    /// `TransactionOrigin::call_depth` and seeds REVM's first frame
    /// from it.
    #[test]
    fn test_inject_tezos_headers_writes_nonzero_crac_depth() {
        let mut request = build_http_request(
            "http://tezos/KT1abc/transfer",
            &[],
            &[],
            1,
            &mut Gas::new(u64::MAX),
        )
        .unwrap();
        let alias = "KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2";
        inject_tezos_headers(
            request.headers_mut(),
            alias,
            alias,
            RuntimeId::Tezos,
            U256::ZERO,
            0,
            U256::ZERO,
            U256::ZERO,
            "test-crac-id",
            42,
            Gas::new(u64::MAX),
        )
        .unwrap();
        assert_eq!(
            request
                .headers()
                .get(X_TEZOS_CRAC_DEPTH)
                .unwrap()
                .to_str()
                .unwrap(),
            "42",
        );
    }

    #[test]
    fn test_build_http_request_rejects_user_supplied_x_tezos_headers() {
        // User-supplied X-Tezos-* headers are forbidden to prevent
        // forgery of trusted execution context.
        let result = build_http_request(
            "http://tezos/KT1abc",
            &[("X-Tezos-Sender".to_string(), "0xevil".to_string())],
            &[],
            0,
            &mut Gas::new(u64::MAX),
        );
        assert!(matches!(
            result,
            Err(CustomPrecompileError::Revert(msg, _)) if msg.contains("X-Tezos-* headers are forbidden")
        ));
    }

    #[test]
    fn test_build_http_request_rejects_x_tezos_case_insensitive() {
        let result = build_http_request(
            "http://tezos/KT1abc",
            &[("x-tezos-amount".to_string(), "999".to_string())],
            &[],
            0,
            &mut Gas::new(u64::MAX),
        );
        assert!(matches!(
            result,
            Err(CustomPrecompileError::Revert(msg, _)) if msg.contains("X-Tezos-* headers are forbidden")
        ));
    }

    // --- inject_tezos_headers: amount edge cases ---

    #[test]
    fn test_inject_tezos_headers_zero_amount() {
        let mut request = build_http_request(
            "http://tezos/KT1abc",
            &[],
            &[],
            1,
            &mut Gas::new(u64::MAX),
        )
        .unwrap();

        let sender_alias =
            Contract::from_b58check("KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2").unwrap();
        let source_alias =
            Contract::from_b58check("KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2").unwrap();

        inject_tezos_headers(
            request.headers_mut(),
            &sender_alias.to_b58check(),
            &source_alias.to_b58check(),
            RuntimeId::Tezos,
            U256::ZERO,
            100_000,
            U256::from(1_700_000_000u64),
            U256::from(1u64),
            "test-crac-id",
            0,
            Gas::new(u64::MAX),
        )
        .unwrap();

        assert_eq!(
            request
                .headers()
                .get(X_TEZOS_AMOUNT)
                .unwrap()
                .to_str()
                .unwrap(),
            "0"
        );
    }

    #[test]
    fn test_inject_tezos_headers_fractional_amount() {
        let mut request = build_http_request(
            "http://tezos/KT1abc",
            &[],
            &[],
            1,
            &mut Gas::new(u64::MAX),
        )
        .unwrap();

        let sender_alias =
            Contract::from_b58check("KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2").unwrap();
        let source_alias =
            Contract::from_b58check("KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2").unwrap();
        // 0.5 TEZ in wei
        let amount = U256::from(5u64) * U256::from(10u64).pow(U256::from(17u64));

        inject_tezos_headers(
            request.headers_mut(),
            &sender_alias.to_b58check(),
            &source_alias.to_b58check(),
            RuntimeId::Tezos,
            amount,
            100_000,
            U256::from(1_700_000_000u64),
            U256::from(1u64),
            "test-crac-id",
            0,
            Gas::new(u64::MAX),
        )
        .unwrap();

        assert_eq!(
            request
                .headers()
                .get(X_TEZOS_AMOUNT)
                .unwrap()
                .to_str()
                .unwrap(),
            "0.5"
        );
    }

    #[test]
    fn test_inject_tezos_headers_zero_gas_and_block() {
        let mut request = build_http_request(
            "http://tezos/KT1abc",
            &[],
            &[],
            1,
            &mut Gas::new(u64::MAX),
        )
        .unwrap();

        let sender_alias =
            Contract::from_b58check("KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2").unwrap();
        let source_alias =
            Contract::from_b58check("KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2").unwrap();

        inject_tezos_headers(
            request.headers_mut(),
            &sender_alias.to_b58check(),
            &source_alias.to_b58check(),
            RuntimeId::Tezos,
            U256::ZERO,
            0,
            U256::ZERO,
            U256::ZERO,
            "test-crac-id",
            0,
            Gas::new(u64::MAX),
        )
        .unwrap();

        assert_eq!(
            request
                .headers()
                .get(X_TEZOS_GAS_LIMIT)
                .unwrap()
                .to_str()
                .unwrap(),
            "0"
        );
        assert_eq!(
            request
                .headers()
                .get(X_TEZOS_TIMESTAMP)
                .unwrap()
                .to_str()
                .unwrap(),
            "0"
        );
        assert_eq!(
            request
                .headers()
                .get(X_TEZOS_BLOCK_NUMBER)
                .unwrap()
                .to_str()
                .unwrap(),
            "0"
        );
    }

    // --- build_http_request edge cases ---

    #[test]
    fn test_build_http_request_empty_body() {
        let request = build_http_request(
            "http://tezos/KT1abc",
            &[],
            &[],
            1,
            &mut Gas::new(u64::MAX),
        )
        .unwrap();
        assert!(request.body().is_empty());
        assert_eq!(request.method(), http::Method::POST);
    }

    #[test]
    fn test_build_http_request_preserves_large_body() {
        let body = vec![0xAB; 1024];
        let request = build_http_request(
            "http://tezos/KT1abc",
            &[],
            &body,
            1,
            &mut Gas::new(u64::MAX),
        )
        .unwrap();
        assert_eq!(request.body().len(), 1024);
        assert!(request.body().iter().all(|&b| b == 0xAB));
    }

    #[test]
    fn test_build_http_request_rejects_x_tezos_mixed_case() {
        let result = build_http_request(
            "http://tezos/KT1abc",
            &[("X-TEZOS-Sender".to_string(), "bad".to_string())],
            &[],
            1,
            &mut Gas::new(u64::MAX),
        );
        assert!(matches!(
            result,
            Err(CustomPrecompileError::Revert(msg, _)) if msg.contains("X-Tezos-* headers are forbidden")
        ));
    }

    // --- ABI decoding edge cases ---

    #[test]
    fn test_call_michelson_abi_decode() {
        use alloy_sol_types::SolCall;

        let call = RuntimeGateway::callMichelsonCall {
            destination: "KT1abc".to_string(),
            entrypoint: "transfer".to_string(),
            parameters: vec![0x01, 0x02].into(),
        };
        let encoded = call.abi_encode();

        let decoded = RuntimeGatewayCalls::abi_decode(&encoded).unwrap();
        match decoded {
            RuntimeGatewayCalls::callMichelson(decoded_call) => {
                assert_eq!(decoded_call.destination, "KT1abc");
                assert_eq!(decoded_call.entrypoint, "transfer");
                assert_eq!(decoded_call.parameters.as_ref(), &[0x01, 0x02]);
            }
            _ => panic!("expected callMichelson variant"),
        }
    }

    #[test]
    fn test_call_michelson_view_abi_decode() {
        use alloy_sol_types::SolCall;

        let call = RuntimeGateway::callMichelsonViewCall {
            destination: "KT1abc".to_string(),
            viewName: "get_balance".to_string(),
            input: vec![0xAA, 0xBB].into(),
        };
        let encoded = call.abi_encode();

        let decoded = RuntimeGatewayCalls::abi_decode(&encoded).unwrap();
        match decoded {
            RuntimeGatewayCalls::callMichelsonView(decoded_call) => {
                assert_eq!(decoded_call.destination, "KT1abc");
                assert_eq!(decoded_call.viewName, "get_balance");
                assert_eq!(decoded_call.input.as_ref(), &[0xAA, 0xBB]);
            }
            _ => panic!("expected callMichelsonView variant"),
        }
    }

    #[test]
    fn test_call_return_encoding_with_body() {
        let body = vec![0xCA, 0xFE, 0xBA, 0xBE];
        let output: Vec<u8> = (true, body).abi_encode_params();
        // bool (true) + offset to bytes + length of bytes + padded bytes
        assert!(output.len() >= 128);
        assert_eq!(output[31], 1); // bool = true
    }

    #[test]
    fn test_inject_tezos_headers_overwrites_all_existing() {
        let mut request = build_http_request(
            "http://tezos/KT1abc",
            &[],
            &[],
            1,
            &mut Gas::new(u64::MAX),
        )
        .unwrap();

        // Pre-populate with old values
        request
            .headers_mut()
            .insert(X_TEZOS_SENDER, "old-sender".parse().unwrap());
        request
            .headers_mut()
            .insert(X_TEZOS_SOURCE, "old-source".parse().unwrap());
        request
            .headers_mut()
            .insert(X_TEZOS_AMOUNT, "old-amount".parse().unwrap());
        request
            .headers_mut()
            .insert(X_TEZOS_GAS_LIMIT, "old-gas".parse().unwrap());
        request
            .headers_mut()
            .insert(X_TEZOS_TIMESTAMP, "old-ts".parse().unwrap());
        request
            .headers_mut()
            .insert(X_TEZOS_BLOCK_NUMBER, "old-block".parse().unwrap());

        let sender_alias =
            Contract::from_b58check("KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2").unwrap();

        inject_tezos_headers(
            request.headers_mut(),
            &sender_alias.to_b58check(),
            &sender_alias.to_b58check(),
            RuntimeId::Tezos,
            // 1 TEZ in wei
            U256::from(10u64).pow(U256::from(18u64)),
            50_000,
            U256::from(100u64),
            U256::from(42u64),
            "test-crac-id",
            0,
            Gas::new(u64::MAX),
        )
        .unwrap();

        // All values should be overwritten
        assert_ne!(
            request
                .headers()
                .get(X_TEZOS_SENDER)
                .unwrap()
                .to_str()
                .unwrap(),
            "old-sender"
        );
        assert_ne!(
            request
                .headers()
                .get(X_TEZOS_SOURCE)
                .unwrap()
                .to_str()
                .unwrap(),
            "old-source"
        );
        assert_eq!(
            request
                .headers()
                .get(X_TEZOS_AMOUNT)
                .unwrap()
                .to_str()
                .unwrap(),
            "1"
        );
        assert_eq!(
            request
                .headers()
                .get(X_TEZOS_GAS_LIMIT)
                .unwrap()
                .to_str()
                .unwrap(),
            "50000"
        );
        assert_eq!(
            request
                .headers()
                .get(X_TEZOS_TIMESTAMP)
                .unwrap()
                .to_str()
                .unwrap(),
            "100"
        );
        assert_eq!(
            request
                .headers()
                .get(X_TEZOS_BLOCK_NUMBER)
                .unwrap()
                .to_str()
                .unwrap(),
            "42"
        );
    }

    #[test]
    fn test_charge_delegated_storage_cost_converts_mutez_to_wei() {
        // 1 mutez = 10^12 wei; base_fee = 1 GWei = 10^9 wei/gas.
        // g2 = ceil(1 * 10^12 / 10^9) = 1000. Guards against the
        // mutez-as-wei unit bug, which would charge 1 / 10^9 = 0.
        let gas_limit = 1_000_000u64;
        let mut gas = Gas::new(gas_limit);
        charge_delegated_storage_cost(&mut gas, Some(1), 1_000_000_000)
            .expect("should succeed");
        assert_eq!(
            gas.remaining(),
            gas_limit - 1000,
            "1 mutez at 1 GWei base_fee must deduct 1000 gas"
        );
    }

    #[test]
    fn test_charge_delegated_storage_cost_none_no_charge() {
        // No delegation → no charge.
        let gas_limit = 1_000_000u64;
        let mut gas = Gas::new(gas_limit);
        charge_delegated_storage_cost(&mut gas, None, 100).expect("should succeed");
        assert_eq!(
            gas.remaining(),
            gas_limit,
            "no delegation must not deduct any gas"
        );
    }

    #[test]
    fn test_charge_delegated_storage_cost_some_zero_no_charge() {
        // cost = 0 → no storage charge.
        let gas_limit = 1_000_000u64;
        let mut gas = Gas::new(gas_limit);
        charge_delegated_storage_cost(&mut gas, Some(0), 100).expect("should succeed");
        assert_eq!(
            gas.remaining(),
            gas_limit,
            "zero storage cost must not deduct any gas"
        );
    }

    #[test]
    fn test_charge_delegated_storage_cost_exact_division() {
        // cost_wei = 10^12, base_fee = 10^12 → exactly 1, no rounding.
        let gas_limit = 1_000_000u64;
        let mut gas = Gas::new(gas_limit);
        charge_delegated_storage_cost(&mut gas, Some(1), 1_000_000_000_000)
            .expect("should succeed");
        assert_eq!(
            gas.remaining(),
            gas_limit - 1,
            "an exact division must deduct exactly the quotient"
        );
    }

    #[test]
    fn test_charge_delegated_storage_cost_rounds_up() {
        // cost_wei = 3 * 10^12, base_fee = 2 * 10^12 → 1.5, ceil → 2.
        let gas_limit = 1_000_000u64;
        let mut gas = Gas::new(gas_limit);
        charge_delegated_storage_cost(&mut gas, Some(3), 2_000_000_000_000)
            .expect("should succeed");
        assert_eq!(
            gas.remaining(),
            gas_limit - 2,
            "a non-exact division must round the gas charge up"
        );
    }

    #[test]
    fn test_charge_delegated_storage_cost_sub_base_fee_rounds_up_to_one() {
        // cost_wei = 10^12 < base_fee = 3 * 10^12 → ceil(1/3) = 1.
        // Under ceil, any non-zero delegated cost charges at least 1 gas.
        let gas_limit = 1_000_000u64;
        let mut gas = Gas::new(gas_limit);
        charge_delegated_storage_cost(&mut gas, Some(1), 3_000_000_000_000)
            .expect("should succeed");
        assert_eq!(
            gas.remaining(),
            gas_limit - 1,
            "a non-zero cost below one gas-unit must round up to 1 gas"
        );
    }

    #[test]
    fn test_charge_delegated_storage_cost_reverts_on_zero_base_fee_with_cost() {
        // cost > 0 with base_fee == 0 must revert explicitly.
        let mut gas = Gas::new(1_000_000);
        let result = charge_delegated_storage_cost(&mut gas, Some(42), 0);
        assert!(
            matches!(
                result,
                Err(CustomPrecompileError::Revert(ref msg, _))
                if msg.contains("storage cost")
                    && msg.contains("base_fee_per_gas is zero")
            ),
            "expected Revert with base-fee-zero message, got: {result:?}"
        );
    }

    #[test]
    fn test_charge_delegated_storage_cost_zero_cost_with_zero_base_fee_no_revert() {
        // cost == 0 + base_fee == 0 → division skipped, no revert, no charge.
        let gas_limit = 1_000_000u64;
        let mut gas = Gas::new(gas_limit);
        charge_delegated_storage_cost(&mut gas, Some(0), 0)
            .expect("zero cost with zero base_fee must not revert");
        assert_eq!(
            gas.remaining(),
            gas_limit,
            "zero storage cost must not deduct any gas even when base_fee is zero"
        );
    }

    #[test]
    fn test_charge_delegated_storage_cost_none_with_zero_base_fee_no_revert() {
        // None + base_fee == 0 → division skipped, no revert, no charge.
        let gas_limit = 1_000_000u64;
        let mut gas = Gas::new(gas_limit);
        charge_delegated_storage_cost(&mut gas, None, 0)
            .expect("no delegation with zero base_fee must not revert");
        assert_eq!(
            gas.remaining(),
            gas_limit,
            "no delegation must not deduct any gas even when base_fee is zero"
        );
    }

    #[test]
    fn test_charge_delegated_storage_cost_overflow_is_out_of_gas() {
        // cost_wei = 20_000_000 * 10^12 = 2e19 > u64::MAX at base_fee 1 →
        // g2 overflows u64, so the cost is unaffordable: OutOfGas.
        let mut gas = Gas::new(u64::MAX);
        let result = charge_delegated_storage_cost(&mut gas, Some(20_000_000), 1);
        assert!(
            matches!(result, Err(CustomPrecompileError::OutOfGas)),
            "a gas equivalent overflowing u64 must surface as OutOfGas, got: {result:?}"
        );
    }

    // --- Integration: the storage-cost header wired through
    // `classify_and_charge_crac_response` on the 2xx CRAC return path. ---

    #[test]
    fn test_classify_and_charge_crac_response_charges_storage_cost_header() {
        // 2xx + `X-Tezos-Storage-Cost: 1` mutez at base_fee = 1 GWei →
        // g2 = ceil(1 * 10^12 / 10^9) = 1000. Compare the gas remaining
        // with vs without the header: the difference is exactly the
        // storage charge, isolating it from the callee-gas charge. This
        // is the wiring guard — it fails if the helper call is dropped
        // from the success branch.
        let base_fee = 1_000_000_000u64;
        let gas_limit = 1_000_000u64;
        let build = |with_storage_cost: bool| {
            let mut builder = http::Response::builder()
                .status(http::status::StatusCode::OK)
                .header(X_TEZOS_GAS_CONSUMED, "1000");
            if with_storage_cost {
                builder = builder.header(X_TEZOS_STORAGE_COST, "1");
            }
            builder.body(vec![]).unwrap()
        };

        let mut gas_with = Gas::new(gas_limit);
        classify_and_charge_crac_response(
            build(true),
            RuntimeId::Tezos,
            &mut gas_with,
            base_fee,
        )
        .expect("should succeed");
        let mut gas_without = Gas::new(gas_limit);
        classify_and_charge_crac_response(
            build(false),
            RuntimeId::Tezos,
            &mut gas_without,
            base_fee,
        )
        .expect("should succeed");

        assert_eq!(
            gas_without.remaining() - gas_with.remaining(),
            1000,
            "the storage-cost header must deduct g2 = ceil(1 mutez / 1 GWei) = 1000 gas"
        );
    }

    #[test]
    fn test_classify_and_charge_crac_response_no_storage_cost_header() {
        // 2xx without `X-Tezos-Storage-Cost` → only the callee gas is charged.
        // 1000 milligas = ceil(1000 / 22) = 46 EVM gas (rounded up, L2-1751).
        let response = http::Response::builder()
            .status(http::status::StatusCode::OK)
            .header(X_TEZOS_GAS_CONSUMED, "1000")
            .body(vec![])
            .unwrap();
        let gas_limit = 1_000_000u64;
        let mut gas = Gas::new(gas_limit);
        classify_and_charge_crac_response(
            response,
            RuntimeId::Tezos,
            &mut gas,
            1_000_000_000,
        )
        .expect("should succeed");
        assert_eq!(
            gas.remaining(),
            gas_limit - 46,
            "an absent storage-cost header must not deduct any storage gas"
        );
    }

    #[test]
    fn test_classify_and_charge_crac_response_rounds_consumed_up() {
        // L2-1751: 944 milligas = 22 * 42 + 20 must charge 43 EVM gas (ceil),
        // not the floored 42 — the sub-22 remainder is no longer uncharged.
        let response = http::Response::builder()
            .status(http::status::StatusCode::OK)
            .header(X_TEZOS_GAS_CONSUMED, "944")
            .body(vec![])
            .unwrap();
        let gas_limit = 1_000_000u64;
        let mut gas = Gas::new(gas_limit);
        classify_and_charge_crac_response(
            response,
            RuntimeId::Tezos,
            &mut gas,
            1_000_000_000,
        )
        .expect("should succeed");
        assert_eq!(gas.remaining(), gas_limit - 43);
    }

    #[test]
    fn charge_consumed_gas_rounds_up() {
        // L2-1751: the consumed-gas charge-back shared by the alias-resolution
        // and originOf/resolveAddress paths rounds up. 944 milligas =
        // 22 * 42 + 20 must charge ceil = 43 EVM gas, not the floored 42.
        let mut gas = Gas::new(1_000_000);
        charge_consumed_gas(&mut gas, RuntimeId::Tezos, 944).expect("affordable");
        assert_eq!(gas.spent(), 43);
    }

    #[test]
    fn charge_consumed_gas_zero_no_charge() {
        // A cache hit / round-trip reports 0 consumed gas: no charge.
        let mut gas = Gas::new(1_000_000);
        charge_consumed_gas(&mut gas, RuntimeId::Tezos, 0).expect("affordable");
        assert_eq!(gas.spent(), 0);
    }

    #[test]
    fn test_classify_and_charge_crac_response_storage_cost_out_of_gas() {
        // g2 = 1000 but only 490 gas remains after the callee charge →
        // the storage charge fails with OutOfGas and the call reverts.
        let response = http::Response::builder()
            .status(http::status::StatusCode::OK)
            .header(X_TEZOS_GAS_CONSUMED, "1000")
            .header(X_TEZOS_STORAGE_COST, "1")
            .body(vec![])
            .unwrap();
        let mut gas = Gas::new(500);
        let result = classify_and_charge_crac_response(
            response,
            RuntimeId::Tezos,
            &mut gas,
            1_000_000_000,
        );
        assert!(
            matches!(result, Err(CustomPrecompileError::OutOfGas)),
            "insufficient gas to cover the storage cost must fail with OutOfGas, got: {result:?}"
        );
    }

    #[test]
    fn test_classify_and_charge_crac_response_malformed_storage_cost_header() {
        // 2xx + malformed `X-Tezos-Storage-Cost` → parse-error revert.
        let response = http::Response::builder()
            .status(http::status::StatusCode::OK)
            .header(X_TEZOS_GAS_CONSUMED, "1000")
            .header(X_TEZOS_STORAGE_COST, "not-a-number")
            .body(vec![])
            .unwrap();
        let mut gas = Gas::new(1_000_000);
        let result = classify_and_charge_crac_response(
            response,
            RuntimeId::Tezos,
            &mut gas,
            1_000_000_000,
        );
        assert!(
            matches!(
                result,
                Err(CustomPrecompileError::Revert(ref msg, _))
                    if msg.contains(X_TEZOS_STORAGE_COST)
            ),
            "a malformed storage-cost header must revert with a parse error, got: {result:?}"
        );
    }

    #[test]
    fn test_classify_and_charge_crac_response_storage_cost_ignored_on_non_2xx() {
        // A non-2xx response carrying the header must NOT trigger storage
        // parsing/charge; it follows the normal client-error path. The
        // header is meaningful only on a 2xx response.
        let response = http::Response::builder()
            .status(http::status::StatusCode::BAD_REQUEST)
            .header(X_TEZOS_STORAGE_COST, "1")
            .body(b"boom".to_vec())
            .unwrap();
        let mut gas = Gas::new(1_000_000);
        let result = classify_and_charge_crac_response(
            response,
            RuntimeId::Tezos,
            &mut gas,
            1_000_000_000,
        );
        assert!(
            matches!(
                result,
                Err(CustomPrecompileError::Revert(ref msg, _))
                    if !msg.contains(X_TEZOS_STORAGE_COST)
            ),
            "the header on a non-2xx response must be ignored, got: {result:?}"
        );
    }

    // ── classify_and_charge_crac_response: 4xx body charging ─────────────────

    /// A 400 response body is charged at the per-word rate before the Revert
    /// is returned.  The charge equals `ceil(body_len / 32) * RUNTIME_GATEWAY_PER_WORD_COST`.
    #[test]
    fn crac_response_4xx_charges_body_per_word() {
        // 64-byte body = 2 EVM words.
        let body = "x".repeat(64);
        let expected_words = 64u64.div_ceil(32);
        let expected_charge = RUNTIME_GATEWAY_PER_WORD_COST * expected_words;

        let response = http::Response::builder()
            .status(http::StatusCode::BAD_REQUEST)
            .header(X_TEZOS_GAS_CONSUMED, "0")
            .body(body.as_bytes().to_vec())
            .unwrap();

        let budget = expected_charge + 1_000_000;
        let mut gas = Gas::new(budget);
        let result = super::classify_and_charge_crac_response(
            response,
            RuntimeId::Tezos,
            &mut gas,
            1_000_000_000,
        );

        // Must be a catchable Revert, not a block abort.
        assert!(
            matches!(result, Err(CustomPrecompileError::Revert(_, _))),
            "expected Revert for 400, got: {result:?}"
        );

        // Gas spent = per-word charge on the body (callee_gas = 0 here).
        let spent = budget - gas.remaining();
        assert_eq!(
            spent, expected_charge,
            "expected {expected_charge} milligas for {expected_words} words, got {spent}"
        );
    }

    /// When the budget is exhausted while charging the 4xx body, the Revert
    /// is still produced (OOG does not become a block abort).
    #[test]
    fn crac_response_4xx_oog_produces_revert_not_abort() {
        // Large body that will exhaust any modest budget.
        let body = "x".repeat(1024);
        let response = http::Response::builder()
            .status(http::StatusCode::BAD_REQUEST)
            .header(X_TEZOS_GAS_CONSUMED, "0")
            .body(body.as_bytes().to_vec())
            .unwrap();

        // Very tight budget — insufficient for the per-word body charge.
        let mut gas = Gas::new(1);
        let result = super::classify_and_charge_crac_response(
            response,
            RuntimeId::Tezos,
            &mut gas,
            1_000_000_000,
        );

        // Must remain a Revert, not a block-abort Abort.
        assert!(
            matches!(result, Err(CustomPrecompileError::Revert(_, _))),
            "OOG on 4xx body must produce Revert, not Abort; got: {result:?}"
        );
    }
}

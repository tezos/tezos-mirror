use alloy_sol_types::{sol, SolInterface, SolValue};
use evm_types::{CustomPrecompileAbort, CustomPrecompileError};
use http::header::HeaderMap;
use revm::{
    context::{Block, ContextTr, JournalTr, Transaction},
    context_interface::journaled_state::account::JournaledAccountTr,
    interpreter::{CallInputs, Gas, InstructionResult, InterpreterResult},
    primitives::{alloy_primitives::IntoLogData, Address, Bytes, Log, U256},
};
use tezosx_interfaces::headers::format_tez_from_wei;
use tezosx_interfaces::{
    gas, RuntimeId, ERR_FORBIDDEN_TEZOS_HEADER, X_TEZOS_AMOUNT, X_TEZOS_BLOCK_NUMBER,
    X_TEZOS_CRAC_ID, X_TEZOS_GAS_CONSUMED, X_TEZOS_GAS_LIMIT, X_TEZOS_SENDER,
    X_TEZOS_SOURCE, X_TEZOS_TIMESTAMP,
};

use crate::{
    database::EtherlinkVMDB,
    helpers::legacy::alloy_to_u256,
    journal::{CrossRuntimeCall, Journal},
    precompiles::{
        constants::{
            ALIAS_LOOKUP_COST, HEADER_VALIDATION_PER_HEADER, RUNTIME_GATEWAY_BASE_COST,
            RUNTIME_GATEWAY_PER_WORD_COST, RUNTIME_GATEWAY_PRECOMPILE_ADDRESS,
            VALUE_TRANSFER_SURCHARGE,
        },
        guard::charge,
        runtime_gateway::RuntimeGateway::RuntimeGatewayCalls,
    },
};
use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_interfaces::Registry;
use tezosx_journal::OriginalSource;

sol! {
    contract RuntimeGateway {
        function transfer(
            string implicitAddress,
        ) external;

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
    }

    /// Emitted on every outgoing CRAC (EVM -> other runtime).
    /// `cracId` allows indexers to correlate operations across
    /// derived blocks.
    event CracSent(
        string cracId,
        string targetRuntime,
        string targetAddress,
        uint256 amount
    );
}

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
/// Gas is charged on a best-effort basis for all statuses, on block
/// aborts the block is reverted anyway, so a missing header is harmless.
/// On 2xx the header is mandatory.
///
/// - 2xx/4xx: charge callee gas, then:
///   - out of gas from charging: return `Ok(None)`.
///   - 2xx: return `Ok(Some(body))`.
///   - 4xx (incl. 429 OOG): catchable revert.
/// - anything else: block abort.
fn classify_and_charge_crac_response(
    response: http::Response<Vec<u8>>,
    target_runtime: RuntimeId,
    gas: &mut Gas,
) -> Result<Vec<u8>, CustomPrecompileError> {
    let callee_gas = response
        .headers()
        .get(X_TEZOS_GAS_CONSUMED)
        .and_then(|v| v.to_str().ok())
        .and_then(|s| s.parse::<u64>().ok())
        .and_then(|c| gas::convert(target_runtime, RuntimeId::Ethereum, c));

    if let Some(evm_consumed) = callee_gas {
        charge(gas, evm_consumed)?;
    }

    if response.status().is_success() {
        if callee_gas.is_none() {
            return Err(CustomPrecompileError::Revert(
                "X-Tezos-Gas-Consumed header missing or invalid in CRAC response".into(),
                *gas,
            ));
        }
        Ok(response.into_body())
    } else if response.status().is_client_error() {
        Err(CustomPrecompileError::Revert(
            format!(
                "Cross-runtime call failed with status {}: {}",
                response.status(),
                String::from_utf8_lossy(response.body())
            ),
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

/// Build an [`OriginalSource`] from `tx().caller()` and the
/// originating runtime, without touching the journal. For Tezos
/// origins, the caller's stored `Origin` is read back to recover the
/// PKH instead of the lossy EVM alias.
fn capture_original_source<'j, CTX, Host, R>(
    context: &CTX,
    remaining_evm_gas: u64,
) -> Result<OriginalSource, CustomPrecompileError>
where
    Host: StorageV1 + 'j,
    R: Registry + 'j,
    CTX: ContextTr<Db = EtherlinkVMDB<'j, Host, R>, Journal = Journal<'j, Host, R>>,
{
    let caller = context.tx().caller();
    let runtime = context.journal().crac_origin_runtime();
    let native_address = match runtime {
        RuntimeId::Ethereum => caller.to_string().to_lowercase(),
        RuntimeId::Tezos => context.journal().tezosx_resolve_source_alias_readonly(
            caller,
            RuntimeId::Tezos,
            remaining_evm_gas,
        )?,
    };
    Ok(OriginalSource::new(runtime, native_address, caller))
}

/// Return the captured original source, building and persisting it on
/// the first call. Re-entrant frames (EVM → TEZ → EVM → …) re-read
/// the depth-0 capture instead of falling back to their own
/// `tx().caller()` (which would be the alias of the Michelson contract
/// that re-entered the EVM).
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
    let (sender_alias, sender_gen_gas) = context
        .journal_mut()
        .tezosx_resolve_source_alias(sender, target_runtime, gas.remaining())?;
    let sender_gen_evm =
        gas::convert(target_runtime, RuntimeId::Ethereum, sender_gen_gas)
            .unwrap_or(u64::MAX);
    if sender_gen_evm > 0 {
        charge(gas, sender_gen_evm)?;
    }

    // --- source alias ---
    // Fast path: if sender == source, reuse the resolved alias and skip
    // the second storage lookup (and its cache-hit charge).
    if sender == source {
        return Ok((sender_alias.clone(), sender_alias));
    }

    charge(gas, ALIAS_LOOKUP_COST)?;
    let (source_alias, source_gen_gas) = context
        .journal_mut()
        .tezosx_resolve_source_alias(source, target_runtime, gas.remaining())?;
    let source_gen_evm =
        gas::convert(target_runtime, RuntimeId::Ethereum, source_gen_gas)
            .unwrap_or(u64::MAX);
    if source_gen_evm > 0 {
        charge(gas, source_gen_evm)?;
    }

    Ok((sender_alias, source_alias))
}

/// Inject X-Tezos-* headers carrying the trusted execution context.
///
/// - `X-Tezos-Sender`: The resolved alias of the immediate caller (UTF-8 string).
/// - `X-Tezos-Source`: The resolved alias of the transaction originator (UTF-8 string).
/// - `X-Tezos-Amount`: The value attached to the call, as a TEZ decimal string.
/// - `X-Tezos-Gas-Limit`: The gas limit forwarded to the call (decimal string).
/// - `X-Tezos-Timestamp`: The current block timestamp in seconds (decimal string).
/// - `X-Tezos-Block-Number`: The current block number (decimal string).
#[allow(clippy::too_many_arguments)]
fn inject_tezos_headers(
    headers: &mut HeaderMap,
    sender_alias: &str,
    source_alias: &str,
    amount: U256,
    gas_limit: u64,
    timestamp: U256,
    block_number: U256,
    crac_id: &str,
    gas: Gas,
) -> Result<(), CustomPrecompileError> {
    let parse_value = |v: &str| -> Result<http::HeaderValue, CustomPrecompileError> {
        v.parse().map_err(|e| {
            CustomPrecompileError::Revert(format!("invalid header value: {e}"), gas)
        })
    };
    headers.insert(X_TEZOS_SENDER, parse_value(sender_alias)?);
    headers.insert(X_TEZOS_SOURCE, parse_value(source_alias)?);
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
    Ok(())
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
    // state-mutating entries (`transfer`, `callMichelson`, `call`)
    // the confusion is around identity and value; for the read-only
    // `callMichelsonView` it's the same plus it sidesteps the
    // STATICCALL guarantees of the typed entry. In all four cases
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

    match function_call {
        RuntimeGatewayCalls::transfer(call) => {
            charge(&mut gas, RUNTIME_GATEWAY_BASE_COST)?;
            // Per-byte payload surcharge on calldata (body is empty).
            charge_payload(&mut gas, calldata.len())?;

            let implicit_address = call.implicitAddress;
            let amount = inputs.value.get();

            // Build HTTP request targeting the Tezos runtime (no entrypoint)
            let url = format!("http://tezos/{implicit_address}");
            let mut request = http::Request::builder()
                .method(http::Method::POST)
                .uri(&url)
                .body(Vec::new())
                .map_err(|e| {
                    CustomPrecompileError::Revert(
                        format!("failed to build HTTP request: {e}"),
                        gas,
                    )
                })?;

            let source = resolve_original_source(context, gas.remaining())?;
            let (sender_alias, source_alias) = resolve_aliases(
                context,
                &mut gas,
                RuntimeId::Tezos,
                inputs.caller,
                source.evm_alias(),
            )?;

            // Convert remaining EVM gas to Tezos milligas for the target runtime
            let gas_limit =
                gas::convert(RuntimeId::Ethereum, RuntimeId::Tezos, gas.remaining())
                    .ok_or_else(|| {
                        CustomPrecompileError::Revert(
                            "transfer: EVM gas limit overflows Tezos milligas".into(),
                            gas,
                        )
                    })?;
            let timestamp = context.block().timestamp();
            let block_number = context.block().number();
            let crac_id = context.journal().crac_id();

            inject_tezos_headers(
                request.headers_mut(),
                &sender_alias,
                &source_alias,
                amount,
                gas_limit,
                timestamp,
                block_number,
                &crac_id,
                gas,
            )?;

            let response = context.journal_mut().tezosx_call_http(request);
            let _body =
                classify_and_charge_crac_response(response, RuntimeId::Tezos, &mut gas)?;

            // Emit CracSent event
            let crac_log = Log {
                address: RUNTIME_GATEWAY_PRECOMPILE_ADDRESS,
                data: CracSent {
                    cracId: crac_id,
                    targetRuntime: "tezos".to_string(),
                    targetAddress: implicit_address,
                    amount,
                }
                .into_log_data(),
            };
            context.journal_mut().log(crac_log);
        }
        RuntimeGatewayCalls::callMichelson(call) => {
            charge(&mut gas, RUNTIME_GATEWAY_BASE_COST)?;
            // Per-byte payload surcharge on calldata + outgoing body.
            charge_payload(&mut gas, calldata.len())?;
            charge_payload(&mut gas, call.parameters.len())?;

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
            let (sender_alias, source_alias) = resolve_aliases(
                context,
                &mut gas,
                RuntimeId::Tezos,
                inputs.caller,
                source.evm_alias(),
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
            let timestamp = context.block().timestamp();
            let block_number = context.block().number();
            let crac_id = context.journal().crac_id();

            inject_tezos_headers(
                request.headers_mut(),
                &sender_alias,
                &source_alias,
                amount,
                gas_limit,
                timestamp,
                block_number,
                &crac_id,
                gas,
            )?;

            let response = context.journal_mut().tezosx_call_http(request);
            let _body =
                classify_and_charge_crac_response(response, RuntimeId::Tezos, &mut gas)?;

            // Emit CracSent event
            let crac_log = Log {
                address: RUNTIME_GATEWAY_PRECOMPILE_ADDRESS,
                data: CracSent {
                    cracId: crac_id,
                    targetRuntime: "tezos".to_string(),
                    targetAddress: destination,
                    amount,
                }
                .into_log_data(),
            };
            context.journal_mut().log(crac_log);
        }
        RuntimeGatewayCalls::callMichelsonView(call) => {
            charge(&mut gas, RUNTIME_GATEWAY_BASE_COST)?;

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

            // STATICCALL-compatible: read the persisted source if a
            // prior outgoing call has captured one, otherwise build
            // one locally without writing to the journal.
            let source = match context.journal().original_source() {
                Some(s) => s.clone(),
                None => capture_original_source(context, gas.remaining())?,
            };
            let sender_alias = context.journal().tezosx_resolve_source_alias_readonly(
                inputs.caller,
                RuntimeId::Tezos,
                gas.remaining(),
            )?;
            let source_alias = context.journal().tezosx_resolve_source_alias_readonly(
                source.evm_alias(),
                RuntimeId::Tezos,
                gas.remaining(),
            )?;

            let gas_limit =
                gas::convert(RuntimeId::Ethereum, RuntimeId::Tezos, gas.remaining())
                    .ok_or(CustomPrecompileError::Revert(
                        "callMichelsonView: EVM gas limit overflows Tezos milligas"
                            .into(),
                        gas,
                    ))?;
            let timestamp = context.block().timestamp();
            let block_number = context.block().number();
            let crac_id = context.journal().crac_id();

            // Amount is always zero (checked above), reuse the header
            // injector for uniform context propagation.
            inject_tezos_headers(
                request.headers_mut(),
                &sender_alias,
                &source_alias,
                U256::ZERO,
                gas_limit,
                timestamp,
                block_number,
                &crac_id,
                gas,
            )?;

            let response = context.journal_mut().tezosx_call_http(request);
            let body =
                classify_and_charge_crac_response(response, RuntimeId::Tezos, &mut gas)?;
            let output: Vec<u8> = (body,).abi_encode_params();

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
            charge(&mut gas, RUNTIME_GATEWAY_BASE_COST)?;
            // Per-byte payload surcharge on calldata + outgoing body.
            // The response body is charged after the CRAC completes.
            charge_payload(&mut gas, calldata.len())?;
            charge_payload(&mut gas, call.body.len())?;

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

            let (sender_alias, source_alias) = if is_get {
                if !amount.is_zero() {
                    return Err(CustomPrecompileError::Revert(
                        "call: GET requests cannot carry value".into(),
                        gas,
                    ));
                }
                // STATICCALL-compatible: capture without persisting.
                let source = match context.journal().original_source() {
                    Some(s) => s.clone(),
                    None => capture_original_source(context, gas.remaining())?,
                };
                (
                    context.journal().tezosx_resolve_source_alias_readonly(
                        inputs.caller,
                        target_runtime,
                        gas.remaining(),
                    )?,
                    context.journal().tezosx_resolve_source_alias_readonly(
                        source.evm_alias(),
                        target_runtime,
                        gas.remaining(),
                    )?,
                )
            } else {
                let source = resolve_original_source(context, gas.remaining())?;
                resolve_aliases(
                    context,
                    &mut gas,
                    target_runtime,
                    inputs.caller,
                    source.evm_alias(),
                )?
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
            let timestamp = context.block().timestamp();
            let block_number = context.block().number();
            let crac_id = context.journal().crac_id();

            // Inject X-Tezos-* headers with trusted execution context.
            // These carry the call context that the target runtime's `serve`
            // implementation needs (sender, source, amount, gas, block info).
            inject_tezos_headers(
                request.headers_mut(),
                &sender_alias,
                &source_alias,
                amount,
                gas_limit,
                timestamp,
                block_number,
                &crac_id,
                gas,
            )?;

            // Extract URI info before the request is consumed by the call.
            let uri = request.uri().clone();
            let target_rt = uri.host().unwrap_or("unknown").to_string();
            let target_addr = uri.path().trim_start_matches('/').to_string();

            let response = context.journal_mut().tezosx_call_http(request);
            let body =
                classify_and_charge_crac_response(response, target_runtime, &mut gas)?;
            // Response body is ABI-encoded and returned; charge per byte.
            charge_payload(&mut gas, body.len())?;
            let output: Vec<u8> = (body,).abi_encode_params();

            // POST-only state effects: burn any residual precompile
            // balance from a bridged value, and emit the CracSent
            // event. Both are incompatible with STATICCALL and
            // intentionally skipped on GET — for which the entry is
            // read-only end-to-end.
            if !is_get {
                burn_gateway_residual(context, &mut gas)?;

                let crac_log = Log {
                    address: RUNTIME_GATEWAY_PRECOMPILE_ADDRESS,
                    data: CracSent {
                        cracId: crac_id,
                        targetRuntime: target_rt,
                        targetAddress: target_addr,
                        amount,
                    }
                    .into_log_data(),
                };
                context.journal_mut().log(crac_log);
            }

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
    use tezos_protocol::contract::Contract;

    use super::*;

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
            amount,
            gas_limit,
            timestamp,
            block_number,
            "test-crac-id",
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
            U256::ZERO,
            100_000,
            U256::from(1_700_000_000u64),
            U256::from(1u64),
            "test-crac-id",
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
            amount,
            100_000,
            U256::from(1_700_000_000u64),
            U256::from(1u64),
            "test-crac-id",
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
            U256::ZERO,
            0,
            U256::ZERO,
            U256::ZERO,
            "test-crac-id",
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
    fn test_transfer_abi_decode() {
        use alloy_sol_types::SolCall;

        let call = RuntimeGateway::transferCall {
            implicitAddress: "tz1abc123".to_string(),
        };
        let encoded = call.abi_encode();

        let decoded = RuntimeGatewayCalls::abi_decode(&encoded).unwrap();
        match decoded {
            RuntimeGatewayCalls::transfer(decoded_call) => {
                assert_eq!(decoded_call.implicitAddress, "tz1abc123");
            }
            _ => panic!("expected transfer variant"),
        }
    }

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
            // 1 TEZ in wei
            U256::from(10u64).pow(U256::from(18u64)),
            50_000,
            U256::from(100u64),
            U256::from(42u64),
            "test-crac-id",
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
}

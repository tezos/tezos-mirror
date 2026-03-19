use alloy_sol_types::{sol, SolInterface, SolValue};
use evm_types::CustomPrecompileError;
use http::header::HeaderMap;
use revm::{
    context::{Block, ContextTr, JournalTr, Transaction},
    context_interface::journaled_state::account::JournaledAccountTr,
    interpreter::{CallInputs, Gas, InstructionResult, InterpreterResult},
    primitives::{alloy_primitives::IntoLogData, Bytes, Log, U256},
};
use tezos_data_encoding::nom::NomReader;
use tezos_protocol::contract::Contract;
use tezosx_interfaces::headers::format_tez_from_wei;
use tezosx_interfaces::{
    gas, RuntimeId, ERR_FORBIDDEN_TEZOS_HEADER, X_TEZOS_AMOUNT, X_TEZOS_BLOCK_NUMBER,
    X_TEZOS_GAS_CONSUMED, X_TEZOS_GAS_LIMIT, X_TEZOS_SENDER, X_TEZOS_SOURCE,
    X_TEZOS_TIMESTAMP,
};

use crate::{
    helpers::legacy::alloy_to_u256,
    journal::{CrossRuntimeCall, Journal},
    precompiles::{
        constants::{
            RUNTIME_GATEWAY_CALL_BASE_COST, RUNTIME_GATEWAY_PRECOMPILE_ADDRESS,
            RUNTIME_GATEWAY_TRANSFER_BASE_COST,
        },
        guard::out_of_gas,
        runtime_gateway::RuntimeGateway::RuntimeGatewayCalls,
    },
};
use evm_types::{DatabaseCommitPrecompileStateChanges, DatabasePrecompileStateChanges};

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

        function call(
            string url,
            (string, string)[] headers,
            bytes body,
            uint8 method,
        ) external returns (bool success, bytes memory response);
    }

    event TransferEvent(
        string implicitAddress,
        uint256 amount
    );

    event CallMichelsonEvent(
        string destination,
        string entrypoint,
        uint256 amount
    );
}

/// Build an `http::Request<Vec<u8>>` from ABI-decoded parameters.
fn build_http_request(
    url: &str,
    headers: &[(String, String)],
    body: &[u8],
    method_u8: u8,
) -> Result<http::Request<Vec<u8>>, CustomPrecompileError> {
    let method = match method_u8 {
        0 => http::Method::GET,
        1 => http::Method::POST,
        _ => {
            return Err(CustomPrecompileError::Revert(format!(
                "unsupported HTTP method: {method_u8}"
            )))
        }
    };

    let mut builder = http::Request::builder().method(method).uri(url);

    for (name, value) in headers {
        if name.as_str().to_ascii_lowercase().starts_with("x-tezos-") {
            return Err(CustomPrecompileError::Revert(format!(
                "{ERR_FORBIDDEN_TEZOS_HEADER}: {name}"
            )));
        }
        builder = builder.header(name.as_str(), value.as_str());
    }

    builder.body(body.to_vec()).map_err(|e| {
        CustomPrecompileError::Revert(format!("failed to build HTTP request: {e}"))
    })
}

/// Inject X-Tezos-* headers carrying the trusted execution context.
///
/// - `X-Tezos-Sender`: The resolved alias of the immediate caller (UTF-8 string).
/// - `X-Tezos-Source`: The resolved alias of the transaction originator (UTF-8 string).
/// - `X-Tezos-Amount`: The value attached to the call, as a TEZ decimal string.
/// - `X-Tezos-Gas-Limit`: The gas limit forwarded to the call (decimal string).
/// - `X-Tezos-Timestamp`: The current block timestamp in seconds (decimal string).
/// - `X-Tezos-Block-Number`: The current block number (decimal string).
fn inject_tezos_headers(
    headers: &mut HeaderMap,
    sender_alias: &[u8],
    source_alias: &[u8],
    amount: U256,
    gas_limit: u64,
    timestamp: U256,
    block_number: U256,
) -> Result<(), CustomPrecompileError> {
    let parse_value = |v: &str| -> Result<http::HeaderValue, CustomPrecompileError> {
        v.parse().map_err(|e| {
            CustomPrecompileError::Revert(format!("invalid header value: {e}"))
        })
    };
    //TODO: Avoid michelson specific formatting with https://linear.app/tezos/issue/L2-954/read-string-for-alias-on-durable-storage
    let sender_alias: String = Contract::nom_read_exact(sender_alias)
        .map_err(|_| CustomPrecompileError::Revert("invalid sender alias".to_string()))?
        .to_string();
    headers.insert(X_TEZOS_SENDER, parse_value(&sender_alias)?);
    let source_alias: String = Contract::nom_read_exact(source_alias)
        .map_err(|_| CustomPrecompileError::Revert("invalid source alias".to_string()))?
        .to_string();
    headers.insert(X_TEZOS_SOURCE, parse_value(&source_alias)?);
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
    Ok(())
}

pub(crate) fn runtime_gateway_precompile<'j, CTX, DB>(
    calldata: &[u8],
    context: &mut CTX,
    inputs: &CallInputs,
) -> Result<InterpreterResult, CustomPrecompileError>
where
    DB: DatabasePrecompileStateChanges
        + DatabaseCommitPrecompileStateChanges
        + revm::Database,
    CTX: ContextTr<Db = DB, Journal = Journal<'j, DB>>,
    Journal<'j, DB>: CrossRuntimeCall,
{
    // TODO: Do we need protection for STATICCALL, DELEGATECALL, CALLCODE?

    let mut gas = Gas::new(inputs.gas_limit);

    let Ok(function_call) = RuntimeGatewayCalls::abi_decode(calldata) else {
        return Err(CustomPrecompileError::Revert(String::from(
            "invalid input encoding",
        )));
    };

    match function_call {
        RuntimeGatewayCalls::transfer(call) => {
            if !gas.record_cost(RUNTIME_GATEWAY_TRANSFER_BASE_COST) {
                return Ok(out_of_gas(inputs.gas_limit));
            }

            let implicit_address = call.implicitAddress;
            let amount = inputs.value.get();

            // Build HTTP request targeting the Tezos runtime (no entrypoint)
            let url = format!("http://tezos/{implicit_address}");
            let mut request = http::Request::builder()
                .method(http::Method::POST)
                .uri(&url)
                .body(Vec::new())
                .map_err(|e| {
                    CustomPrecompileError::Revert(format!(
                        "failed to build HTTP request: {e}"
                    ))
                })?;

            // Resolve cross-runtime aliases and inject context headers
            let tx_caller = context.tx().caller();
            // Convert EVM gas to Tezos milligas for the target runtime
            let gas_limit =
                gas::convert(RuntimeId::Ethereum, RuntimeId::Tezos, inputs.gas_limit)
                    .ok_or(CustomPrecompileError::Revert(
                        "transfer: EVM gas limit overflows Tezos milligas".into(),
                    ))?;
            let timestamp = context.block().timestamp();
            let block_number = context.block().number();
            let sender_alias = context
                .journal_mut()
                .tezosx_resolve_source_alias(inputs.caller)?;
            let source_alias = context
                .journal_mut()
                .tezosx_resolve_source_alias(tx_caller)?;

            inject_tezos_headers(
                request.headers_mut(),
                &sender_alias,
                &source_alias,
                amount,
                gas_limit,
                timestamp,
                block_number,
            )?;

            let response = context.journal_mut().tezosx_call_http(request)?;
            if !response.status().is_success() {
                return Err(CustomPrecompileError::Revert(format!(
                    "Cross-runtime call failed with status {}: {}",
                    response.status(),
                    String::from_utf8_lossy(response.body())
                )));
            }

            // Emit event
            let log_data = TransferEvent {
                implicitAddress: implicit_address,
                amount,
            };
            let log = Log {
                address: RUNTIME_GATEWAY_PRECOMPILE_ADDRESS,
                data: log_data.into_log_data(),
            };
            context.journal_mut().log(log);
        }
        RuntimeGatewayCalls::callMichelson(call) => {
            if !gas.record_cost(RUNTIME_GATEWAY_TRANSFER_BASE_COST) {
                return Ok(out_of_gas(inputs.gas_limit));
            }

            let destination = call.destination;
            let entrypoint = call.entrypoint;
            let parameters = call.parameters;
            let amount = inputs.value.get();

            // Build HTTP request targeting the Tezos runtime.
            // Entrypoint goes in the URL path, Micheline parameters in the body.
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
                    CustomPrecompileError::Revert(format!(
                        "failed to build HTTP request: {e}"
                    ))
                })?;

            // Resolve cross-runtime aliases and inject context headers
            let tx_caller = context.tx().caller();
            // Convert EVM gas to Tezos milligas for the target runtime
            let gas_limit =
                gas::convert(RuntimeId::Ethereum, RuntimeId::Tezos, inputs.gas_limit)
                    .ok_or(CustomPrecompileError::Revert(
                        "callMichelson: EVM gas limit overflows Tezos milligas".into(),
                    ))?;
            let timestamp = context.block().timestamp();
            let block_number = context.block().number();
            let sender_alias = context
                .journal_mut()
                .tezosx_resolve_source_alias(inputs.caller)?;
            let source_alias = context
                .journal_mut()
                .tezosx_resolve_source_alias(tx_caller)?;

            inject_tezos_headers(
                request.headers_mut(),
                &sender_alias,
                &source_alias,
                amount,
                gas_limit,
                timestamp,
                block_number,
            )?;

            let response = context.journal_mut().tezosx_call_http(request)?;
            if !response.status().is_success() {
                return Err(CustomPrecompileError::Revert(format!(
                    "Cross-runtime call failed with status {}: {}",
                    response.status(),
                    String::from_utf8_lossy(response.body())
                )));
            }

            // Emit event
            let log_data = CallMichelsonEvent {
                destination: destination.clone(),
                entrypoint: entrypoint.clone(),
                amount,
            };
            let log = Log {
                address: RUNTIME_GATEWAY_PRECOMPILE_ADDRESS,
                data: log_data.into_log_data(),
            };
            context.journal_mut().log(log);
        }
        RuntimeGatewayCalls::call(call) => {
            if !gas.record_cost(RUNTIME_GATEWAY_CALL_BASE_COST) {
                return Ok(out_of_gas(inputs.gas_limit));
            }

            let mut request =
                build_http_request(&call.url, &call.headers, &call.body, call.method)?;

            // Resolve cross-runtime aliases for the caller and tx originator,
            // just like the transfer/callMichelson handlers do.
            let tx_caller = context.tx().caller();
            let amount = inputs.value.get();
            // Convert EVM gas to the target runtime's units.
            // Unknown host is a user error (bad URL); overflow is impossible
            // for EVM→Tezos (÷10) but we guard it defensively. Both revert.
            let target_runtime =
                request.uri().host().and_then(RuntimeId::from_host).ok_or(
                    CustomPrecompileError::Revert(
                        "httpCall: unknown or missing target runtime in URL host".into(),
                    ),
                )?;
            let gas_limit =
                gas::convert(RuntimeId::Ethereum, target_runtime, inputs.gas_limit)
                    .ok_or(CustomPrecompileError::Revert(
                        "httpCall: EVM gas limit overflows target runtime units".into(),
                    ))?;
            let timestamp = context.block().timestamp();
            let block_number = context.block().number();
            let sender_alias = context
                .journal_mut()
                .tezosx_resolve_source_alias(inputs.caller)?;
            let source_alias = context
                .journal_mut()
                .tezosx_resolve_source_alias(tx_caller)?;

            let target_runtime =
                request.uri().host().and_then(RuntimeId::from_host).ok_or(
                    CustomPrecompileError::Revert(
                        "httpCall: unknown or missing target runtime in URL host".into(),
                    ),
                )?;

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
            )?;

            // TODO: L2-918 Handle http code responses
            let response = context.journal_mut().tezosx_call_http(request)?;

            // Charge the EVM caller for gas consumed in the CRAC, whether it
            // succeeded or failed. X-Tezos-Gas-Consumed is in the called
            // runtime's units; convert to EVM units to keep the gas object
            // in sync.
            //
            // Both error cases (missing header, overflow) use Revert rather
            // than Abort: Revert fails only this call (consuming all its gas)
            // while letting EVM execution continue in the caller; Abort would
            // kill the entire blueprint. Our runtimes always set this header,
            // so either condition indicates a bug in the target runtime.
            let callee_consumed = response
                .headers()
                .get(X_TEZOS_GAS_CONSUMED)
                .and_then(|v| v.to_str().ok())
                .and_then(|s| s.parse::<u64>().ok())
                .ok_or(CustomPrecompileError::Revert(
                    "X-Tezos-Gas-Consumed header missing or invalid in CRAC response"
                        .into(),
                ))?;
            let evm_consumed =
                gas::convert(target_runtime, RuntimeId::Ethereum, callee_consumed)
                    .ok_or(CustomPrecompileError::Revert(
                        "X-Tezos-Gas-Consumed overflows EVM gas units".into(),
                    ))?;
            if !gas.record_cost(evm_consumed) {
                return Ok(out_of_gas(inputs.gas_limit));
            }

            let output: Vec<u8> = (true, response.body()).abi_encode_params();

            // The precompile is expected to have a zero balance at this stage.
            // However, due to differences in representation precision across runtimes,
            // it may retain a residual balance caused by truncation.
            // If value was bridged, this residual balance must be burned by resetting
            // the precompile's EVM balance to zero, consistent with the transfer/callMichelson
            // handling below.
            if !inputs.value.get().is_zero() {
                let mut account_load = context
                    .journal_mut()
                    .load_account_mut_skip_cold_load(
                        RUNTIME_GATEWAY_PRECOMPILE_ADDRESS,
                        true,
                    )
                    .map_err(|_| {
                        CustomPrecompileError::Revert(
                            "failed to load precompile account".into(),
                        )
                    })?;
                account_load.data.set_balance(U256::ZERO);
            }

            return Ok(InterpreterResult {
                result: InstructionResult::Return,
                gas,
                output: output.into(),
            });
        }
    }

    if !inputs.value.get().is_zero() {
        // The value was bridged to Tezos — erase it from the precompile's
        // EVM balance. set_balance records a BalanceChange journal entry so
        // this is properly reverted if the enclosing call frame reverts.
        let mut account_load = context
            .journal_mut()
            .load_account_mut_skip_cold_load(RUNTIME_GATEWAY_PRECOMPILE_ADDRESS, true)
            .map_err(|_| {
                CustomPrecompileError::Revert("failed to load precompile account".into())
            })?;
        account_load.data.set_balance(U256::ZERO);
    }

    Ok(InterpreterResult {
        result: InstructionResult::Return,
        gas,
        output: Bytes::new(),
    })
}

#[cfg(test)]
mod tests {
    use tezos_data_encoding::enc::BinWriter;

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
        let request =
            build_http_request("http://michelson/KT1abc", &headers, &[], 1).unwrap();

        assert_eq!(
            request.headers().get("Content-Type").unwrap(),
            "application/micheline"
        );
        assert_eq!(request.headers().get("X-Custom").unwrap(), "some-value");
    }

    #[test]
    fn test_build_http_request_unsupported_method() {
        let result = build_http_request("http://michelson/KT1abc", &[], &[], 42);
        assert!(matches!(
            result,
            Err(CustomPrecompileError::Revert(msg)) if msg.contains("unsupported HTTP method")
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
            &sender_alias.to_bytes().unwrap(),
            &source_alias.to_bytes().unwrap(),
            amount,
            gas_limit,
            timestamp,
            block_number,
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
        );
        assert!(matches!(
            result,
            Err(CustomPrecompileError::Revert(msg)) if msg.contains("X-Tezos-* headers are forbidden")
        ));
    }

    #[test]
    fn test_build_http_request_rejects_x_tezos_case_insensitive() {
        let result = build_http_request(
            "http://tezos/KT1abc",
            &[("x-tezos-amount".to_string(), "999".to_string())],
            &[],
            0,
        );
        assert!(matches!(
            result,
            Err(CustomPrecompileError::Revert(msg)) if msg.contains("X-Tezos-* headers are forbidden")
        ));
    }

    // --- inject_tezos_headers: amount edge cases ---

    #[test]
    fn test_inject_tezos_headers_zero_amount() {
        let mut request = build_http_request("http://tezos/KT1abc", &[], &[], 1).unwrap();

        let sender_alias =
            Contract::from_b58check("KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2").unwrap();
        let source_alias =
            Contract::from_b58check("KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2").unwrap();

        inject_tezos_headers(
            request.headers_mut(),
            &sender_alias.to_bytes().unwrap(),
            &source_alias.to_bytes().unwrap(),
            U256::ZERO,
            100_000,
            U256::from(1_700_000_000u64),
            U256::from(1u64),
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
        let mut request = build_http_request("http://tezos/KT1abc", &[], &[], 1).unwrap();

        let sender_alias =
            Contract::from_b58check("KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2").unwrap();
        let source_alias =
            Contract::from_b58check("KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2").unwrap();
        // 0.5 TEZ in wei
        let amount = U256::from(5u64) * U256::from(10u64).pow(U256::from(17u64));

        inject_tezos_headers(
            request.headers_mut(),
            &sender_alias.to_bytes().unwrap(),
            &source_alias.to_bytes().unwrap(),
            amount,
            100_000,
            U256::from(1_700_000_000u64),
            U256::from(1u64),
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
        let mut request = build_http_request("http://tezos/KT1abc", &[], &[], 1).unwrap();

        let sender_alias =
            Contract::from_b58check("KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2").unwrap();
        let source_alias =
            Contract::from_b58check("KT1GRAN26ni19mgd6xpL6tsH52LNnhKSQzP2").unwrap();

        inject_tezos_headers(
            request.headers_mut(),
            &sender_alias.to_bytes().unwrap(),
            &source_alias.to_bytes().unwrap(),
            U256::ZERO,
            0,
            U256::ZERO,
            U256::ZERO,
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
        let request = build_http_request("http://tezos/KT1abc", &[], &[], 1).unwrap();
        assert!(request.body().is_empty());
        assert_eq!(request.method(), http::Method::POST);
    }

    #[test]
    fn test_build_http_request_preserves_large_body() {
        let body = vec![0xAB; 1024];
        let request = build_http_request("http://tezos/KT1abc", &[], &body, 1).unwrap();
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
        );
        assert!(matches!(
            result,
            Err(CustomPrecompileError::Revert(msg)) if msg.contains("X-Tezos-* headers are forbidden")
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
    fn test_call_return_encoding_with_body() {
        let body = vec![0xCA, 0xFE, 0xBA, 0xBE];
        let output: Vec<u8> = (true, body).abi_encode_params();
        // bool (true) + offset to bytes + length of bytes + padded bytes
        assert!(output.len() >= 128);
        assert_eq!(output[31], 1); // bool = true
    }

    #[test]
    fn test_inject_tezos_headers_overwrites_all_existing() {
        let mut request = build_http_request("http://tezos/KT1abc", &[], &[], 1).unwrap();

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
            &sender_alias.to_bytes().unwrap(),
            &sender_alias.to_bytes().unwrap(),
            // 1 TEZ in wei
            U256::from(10u64).pow(U256::from(18u64)),
            50_000,
            U256::from(100u64),
            U256::from(42u64),
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

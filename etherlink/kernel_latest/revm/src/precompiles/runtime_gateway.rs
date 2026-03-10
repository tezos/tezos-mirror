use alloy_sol_types::{sol, SolInterface, SolValue};
use evm_types::CustomPrecompileError;
use http::header::HeaderMap;
use revm::{
    context::{Block, ContextTr, JournalTr, Transaction},
    context_interface::journaled_state::account::JournaledAccountTr,
    interpreter::{CallInputs, Gas, InstructionResult, InterpreterResult},
    primitives::{alloy_primitives::IntoLogData, Bytes, Log, U256},
};
use tezosx_interfaces::{
    ERR_FORBIDDEN_TEZOS_HEADER, X_TEZOS_AMOUNT, X_TEZOS_BLOCK_NUMBER, X_TEZOS_GAS_LIMIT,
    X_TEZOS_SENDER, X_TEZOS_SOURCE, X_TEZOS_TIMESTAMP,
};

use crate::{
    journal::{CrossRuntimeCall, Journal},
    precompiles::{
        constants::{
            RUNTIME_GATEWAY_HTTP_CALL_BASE_COST, RUNTIME_GATEWAY_PRECOMPILE_ADDRESS,
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

        function call(
            string destination,
            string entrypoint,
            bytes parameters,
        ) external;

        function httpCall(
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

    event CallEvent(
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
/// - `X-Tezos-Amount`: The value attached to the call, in wei (decimal string).
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
    let parse_value = |v: String| -> Result<http::HeaderValue, CustomPrecompileError> {
        v.parse().map_err(|e| {
            CustomPrecompileError::Revert(format!("invalid header value: {e}"))
        })
    };
    headers.insert(
        X_TEZOS_SENDER,
        parse_value(format!("{}", String::from_utf8_lossy(sender_alias)))?,
    );
    headers.insert(
        X_TEZOS_SOURCE,
        parse_value(format!("{}", String::from_utf8_lossy(source_alias)))?,
    );
    headers.insert(X_TEZOS_AMOUNT, parse_value(format!("{amount}"))?);
    headers.insert(X_TEZOS_GAS_LIMIT, parse_value(gas_limit.to_string())?);
    headers.insert(X_TEZOS_TIMESTAMP, parse_value(format!("{timestamp}"))?);
    headers.insert(
        X_TEZOS_BLOCK_NUMBER,
        parse_value(format!("{block_number}"))?,
    );
    Ok(())
}

pub(crate) fn runtime_gateway_precompile<CTX, DB>(
    calldata: &[u8],
    context: &mut CTX,
    inputs: &CallInputs,
) -> Result<InterpreterResult, CustomPrecompileError>
where
    DB: DatabasePrecompileStateChanges
        + DatabaseCommitPrecompileStateChanges
        + revm::Database,
    CTX: ContextTr<Db = DB, Journal = Journal<DB>>,
    Journal<DB>: CrossRuntimeCall,
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

            // Perform the transfer (no entrypoint/parameters)
            context.journal_mut().tezosx_call_michelson(
                inputs.caller,
                &implicit_address,
                amount,
                &[],
            )?;

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
        RuntimeGatewayCalls::call(call) => {
            if !gas.record_cost(RUNTIME_GATEWAY_TRANSFER_BASE_COST) {
                return Ok(out_of_gas(inputs.gas_limit));
            }

            let destination = call.destination;
            let entrypoint = call.entrypoint;
            let parameters = call.parameters;
            let amount = inputs.value.get();

            // Encode entrypoint + parameters into the bridge data field
            // Format: [2 bytes: entrypoint length][entrypoint UTF-8][Micheline bytes]
            let entrypoint_bytes = entrypoint.as_bytes();
            let ep_len: u16 = entrypoint_bytes.len().try_into().map_err(|_| {
                CustomPrecompileError::Revert("entrypoint name too long".into())
            })?;
            let mut data =
                Vec::with_capacity(2 + entrypoint_bytes.len() + parameters.len());
            data.extend_from_slice(&ep_len.to_be_bytes());
            data.extend_from_slice(entrypoint_bytes);
            data.extend_from_slice(&parameters);

            // Perform the cross-runtime call
            context.journal_mut().tezosx_call_michelson(
                inputs.caller,
                &destination,
                amount,
                &data,
            )?;

            // Emit event
            let log_data = CallEvent {
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
        RuntimeGatewayCalls::httpCall(call) => {
            if !gas.record_cost(RUNTIME_GATEWAY_HTTP_CALL_BASE_COST) {
                return Ok(out_of_gas(inputs.gas_limit));
            }

            let mut request =
                build_http_request(&call.url, &call.headers, &call.body, call.method)?;

            // Resolve cross-runtime aliases for the caller and tx originator,
            // just like tezosx_call_michelson does for transfer/call endpoints.
            let tx_caller = context.tx().caller();
            let amount = inputs.value.get();
            let gas_limit = inputs.gas_limit;
            let timestamp = context.block().timestamp();
            let block_number = context.block().number();
            let sender_alias = context
                .journal_mut()
                .tezosx_resolve_source_alias(inputs.caller)?;
            let source_alias = context
                .journal_mut()
                .tezosx_resolve_source_alias(tx_caller)?;

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

            // TODO: Dispatch through gateway protocol (L2-918)
            // For now, return placeholder success with empty response body
            let _request = request;
            let output: Vec<u8> = (true, Vec::<u8>::new()).abi_encode_params();

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
    fn test_http_call_abi_decode() {
        use alloy_sol_types::SolCall;

        let call = RuntimeGateway::httpCallCall {
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
            RuntimeGatewayCalls::httpCall(decoded_call) => {
                assert_eq!(decoded_call.url, "http://michelson/KT1abc/transfer");
                assert_eq!(decoded_call.headers.len(), 1);
                assert_eq!(decoded_call.headers[0].0, "Content-Type");
                assert_eq!(decoded_call.headers[0].1, "application/micheline");
                assert_eq!(decoded_call.body.as_ref(), &[0x01, 0x02]);
                assert_eq!(decoded_call.method, 1);
            }
            _ => panic!("expected httpCall variant"),
        }
    }

    #[test]
    fn test_http_call_return_encoding() {
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

        let sender_alias = b"KT1SenderAlias";
        let source_alias = b"KT1SourceAlias";
        let amount = U256::from(42_000_000u64);
        let gas_limit = 100_000u64;
        let timestamp = U256::from(1_700_000_000u64);
        let block_number = U256::from(12345u64);

        inject_tezos_headers(
            request.headers_mut(),
            sender_alias,
            source_alias,
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
            "KT1SenderAlias"
        );
        assert_eq!(
            request
                .headers()
                .get(X_TEZOS_SOURCE)
                .unwrap()
                .to_str()
                .unwrap(),
            "KT1SourceAlias"
        );
        assert_eq!(
            request
                .headers()
                .get(X_TEZOS_AMOUNT)
                .unwrap()
                .to_str()
                .unwrap(),
            "42000000"
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
}

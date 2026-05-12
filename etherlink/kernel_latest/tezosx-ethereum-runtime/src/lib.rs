// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

mod headers;
mod url;

use alloy_primitives::{hex::FromHex, Address, Bytes, Keccak256, U256 as AlloyU256};
use alloy_primitives::{IntoLogData, Log};
use alloy_sol_types::{sol, SolCall};
use http::StatusCode;
use primitive_types::U256;
use revm::context::result::{ExecutionResult, HaltReason, Output};
use revm::state::Bytecode;
use revm_etherlink::precompiles::constants::RUNTIME_GATEWAY_PRECOMPILE_ADDRESS;
use revm_etherlink::{
    precompiles::constants::{
        ALIAS_FORWARDER_PRECOMPILE_ADDRESS, ALIAS_FORWARDER_SOL_CONTRACT,
        TEZOSX_CALLER_ADDRESS,
    },
    run_transaction,
    storage::{
        code::CodeStorage, version::read_evm_version, world_state_handler::StorageAccount,
    },
    ExecutionOutcome, GasData, TransactionOrigin,
};
use tezos_ethereum::block::{BlockConstants, BlockFees};
use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_interfaces::{
    AliasInfo, CrossRuntimeContext, Origin, Registry, RuntimeInterface,
    TezosXRuntimeError, X_TEZOS_GAS_CONSUMED,
};
use tezosx_journal::TezosXJournal;

alloy_sol_types::sol! {
    function init_tezosx_alias(string nativeAddress, bytes nativePublicKey) external payable;
}

pub struct EthereumRuntime {
    chain_id: primitive_types::U256,
}

impl Default for EthereumRuntime {
    fn default() -> Self {
        Self::new(U256::from(1337))
    }
}

impl EthereumRuntime {
    pub fn new(chain_id: primitive_types::U256) -> Self {
        Self { chain_id }
    }

    fn create_block_constants(
        &self,
        host: &impl StorageV1,
        context: &CrossRuntimeContext,
    ) -> BlockConstants {
        let coinbase = read_sequencer_pool_address(host).unwrap_or_default();

        BlockConstants {
            number: context.block_number,
            coinbase,
            timestamp: context.timestamp,
            gas_limit: context.gas_limit,
            block_fees: BlockFees::new(
                primitive_types::U256::zero(),
                primitive_types::U256::zero(),
                primitive_types::U256::zero(),
            ),
            chain_id: self.chain_id,
            tezos_experimental_features: true,
            prevrandao: None,
        }
    }
}

/// Read the sequencer pool address (coinbase) from storage.
fn read_sequencer_pool_address(host: &impl StorageV1) -> Option<primitive_types::H160> {
    use tezos_smart_rollup_host::path::RefPath;
    const SEQUENCER_POOL_PATH: RefPath =
        RefPath::assert_from(b"/evm/sequencer_pool_address");
    let mut bytes = [0u8; 20];
    match host.store_read_slice(&SEQUENCER_POOL_PATH, 0, bytes.as_mut_slice()) {
        Ok(20) => Some(bytes.into()),
        _ => None,
    }
}

/// Build an HTTP response from the result of [`execute_request`].
///
/// Execution results are mapped to HTTP status codes:
/// - `Success` → 200
/// - `Revert` → 400 (the contract rejected the call)
/// - `Halt(OutOfGas)` → 429 (OOG)
/// - `Halt(other)` → 500 (invalid opcode, stack overflow, etc.)
///
/// Pre-execution errors are mapped as:
/// - `BadRequest` → 400
/// - `NotFound` → 404
///
/// All other errors propagate as `Err` — they indicate infrastructure
/// problems that cannot be meaningfully represented as HTTP responses.
fn build_response(
    result: Result<ExecutionOutcome, TezosXRuntimeError>,
) -> http::Response<Vec<u8>> {
    let (status, body, gas_consumed) = match result {
        Ok(outcome) => {
            // X-Tezos-Gas-Consumed is in the called runtime's units (EVM here).
            // The caller is responsible for converting to its own units.
            let gas = outcome.result.gas_used().to_string();
            match outcome.result {
                ExecutionResult::Success { output, .. } => {
                    let body = match output {
                        Output::Call(bytes) => bytes.to_vec(),
                        Output::Create(bytes, _) => bytes.to_vec(),
                    };
                    (StatusCode::OK, body, gas)
                }
                ExecutionResult::Revert { output, .. } => {
                    (StatusCode::BAD_REQUEST, output.to_vec(), gas)
                }
                ExecutionResult::Halt { reason, .. } => {
                    let status = match reason {
                        HaltReason::OutOfGas(_) => StatusCode::TOO_MANY_REQUESTS,
                        // STATICCALL violation on the read-only entry
                        // (HTTP `GET`): catchable client error.
                        HaltReason::StateChangeDuringStaticCall => {
                            StatusCode::BAD_REQUEST
                        }
                        _ => StatusCode::INTERNAL_SERVER_ERROR,
                    };
                    (
                        status,
                        format!("EVM execution halted: {reason:?}").into_bytes(),
                        gas,
                    )
                }
            }
        }
        Err(TezosXRuntimeError::BadRequest(msg)) => {
            (StatusCode::BAD_REQUEST, msg.into_bytes(), "0".to_string())
        }
        Err(TezosXRuntimeError::NotFound(msg)) => {
            (StatusCode::NOT_FOUND, msg.into_bytes(), "0".to_string())
        }
        Err(TezosXRuntimeError::MethodNotAllowed(msg)) => (
            StatusCode::METHOD_NOT_ALLOWED,
            msg.into_bytes(),
            "0".to_string(),
        ),
        Err(TezosXRuntimeError::OutOfGas) => (
            StatusCode::TOO_MANY_REQUESTS,
            b"OOG".to_vec(),
            "0".to_string(),
        ),
        Err(e) => (
            StatusCode::INTERNAL_SERVER_ERROR,
            format!("{e:?}").into_bytes(),
            "0".to_string(),
        ),
    };
    // Safe to unwrap: status is a predefined constant, header name is a
    // static ASCII string, and the value is a decimal u64.
    http::Response::builder()
        .status(status)
        .header(X_TEZOS_GAS_CONSUMED, &gas_consumed)
        .body(body)
        .unwrap()
}

sol! {
    event CracReceived(
        string cracId,
        string sourceRuntime,
        string senderAddress,
        string sourceAddress,
        string targetAddress,
        uint256 amount
    );
}

/// Dispatch a cross-runtime request on the HTTP method:
///
/// - `POST` → [`execute_call`] (state-mutating).
/// - `GET`  → [`execute_static_call`] (read-only, top frame runs
///   with `is_static = true` — REVM enforces strict `STATICCALL`).
/// - else  → catchable `405`.
///
/// `serve` then only handles the result-to-HTTP-status mapping.
fn execute_request<Host>(
    runtime: &EthereumRuntime,
    registry: &impl Registry,
    host: &mut Host,
    journal: &mut TezosXJournal,
    request: http::Request<Vec<u8>>,
) -> Result<ExecutionOutcome, TezosXRuntimeError>
where
    Host: StorageV1,
{
    match *request.method() {
        http::Method::POST => execute_call(runtime, registry, host, journal, request),
        http::Method::GET => {
            execute_static_call(runtime, registry, host, journal, request)
        }
        ref other => Err(TezosXRuntimeError::MethodNotAllowed(format!(
            "HTTP method {other} not allowed (use POST for entrypoint calls or GET for static calls)"
        ))),
    }
}

/// Execute a state-mutating cross-runtime entrypoint call (POST).
///
/// Unchanged behavior from pre-L2-1259: emits `CracReceived`, credits the
/// sender, and runs the EVM transaction with state changes preserved
/// (the journal is not committed here — the outer block builder handles
/// that for the whole CRAC).
fn execute_call<Host>(
    runtime: &EthereumRuntime,
    registry: &impl Registry,
    host: &mut Host,
    journal: &mut TezosXJournal,
    request: http::Request<Vec<u8>>,
) -> Result<ExecutionOutcome, TezosXRuntimeError>
where
    Host: StorageV1,
{
    let parsed = url::parse_ethereum_url(request.uri())?;
    let hdrs = headers::parse_request_headers(request.headers())?;
    let call_data = Bytes::from(request.into_body());

    // Verify CRAC-ID from incoming header (debug/consistency check).
    if let Some(crac_id) = hdrs.crac_id {
        journal
            .verify_crac_id(&crac_id)
            .map_err(|e| TezosXRuntimeError::Custom(e.to_string()))?;
    }
    // Save transaction info for building the fake EVM transaction.
    // set_crac_tx_info is only called once subsequent calls in the same CRAC execution will
    // result in an error.
    if !journal.evm.has_crac_data() {
        journal
            .evm
            .set_crac_tx_info(tezosx_journal::CracTransactionInfo {
                source: hdrs.source.unwrap_or_default(),
                sender: hdrs.sender,
                gas_limit: revm::primitives::U256::from(hdrs.gas_limit),
                amount: revm::primitives::U256::from_limbs(hdrs.amount.into_limbs()),
            })
            .map_err(|e| TezosXRuntimeError::Custom(e.to_string()))?;
    }

    let context = CrossRuntimeContext {
        gas_limit: hdrs.gas_limit,
        timestamp: hdrs.timestamp,
        block_number: hdrs.block_number,
    };

    let evm_version = read_evm_version(host);
    let block_constants = runtime.create_block_constants(host, &context);
    let gas_data = GasData::new(hdrs.gas_limit, 0, hdrs.gas_limit);
    let crac_log = Log {
        address: RUNTIME_GATEWAY_PRECOMPILE_ADDRESS,
        data: CracReceived {
            cracId: journal.crac_id().to_string(),
            // TODO: Pass it in headers when more than 2 runtimes are supported.
            sourceRuntime: "tezos".to_string(),
            senderAddress: hdrs.sender.to_string(),
            sourceAddress: hdrs.source.unwrap_or_default().to_string(),
            targetAddress: parsed.destination.to_string(),
            amount: hdrs.amount,
        }
        .into_log_data(),
    };
    journal.evm.inner.log(crac_log);

    let outcome = run_transaction(
        host,
        registry,
        journal,
        evm_version.into(),
        &block_constants,
        None,
        hdrs.sender,
        Some(parsed.destination),
        call_data,
        gas_data,
        hdrs.amount,
        None,
        None,
        false,
        TransactionOrigin::CrossRuntime {
            credit: Some((hdrs.sender, hdrs.amount)),
        },
    )
    .map_err(|e| TezosXRuntimeError::Custom(format!("EVM execution failed: {e:?}")))?;

    Ok(outcome)
}

/// Read-only cross-runtime call (HTTP `GET`): the EVM-side entry
/// point for any originating runtime that wants to read EVM state
/// without leaving observable on-chain effects.
///
/// Differences from [`execute_call`]:
/// - rejects `X-Tezos-Amount != 0` (catchable `400`);
/// - no `CracReceived` log emission;
/// - runs the EVM transaction under [`TransactionOrigin::CrossRuntimeStatic`],
///   so the top-level frame has `is_static = true` and any state
///   mutation halts with `StateChangeDuringStaticCall` (surfaced as
///   `400`).
fn execute_static_call<Host>(
    runtime: &EthereumRuntime,
    registry: &impl Registry,
    host: &mut Host,
    journal: &mut TezosXJournal,
    request: http::Request<Vec<u8>>,
) -> Result<ExecutionOutcome, TezosXRuntimeError>
where
    Host: StorageV1,
{
    let parsed = url::parse_ethereum_url(request.uri())?;
    let hdrs = headers::parse_request_headers(request.headers())?;
    let call_data = Bytes::from(request.into_body());

    if !hdrs.amount.is_zero() {
        return Err(TezosXRuntimeError::BadRequest(
            "static calls (GET) cannot carry value (X-Tezos-Amount must be 0)".into(),
        ));
    }

    // Verify CRAC-ID from incoming header (debug/consistency check).
    if let Some(crac_id) = hdrs.crac_id {
        journal
            .verify_crac_id(&crac_id)
            .map_err(|e| TezosXRuntimeError::Custom(e.to_string()))?;
    }

    // Set CRAC tx info on first hit (forced `amount = 0`); same
    // sender/source headers as `execute_call`.
    if !journal.evm.has_crac_data() {
        journal
            .evm
            .set_crac_tx_info(tezosx_journal::CracTransactionInfo {
                source: hdrs.source.unwrap_or_default(),
                sender: hdrs.sender,
                gas_limit: revm::primitives::U256::from(hdrs.gas_limit),
                amount: revm::primitives::U256::ZERO,
            })
            .map_err(|e| TezosXRuntimeError::Custom(e.to_string()))?;
    }

    let context = CrossRuntimeContext {
        gas_limit: hdrs.gas_limit,
        timestamp: hdrs.timestamp,
        block_number: hdrs.block_number,
    };

    let evm_version = read_evm_version(host);
    let block_constants = runtime.create_block_constants(host, &context);
    let gas_data = GasData::new(hdrs.gas_limit, 0, hdrs.gas_limit);

    // Intentionally no `CracReceived` log on the static path.

    let outcome = run_transaction(
        host,
        registry,
        journal,
        evm_version.into(),
        &block_constants,
        None,
        hdrs.sender,
        Some(parsed.destination),
        call_data,
        gas_data,
        revm::primitives::U256::ZERO,
        None,
        None,
        // `is_simulation = true` disables EIP-3607 so a contract-aliased
        // caller (e.g. a Michelson contract's deterministic EVM alias,
        // which we expect to have code deployed for forwarder usage)
        // can sponsor a static read without being rejected at
        // validation time.
        true,
        TransactionOrigin::CrossRuntimeStatic,
    )
    .map_err(|e| {
        TezosXRuntimeError::Custom(format!("EVM static execution failed: {e:?}"))
    })?;

    Ok(outcome)
}

impl RuntimeInterface for EthereumRuntime {
    fn ensure_alias<Host>(
        &self,
        registry: &impl Registry,
        host: &mut Host,
        journal: &mut TezosXJournal,
        alias_info: AliasInfo,
        native_public_key: Option<&[u8]>,
        context: CrossRuntimeContext,
        gas_remaining: u64,
    ) -> Result<(String, u64), TezosXRuntimeError>
    where
        Host: StorageV1,
    {
        // The native address is stored in `alias_info` as the UTF-8
        // bytes of the canonical address string. Decode once for the
        // EVM init call below; the hash and the classification record
        // both work on the bytes directly.
        let native_address =
            std::str::from_utf8(&alias_info.native_address).map_err(|e| {
                TezosXRuntimeError::ConversionError(format!(
                    "alias_info.native_address is not valid UTF-8: {e}"
                ))
            })?;

        // Step 1: Compute the alias address deterministically from the native address
        let mut hasher = Keccak256::new();
        hasher.update(&alias_info.native_address);
        let hash = hasher.finalize();
        let alias = Address::from_slice(&hash[0..20]);

        // Set up the EIP-7702 delegation bytecode and its hash so we
        // can both detect whether a forwarder is already deployed and
        // deploy one if needed.
        let delegation_bytecode =
            Bytecode::new_eip7702(ALIAS_FORWARDER_PRECOMPILE_ADDRESS);
        let code_bytes = delegation_bytecode.original_byte_slice();
        let delegation_code_hash =
            revm_etherlink::helpers::storage::bytes_hash(code_bytes);

        let mut alias_account = StorageAccount::from_address(&alias)?;

        // Branch 1: already classified as alias. The kernel reaches
        // this on the second and later calls to ensure_alias for the
        // same source. Returning early preserves the gas budget and
        // performs no durable writes.
        match alias_account.get_origin(host).map_err(|e| {
            TezosXRuntimeError::Custom(format!("Failed to read alias origin: {e}"))
        })? {
            Some(Origin::Alias(_)) => {
                return Ok((alias.to_string(), gas_remaining));
            }
            Some(Origin::Native) => {
                return Err(TezosXRuntimeError::Custom(format!(
                    "ensure_alias: address {alias} is recorded as Native, refusing to overwrite"
                )));
            }
            None => {}
        }

        // Branch 2: a forwarder is already deployed but the
        // classification path is empty. This is the legacy account
        // case. Write the classification record only and skip the
        // redeploy.
        let mut storage_info = alias_account.info(host)?;
        if storage_info.code_hash == delegation_code_hash {
            let new_origin = Origin::Alias(alias_info);
            alias_account.set_origin(host, &new_origin).map_err(|e| {
                TezosXRuntimeError::Custom(format!("Failed to write alias origin: {e}"))
            })?;
            return Ok((alias.to_string(), gas_remaining));
        }

        // Branch 3: full materialization. Deploy the forwarder, run
        // the existing init transaction, and record the classification
        // at the end.

        // Set up EIP-7702 delegation bytecode at the alias address.
        storage_info.code_hash = delegation_code_hash;
        alias_account.set_info(host, storage_info)?;

        CodeStorage::add(host, code_bytes, Some(delegation_code_hash)).map_err(|e| {
            TezosXRuntimeError::Custom(format!("Failed to store delegation code: {e}"))
        })?;

        // Ensure the AliasForwarder precompile code is available
        // (it should be initialized at kernel startup, but verify it exists)
        let precompile_account =
            StorageAccount::from_address(&ALIAS_FORWARDER_PRECOMPILE_ADDRESS)?;
        let precompile_info = precompile_account.info(host)?;
        if precompile_info.code_hash != ALIAS_FORWARDER_SOL_CONTRACT.code_hash {
            // Initialize the precompile if not already done
            CodeStorage::add(
                host,
                ALIAS_FORWARDER_SOL_CONTRACT.code,
                Some(ALIAS_FORWARDER_SOL_CONTRACT.code_hash),
            )
            .map_err(|e| {
                TezosXRuntimeError::Custom(format!(
                    "Failed to store AliasForwarder precompile code: {e}"
                ))
            })?;
        }

        // Step 3: Call init_tezosx_alias on the alias address to set up storage
        // The native address is passed as a string (e.g., "tz1...")

        // Encode the init_tezosx_alias call
        let call_data = init_tezosx_aliasCall {
            nativeAddress: native_address.to_string(),
            nativePublicKey: native_public_key.unwrap_or_default().to_vec().into(),
        }
        .abi_encode();

        // Set up block constants for EVM execution
        let evm_version = read_evm_version(host);
        let block_constants = self.create_block_constants(host, &context);

        // Use the caller's remaining gas so it controls the budget. The
        // `effective_gas_price` is 0, so REVM's pre-flight balance check on the
        // caller (`gas_limit * gas_price + value`) reduces to 0 and no funding
        // is needed for `TEZOSX_CALLER_ADDRESS`. Earlier kernels wrote
        // `U256::MAX` to durable storage here as a "safety" buffer, which
        // leaked as a visible huge balance on Blockscout — kept in
        // `TransactionOrigin::CrossRuntime { credit: ... }` for any non-zero
        // future credit, but here we don't credit anything.
        let gas_data = GasData::new(gas_remaining, 0, gas_remaining);

        // Run the EVM transaction to call init_tezosx_alias
        let outcome = run_transaction(
            host,
            registry,
            journal,
            evm_version.into(),
            &block_constants,
            None, // no transaction hash for internal transactions
            TEZOSX_CALLER_ADDRESS,
            Some(alias), // call to the alias address
            Bytes::from(call_data),
            gas_data,
            AlloyU256::ZERO, // no value transfer
            None,            // no authorization list
            None,            // no tracer
            false,           // not a simulation
            TransactionOrigin::CrossRuntime { credit: None },
        )
        .map_err(|e| {
            TezosXRuntimeError::Custom(format!("EVM execution failed: {e:?}"))
        })?;

        // Check that the call succeeded; return remaining EVM gas.
        let gas_used = outcome.result.gas_used();
        let remaining_after = gas_remaining.saturating_sub(gas_used);
        match outcome.result {
            ExecutionResult::Success { .. } => {
                // Record the classification only after init succeeded.
                // The forwarder bytecode and the classification record
                // share the same persistence model: both go to durable
                // storage, both survive a revert in the surrounding
                // frame, and the next call to ensure_alias finds the
                // no-op branch.
                let mut alias_account = StorageAccount::from_address(&alias)?;
                let new_origin = Origin::Alias(alias_info);
                alias_account.set_origin(host, &new_origin).map_err(|e| {
                    TezosXRuntimeError::Custom(format!(
                        "Failed to write alias origin: {e}"
                    ))
                })?;
                Ok((alias.to_string(), remaining_after))
            }
            ExecutionResult::Revert { output, .. } => Err(TezosXRuntimeError::Custom(
                format!("init_tezosx_alias reverted: {output:?}"),
            )),
            ExecutionResult::Halt { reason, .. } => Err(TezosXRuntimeError::Custom(
                format!("init_tezosx_alias halted: {reason:?}"),
            )),
        }
    }

    fn compute_alias(&self, native_address: &[u8]) -> Result<String, TezosXRuntimeError> {
        let mut hasher = Keccak256::new();
        hasher.update(native_address);
        let hash = hasher.finalize();
        let alias = Address::from_slice(&hash[0..20]);
        Ok(alias.to_string())
    }

    fn serve<Host>(
        &self,
        registry: &impl Registry,
        host: &mut Host,
        journal: &mut TezosXJournal,
        request: http::Request<Vec<u8>>,
    ) -> http::Response<Vec<u8>>
    where
        Host: StorageV1,
    {
        build_response(execute_request(self, registry, host, journal, request))
    }

    fn host(&self) -> &'static str {
        "ethereum"
    }

    fn address_from_string(
        &self,
        address_str: &str,
    ) -> Result<Vec<u8>, TezosXRuntimeError> {
        let address = Address::from_hex(address_str).map_err(|e| {
            TezosXRuntimeError::Custom(format!("Invalid address string: {e}"))
        })?;
        Ok(address.0.to_vec())
    }

    // Need to implement this only for IDE. Not needed in compilation or tests.
    #[cfg(feature = "testing")]
    fn get_balance(
        &self,
        _host: &mut impl StorageV1,
        _address: &[u8],
    ) -> Result<primitive_types::U256, TezosXRuntimeError> {
        unimplemented!("Use mocks if you are in tests")
    }

    // Need to implement this only for IDE. Not needed in compilation or tests.
    #[cfg(feature = "testing")]
    fn string_from_address(&self, _address: &[u8]) -> Result<String, TezosXRuntimeError> {
        unimplemented!("Use mocks if you are in tests")
    }
}

#[cfg(test)]
mod tests {
    use alloy_primitives::{hex::FromHex, Bytes, Keccak256};
    use revm::primitives::Address;
    use revm::state::{AccountInfo, Bytecode};
    use revm_etherlink::journal::commit_evm_journal_from_external;
    use revm_etherlink::{
        helpers::storage::bytes_hash,
        storage::{code::CodeStorage, world_state_handler::StorageAccount},
    };
    use tezos_ethereum::block::BlockConstants;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_host::storage::StorageV1;
    use tezosx_interfaces::{
        AliasInfo, CrossRuntimeContext, Registry, RuntimeId, RuntimeInterface,
        TezosXRuntimeError,
    };
    use tezosx_journal::TezosXJournal;

    use crate::EthereumRuntime;

    /// Minimal Registry stub for testing EthereumRuntime in isolation.
    struct StubRegistry;

    impl Registry for StubRegistry {
        fn ensure_alias<Host>(
            &self,
            _host: &mut Host,
            _journal: &mut TezosXJournal,
            _alias_info: AliasInfo,
            _native_public_key: Option<&[u8]>,
            _target_runtime: RuntimeId,
            _context: CrossRuntimeContext,
            _gas_remaining: u64,
        ) -> Result<(String, u64), TezosXRuntimeError>
        where
            Host: StorageV1,
        {
            unimplemented!("not needed for this test")
        }

        fn compute_alias(
            &self,
            _alias_info: AliasInfo,
        ) -> Result<String, TezosXRuntimeError> {
            unimplemented!("not needed for this test")
        }

        fn address_from_string(
            &self,
            _address_str: &str,
            _runtime_id: RuntimeId,
        ) -> Result<Vec<u8>, TezosXRuntimeError> {
            unimplemented!("not needed for this test")
        }

        fn serve<Host>(
            &self,
            _host: &mut Host,
            _journal: &mut TezosXJournal,
            _request: http::Request<Vec<u8>>,
        ) -> http::Response<Vec<u8>>
        where
            Host: StorageV1,
        {
            unimplemented!("not needed for this test")
        }
    }

    /// Build an HTTP request for the Ethereum runtime's `serve()` method.
    fn build_serve_request(
        sender: &Address,
        destination: &Address,
        amount: &str,
        body: Vec<u8>,
    ) -> http::Request<Vec<u8>> {
        // POST is the explicit method for state-mutating cross-runtime
        // entrypoint calls. The HTTP default is GET, which since L2-1259
        // routes to the read-only static path; without this explicit
        // method, every existing transfer/invoke test would silently
        // hit the static path and either reject (amount != 0) or revert
        // its writes, producing confusing failures.
        let url = format!(
            "http://ethereum/{}",
            alloy_primitives::hex::encode(destination.0 .0)
        );
        http::Request::builder()
            .method(http::Method::POST)
            .uri(&url)
            .header(
                tezosx_interfaces::X_TEZOS_SENDER,
                format!("0x{}", alloy_primitives::hex::encode(sender.0 .0)),
            )
            .header(tezosx_interfaces::X_TEZOS_AMOUNT, amount)
            .header(tezosx_interfaces::X_TEZOS_GAS_LIMIT, u64::MAX.to_string())
            .header(tezosx_interfaces::X_TEZOS_TIMESTAMP, "1")
            .header(tezosx_interfaces::X_TEZOS_BLOCK_NUMBER, "1")
            .body(body)
            .unwrap()
    }

    #[test]
    fn test_serve_simple_transfer() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let block_constants = BlockConstants::test_block_with_no_fees();
        let registry = StubRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x22; 20]);

        // No need to fund the sender: serve() credits the sender with
        // the transfer amount (the calling runtime already debited it).
        let five_tez_wei = revm::primitives::U256::from(5_000_000_000_000_000_000u128);

        let mut journal = TezosXJournal::default();
        let request = build_serve_request(&sender, &destination, "5", vec![]);
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);
        commit_evm_journal_from_external(
            &mut host,
            &registry,
            &block_constants,
            &mut journal,
        )
        .unwrap();

        // Verify destination received the transfer (5 TEZ in wei)
        let destination_account = StorageAccount::from_address(&destination).unwrap();
        let info = destination_account.info(&mut host).unwrap();
        assert_eq!(info.balance, five_tez_wei);
    }

    #[test]
    fn test_serve_executes_contract_bytecode() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let block_constants = BlockConstants::test_block_with_no_fees();
        let registry = StubRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        let contract = Address::from_slice(&[0x22; 20]);

        // Deploy a tiny contract:
        //   PUSH1 0x42   (value to store)
        //   PUSH1 0x01   (storage slot)
        //   SSTORE       (store 0x42 at slot 1)
        //   PUSH1 0x01
        //   SLOAD        (load from slot 1)
        let bytecode_raw = Bytes::from_hex("6042600155600154").unwrap();
        let code_hash = bytes_hash(&bytecode_raw);
        let mut contract_account = StorageAccount::from_address(&contract).unwrap();
        contract_account
            .set_info(
                &mut host,
                AccountInfo {
                    balance: revm::primitives::U256::ZERO,
                    nonce: 0,
                    code_hash,
                    account_id: None,
                    code: Some(Bytecode::new_raw(bytecode_raw.clone())),
                },
            )
            .unwrap();
        CodeStorage::add(&mut host, &bytecode_raw, Some(code_hash)).unwrap();

        let mut journal = TezosXJournal::default();
        let request = build_serve_request(&sender, &contract, "0", vec![]);
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);
        commit_evm_journal_from_external(
            &mut host,
            &registry,
            &block_constants,
            &mut journal,
        )
        .unwrap();

        // Verify the contract wrote 0x42 to storage slot 1
        let slot_value = contract_account
            .get_storage(&host, &revm::primitives::U256::from(1))
            .unwrap();
        assert_eq!(slot_value, revm::primitives::U256::from(0x42));
    }

    /// Test that serve() correctly passes value to an EVM contract via
    /// X-Tezos-Amount. The contract reads CALLVALUE and stores it.
    #[test]
    fn test_serve_with_value_sets_correct_msg_value() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = StubRegistry;
        let block_constants = BlockConstants::test_block_with_no_fees();

        let sender = Address::from_slice(&[0x11; 20]);
        let contract = Address::from_slice(&[0x22; 20]);

        // No need to fund the sender: execute_request credits the sender
        // with the transfer amount (the calling runtime already debited it).

        // Deploy a contract that stores CALLVALUE to slot 0:
        //   CALLVALUE    (0x34)
        //   PUSH1 0x00   (0x6000)
        //   SSTORE       (0x55)
        let bytecode_raw = Bytes::from_hex("34600055").unwrap();
        let code_hash = bytes_hash(&bytecode_raw);
        let mut contract_account = StorageAccount::from_address(&contract).unwrap();
        contract_account
            .set_info(
                &mut host,
                AccountInfo {
                    balance: revm::primitives::U256::ZERO,
                    nonce: 0,
                    code_hash,
                    account_id: None,
                    code: Some(Bytecode::new_raw(bytecode_raw.clone())),
                },
            )
            .unwrap();
        CodeStorage::add(&mut host, &bytecode_raw, Some(code_hash)).unwrap();

        let mut journal = TezosXJournal::default();
        let request = build_serve_request(&sender, &contract, "42", vec![]);
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);
        commit_evm_journal_from_external(
            &mut host,
            &registry,
            &block_constants,
            &mut journal,
        )
        .unwrap();

        // Contract stored CALLVALUE at slot 0 — verify it saw the real value
        // "42" TEZ = 42 * 10^18 wei
        let forty_two_tez_wei =
            revm::primitives::U256::from(42_000_000_000_000_000_000u128);
        let stored_value = contract_account
            .get_storage(&host, &revm::primitives::U256::ZERO)
            .unwrap();
        assert_eq!(
            stored_value, forty_two_tez_wei,
            "Contract should see msg.value = 42 TEZ in wei"
        );

        // Contract received the transfer
        let contract_info = contract_account.info(&mut host).unwrap();
        assert_eq!(
            contract_info.balance, forty_two_tez_wei,
            "Contract should hold the transferred value"
        );

        // Sender balance should be 0 after sending 42
        let sender_account = StorageAccount::from_address(&sender).unwrap();
        let sender_info = sender_account.info(&mut host).unwrap();
        assert_eq!(
            sender_info.balance,
            revm::primitives::U256::ZERO,
            "Sender balance should be 0 after transfer"
        );
    }

    /// Test that alias addresses are computed deterministically from native addresses.
    #[test]
    fn test_alias_address_is_deterministic() {
        let native_address = b"tz1abc123";

        // Compute alias address using the same algorithm as ensure_alias
        let mut hasher = Keccak256::new();
        hasher.update(native_address);
        let hash = hasher.finalize();
        let alias = &hash[0..20];

        // Verify it's 20 bytes
        assert_eq!(alias.len(), 20);

        // Compute again to verify determinism
        let mut hasher2 = Keccak256::new();
        hasher2.update(native_address);
        let hash2 = hasher2.finalize();
        let alias2 = &hash2[0..20];

        assert_eq!(alias, alias2);
    }

    #[test]
    fn test_different_native_addresses_produce_different_aliases() {
        let compute_alias = |native: &[u8]| {
            let mut hasher = Keccak256::new();
            hasher.update(native);
            let hash = hasher.finalize();
            hash[0..20].to_vec()
        };

        let alias1 = compute_alias(b"tz1abc");
        let alias2 = compute_alias(b"tz1xyz");

        assert_ne!(alias1, alias2);
    }

    mod build_response_tests {
        use super::*;
        use crate::build_response;
        use http::StatusCode;
        use revm::context::result::{
            ExecutionResult, HaltReason, Output, ResultGas, SuccessReason,
        };
        use revm_etherlink::ExecutionOutcome;
        use tezosx_interfaces::X_TEZOS_GAS_CONSUMED;

        fn make_success(output: Vec<u8>) -> ExecutionOutcome {
            ExecutionOutcome {
                result: ExecutionResult::Success {
                    reason: SuccessReason::Return,
                    gas: ResultGas::new(u64::MAX, 21000, 0, 0, 0),
                    output: Output::Call(output.into()),
                    logs: vec![],
                },
                withdrawals: vec![],
            }
        }

        #[test]
        fn success_returns_200() {
            let resp = build_response(Ok(make_success(b"hello".to_vec())));
            assert_eq!(resp.status(), StatusCode::OK);
            assert_eq!(resp.body(), b"hello");
        }

        #[test]
        fn success_empty_body() {
            let resp = build_response(Ok(make_success(vec![])));
            assert_eq!(resp.status(), StatusCode::OK);
            assert!(resp.body().is_empty());
        }

        #[test]
        fn revert_returns_400() {
            let outcome = ExecutionOutcome {
                result: ExecutionResult::Revert {
                    logs: vec![],
                    gas: ResultGas::new(u64::MAX, 21000, 0, 0, 0),
                    output: b"revert reason".to_vec().into(),
                },
                withdrawals: vec![],
            };
            let resp = build_response(Ok(outcome));
            assert_eq!(resp.status(), StatusCode::BAD_REQUEST);
            assert_eq!(resp.body(), b"revert reason");
        }

        #[test]
        fn halt_out_of_gas_returns_429() {
            let outcome = ExecutionOutcome {
                result: ExecutionResult::Halt {
                    reason: HaltReason::OutOfGas(
                        revm::context::result::OutOfGasError::Basic,
                    ),
                    logs: vec![],
                    gas: ResultGas::new(u64::MAX, 21000, 0, 0, 0),
                },
                withdrawals: vec![],
            };
            let resp = build_response(Ok(outcome));
            assert_eq!(resp.status(), StatusCode::TOO_MANY_REQUESTS);
        }

        #[test]
        fn halt_other_returns_500() {
            let outcome = ExecutionOutcome {
                result: ExecutionResult::Halt {
                    reason: HaltReason::StackOverflow,
                    logs: vec![],
                    gas: ResultGas::new(u64::MAX, 21000, 0, 0, 0),
                },
                withdrawals: vec![],
            };
            let resp = build_response(Ok(outcome));
            assert_eq!(resp.status(), StatusCode::INTERNAL_SERVER_ERROR);
        }

        #[test]
        fn bad_request_error_returns_400() {
            let resp =
                build_response(Err(TezosXRuntimeError::BadRequest("invalid URL".into())));
            assert_eq!(resp.status(), StatusCode::BAD_REQUEST);
            assert!(String::from_utf8_lossy(resp.body()).contains("invalid URL"));
        }

        #[test]
        fn not_found_error_returns_404() {
            let resp = build_response(Err(TezosXRuntimeError::NotFound(
                "no such contract".into(),
            )));
            assert_eq!(resp.status(), StatusCode::NOT_FOUND);
            assert!(String::from_utf8_lossy(resp.body()).contains("no such contract"));
        }

        #[test]
        fn custom_error_returns_500() {
            let resp = build_response(Err(TezosXRuntimeError::Custom("boom".into())));
            assert_eq!(resp.status(), StatusCode::INTERNAL_SERVER_ERROR);
        }

        // Gas header tests: make_success uses ResultGas::new(u64::MAX, 21000, 0, 0, 0)
        // so gas_used() = 21000. X-Tezos-Gas-Consumed is in EVM units (the called runtime).

        #[test]
        fn success_has_gas_consumed_header() {
            let resp = build_response(Ok(make_success(vec![])));
            assert_eq!(
                resp.headers()
                    .get(X_TEZOS_GAS_CONSUMED)
                    .and_then(|v| v.to_str().ok()),
                Some("21000")
            );
        }

        #[test]
        fn revert_has_gas_consumed_header() {
            let outcome = ExecutionOutcome {
                result: ExecutionResult::Revert {
                    logs: vec![],
                    gas: ResultGas::new(u64::MAX, 21000, 0, 0, 0),
                    output: vec![].into(),
                },
                withdrawals: vec![],
            };
            let resp = build_response(Ok(outcome));
            assert_eq!(
                resp.headers()
                    .get(X_TEZOS_GAS_CONSUMED)
                    .and_then(|v| v.to_str().ok()),
                Some("21000")
            );
        }

        #[test]
        fn halt_has_gas_consumed_header() {
            let outcome = ExecutionOutcome {
                result: ExecutionResult::Halt {
                    reason: HaltReason::OutOfGas(
                        revm::context::result::OutOfGasError::Basic,
                    ),
                    logs: vec![],
                    gas: ResultGas::new(u64::MAX, 21000, 0, 0, 0),
                },
                withdrawals: vec![],
            };
            let resp = build_response(Ok(outcome));
            assert_eq!(
                resp.headers()
                    .get(X_TEZOS_GAS_CONSUMED)
                    .and_then(|v| v.to_str().ok()),
                Some("21000")
            );
        }

        #[test]
        fn error_response_has_zero_gas_consumed() {
            let resp = build_response(Err(TezosXRuntimeError::BadRequest("err".into())));
            assert_eq!(resp.headers().get(X_TEZOS_GAS_CONSUMED).unwrap(), "0");
        }
    }

    /// Test that serve() handles zero-amount transfers correctly.
    #[test]
    fn test_serve_zero_amount_transfer() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let block_constants = BlockConstants::test_block_with_no_fees();
        let registry = StubRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x22; 20]);

        let mut journal = TezosXJournal::default();
        let request = build_serve_request(&sender, &destination, "0", vec![]);
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);
        commit_evm_journal_from_external(
            &mut host,
            &registry,
            &block_constants,
            &mut journal,
        )
        .unwrap();

        // Destination should have 0 balance (no transfer)
        let destination_account = StorageAccount::from_address(&destination).unwrap();
        let info = destination_account.info(&mut host).unwrap();
        assert_eq!(info.balance, revm::primitives::U256::ZERO);
    }

    /// Test that serve() correctly handles fractional TEZ amounts.
    #[test]
    fn test_serve_fractional_amount_transfer() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let block_constants = BlockConstants::test_block_with_no_fees();
        let registry = StubRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x22; 20]);

        // 0.5 TEZ = 500_000_000_000_000_000 wei
        let half_tez_wei = revm::primitives::U256::from(500_000_000_000_000_000u64);

        let mut journal = TezosXJournal::default();
        let request = build_serve_request(&sender, &destination, "0.5", vec![]);
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);
        commit_evm_journal_from_external(
            &mut host,
            &registry,
            &block_constants,
            &mut journal,
        )
        .unwrap();

        let destination_account = StorageAccount::from_address(&destination).unwrap();
        let info = destination_account.info(&mut host).unwrap();
        assert_eq!(info.balance, half_tez_wei);
    }

    #[test]
    fn test_serve_calls_contract() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = StubRegistry;

        let contract = Address::from_slice(&[0x33; 20]);

        // Deploy a contract that stores CALLVALUE to slot 0 and returns
        // the value from slot 0 (32 bytes).
        //   CALLVALUE      (0x34)
        //   PUSH1 0x00     (0x6000)
        //   SSTORE         (0x55)
        //   PUSH1 0x20     (0x6020)  -- return size: 32 bytes
        //   PUSH1 0x00     (0x6000)  -- return offset: 0
        //   PUSH1 0x00     (0x6000)  -- slot 0
        //   SLOAD          (0x54)    -- load slot 0
        //   PUSH1 0x00     (0x6000)  -- memory offset: 0
        //   MSTORE         (0x52)    -- store to memory
        //   RETURN         (0xF3)
        let bytecode_raw = Bytes::from_hex("34600055602060006000546000525AF3").unwrap();
        let code_hash = bytes_hash(&bytecode_raw);
        let mut contract_account = StorageAccount::from_address(&contract).unwrap();
        contract_account
            .set_info(
                &mut host,
                AccountInfo {
                    balance: revm::primitives::U256::ZERO,
                    nonce: 0,
                    code_hash,
                    account_id: None,
                    code: Some(Bytecode::new_raw(bytecode_raw.clone())),
                },
            )
            .unwrap();
        CodeStorage::add(&mut host, &bytecode_raw, Some(code_hash)).unwrap();

        // The cross-runtime call path uses gas_price = 0 and amount = 0,
        // so REVM's pre-flight balance requirement on the caller is 0 — no
        // need to fund TEZOSX_CALLER_ADDRESS (the production code in
        // `ensure_alias` doesn't either).
        let caller_addr = revm_etherlink::precompiles::constants::TEZOSX_CALLER_ADDRESS;
        let url = format!(
            "http://ethereum/{}",
            alloy_primitives::hex::encode(contract.0 .0)
        );
        let request = http::Request::builder()
            .method(http::Method::POST)
            .uri(&url)
            .header(
                tezosx_interfaces::X_TEZOS_SENDER,
                format!("0x{}", alloy_primitives::hex::encode(caller_addr.0 .0)),
            )
            .header(tezosx_interfaces::X_TEZOS_AMOUNT, "0")
            .header(tezosx_interfaces::X_TEZOS_GAS_LIMIT, u64::MAX.to_string())
            .header(tezosx_interfaces::X_TEZOS_TIMESTAMP, "0")
            .header(tezosx_interfaces::X_TEZOS_BLOCK_NUMBER, "0")
            .body(vec![])
            .unwrap();

        let mut journal = TezosXJournal::default();
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);
    }

    // ── L2-1259: HTTP method dispatch and GET-static path ────────────

    /// Build an HTTP request with an explicit method, used by the
    /// L2-1259 dispatch tests.
    fn build_serve_request_with_method(
        method: http::Method,
        sender: &Address,
        destination: &Address,
        amount: &str,
        body: Vec<u8>,
    ) -> http::Request<Vec<u8>> {
        let url = format!(
            "http://ethereum/{}",
            alloy_primitives::hex::encode(destination.0 .0)
        );
        http::Request::builder()
            .method(method)
            .uri(&url)
            .header(
                tezosx_interfaces::X_TEZOS_SENDER,
                format!("0x{}", alloy_primitives::hex::encode(sender.0 .0)),
            )
            .header(tezosx_interfaces::X_TEZOS_AMOUNT, amount)
            .header(tezosx_interfaces::X_TEZOS_GAS_LIMIT, u64::MAX.to_string())
            .header(tezosx_interfaces::X_TEZOS_TIMESTAMP, "1")
            .header(tezosx_interfaces::X_TEZOS_BLOCK_NUMBER, "1")
            .body(body)
            .unwrap()
    }

    /// Deploy a bytecode at `address` (test helper).
    fn deploy_at(host: &mut MockKernelHost, address: &Address, bytecode_raw: Bytes) {
        let code_hash = bytes_hash(&bytecode_raw);
        let mut account = StorageAccount::from_address(address).unwrap();
        account
            .set_info(
                host,
                AccountInfo {
                    balance: revm::primitives::U256::ZERO,
                    nonce: 0,
                    code_hash,
                    account_id: None,
                    code: Some(Bytecode::new_raw(bytecode_raw.clone())),
                },
            )
            .unwrap();
        CodeStorage::add(host, &bytecode_raw, Some(code_hash)).unwrap();
    }

    /// Read `slot` from `address`'s storage.
    fn read_slot(
        host: &mut MockKernelHost,
        address: &Address,
        slot: revm::primitives::U256,
    ) -> revm::primitives::U256 {
        let account = StorageAccount::from_address(address).unwrap();
        account.get_storage(host, &slot).unwrap_or_default()
    }

    #[test]
    fn test_serve_unsupported_method_returns_405() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = StubRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x22; 20]);

        for method in [
            http::Method::PUT,
            http::Method::DELETE,
            http::Method::PATCH,
            http::Method::HEAD,
            http::Method::OPTIONS,
        ] {
            let mut journal = TezosXJournal::default();
            let request = build_serve_request_with_method(
                method.clone(),
                &sender,
                &destination,
                "0",
                vec![],
            );
            let resp = runtime.serve(&registry, &mut host, &mut journal, request);
            assert_eq!(
                resp.status(),
                http::StatusCode::METHOD_NOT_ALLOWED,
                "method {method} should be rejected with 405",
            );
        }
    }

    #[test]
    fn test_static_call_with_nonzero_amount_is_rejected() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = StubRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x22; 20]);

        let mut journal = TezosXJournal::default();
        let request = build_serve_request_with_method(
            http::Method::GET,
            &sender,
            &destination,
            "1",
            vec![],
        );
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::BAD_REQUEST);
    }

    #[test]
    fn test_static_call_returns_view_result() {
        // Bytecode that returns the constant 0x42 left-padded to 32 bytes.
        // No state mutation: a pure Solidity-`view`-style read.
        //
        //   PUSH1 0x42     (0x6042)  -- value
        //   PUSH1 0x00     (0x6000)  -- mem offset
        //   MSTORE         (0x52)
        //   PUSH1 0x20     (0x6020)  -- return size
        //   PUSH1 0x00     (0x6000)  -- return offset
        //   RETURN         (0xF3)
        let bytecode_raw = Bytes::from_hex("604260005260206000F3").unwrap();

        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = StubRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x33; 20]);
        deploy_at(&mut host, &destination, bytecode_raw);

        let mut journal = TezosXJournal::default();
        let request = build_serve_request_with_method(
            http::Method::GET,
            &sender,
            &destination,
            "0",
            vec![],
        );
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::OK);
        // Body is 32 bytes ending in 0x42.
        let body = resp.body();
        assert_eq!(body.len(), 32);
        assert_eq!(body[31], 0x42);
        for &b in &body[..31] {
            assert_eq!(b, 0);
        }
    }

    #[test]
    fn test_static_call_rejects_state_mutation() {
        // A `GET` runs the EVM transaction with `is_static = true` on
        // the top-level frame, so REVM enforces the standard
        // `STATICCALL` contract: any state-mutating opcode (`SSTORE`,
        // `LOG*`, `CREATE*`, `SELFDESTRUCT`, value-bearing `CALL`)
        // halts the call with `StateChangeDuringStaticCall`. We
        // surface that as a catchable 400 BadRequest so the
        // originating runtime can revert just this call.
        //
        // Bytecode: SSTORE 1 into slot 0 then RETURN — the SSTORE
        // alone is enough to trip the static check.
        //   PUSH1 0x01     (0x6001)
        //   PUSH1 0x00     (0x6000)
        //   SSTORE         (0x55)
        //   PUSH1 0x00     (0x6000)
        //   PUSH1 0x00     (0x6000)
        //   RETURN         (0xF3)
        let bytecode_raw = Bytes::from_hex("60016000556000600060F3").unwrap();

        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let block_constants = BlockConstants::test_block_with_no_fees();
        let registry = StubRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x44; 20]);
        deploy_at(&mut host, &destination, bytecode_raw);

        // Pre-condition: slot 0 starts at zero.
        assert_eq!(
            read_slot(&mut host, &destination, revm::primitives::U256::ZERO),
            revm::primitives::U256::ZERO,
        );

        let mut journal = TezosXJournal::default();
        let request = build_serve_request_with_method(
            http::Method::GET,
            &sender,
            &destination,
            "0",
            vec![],
        );
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        // The SSTORE attempt halts the EVM; we surface it as 400.
        assert_eq!(resp.status(), http::StatusCode::BAD_REQUEST);

        // Top-level commit applies whatever the journal accumulated.
        // Since REVM aborted the call on the SSTORE attempt, slot 0
        // stays at zero.
        commit_evm_journal_from_external(
            &mut host,
            &registry,
            &block_constants,
            &mut journal,
        )
        .unwrap();

        assert_eq!(
            read_slot(&mut host, &destination, revm::primitives::U256::ZERO),
            revm::primitives::U256::ZERO,
        );
    }

    #[test]
    fn test_static_call_revert_surfaces_as_bad_request() {
        // Bytecode that REVERTs unconditionally with a 1-byte payload.
        //   PUSH1 0x42     (0x6042)
        //   PUSH1 0x00     (0x6000)
        //   MSTORE         (0x52)
        //   PUSH1 0x20     (0x6020)
        //   PUSH1 0x00     (0x6000)
        //   REVERT         (0xFD)
        let bytecode_raw = Bytes::from_hex("604260005260206000FD").unwrap();

        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = StubRegistry;

        let sender = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x55; 20]);
        deploy_at(&mut host, &destination, bytecode_raw);

        let mut journal = TezosXJournal::default();
        let request = build_serve_request_with_method(
            http::Method::GET,
            &sender,
            &destination,
            "0",
            vec![],
        );
        let resp = runtime.serve(&registry, &mut host, &mut journal, request);
        assert_eq!(resp.status(), http::StatusCode::BAD_REQUEST);
    }
}

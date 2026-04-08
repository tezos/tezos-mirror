// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use http::StatusCode;
use mir::ast::big_map::BigMapId;
use primitive_types::U256;
use std::collections::BTreeMap;
use tezos_crypto_rs::{
    blake2b,
    hash::{BlockHash, ChainId, ContractKt1Hash, OperationHash, UnknownSignature},
};
// UnknownSignature has a private constructor; use try_from to build one.
#[allow(dead_code)]
const ZERO_SIGNATURE: [u8; 64] = [0u8; 64];
use tezos_data_encoding::{
    enc::BinWriter,
    types::{Narith, Zarith},
};
use tezos_execution::{
    account_storage::{TezlinkAccount, TezosOriginatedAccount},
    context::Context,
    cross_runtime_transfer,
    mir_ctx::{OperationCtx, TcCtx},
    OriginationNonce, TezlinkOperationGas,
};
use tezos_protocol::contract::Contract;
use tezos_smart_rollup::types::PublicKeyHash;
use tezos_smart_rollup_host::storage::StorageV1;
// `Parameters` could come from `tezos_protocol::operation`, but we also need
// `tezos_tezlink` for types that live only there (OperationHash, BlockNumber,
// TransferError). To avoid the dependency altogether, those types would need
// to be moved to a shared crate.
use tezos_tezlink::{
    block::AppliedOperation,
    operation::{ManagerOperation, ManagerOperationContent, Parameters, TransferContent},
    operation_result::{
        ApplyOperationError, ApplyOperationErrors, BacktrackedResult, ContentResult,
        EventContent, EventSuccess, InternalContentWithMetadata, InternalOperationSum,
        OperationBatchWithMetadata, OperationDataAndMetadata, OperationResult,
        OperationResultSum, OperationWithMetadata, TransferError, TransferSuccess,
        TransferTarget,
    },
};
use tezosx_interfaces::{
    CrossRuntimeContext, Registry, RuntimeInterface, TezosXRuntimeError,
    X_TEZOS_GAS_CONSUMED,
};
use tezosx_journal::TezosXJournal;

use tezos_evm_runtime::safe_storage::ETHERLINK_SAFE_STORAGE_ROOT_PATH;

const NULL_PKH: &str = "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU";

use crate::{
    account::{get_tezos_account_info_or_init, narith_to_u256, set_tezos_account_info},
    context::TezosRuntimeContext,
};

pub struct TezosRuntime(pub ChainId);

pub mod account;
pub mod alias_forwarder;
pub mod context;
pub mod headers;
pub mod url;

/// Build an HTTP response from the result of [`execute_request`].
///
/// Request-level errors are turned into HTTP error responses:
/// - `Ok` → 200
/// - `BadRequest` → 400
/// - `NotFound` → 404
///
/// All other errors propagate as `Err` — they indicate infrastructure
/// problems that cannot be meaningfully represented as HTTP responses.
///
// TODO: Review `Custom` errors in `execute_request` to extract more
// structured variants (and corresponding HTTP status codes) where
// possible.
fn build_response(
    result: Result<TransferSuccess, TezosXRuntimeError>,
    consumed_milligas: u64,
) -> Result<http::Response<Vec<u8>>, TezosXRuntimeError> {
    let gas_header = consumed_milligas.to_string();
    let (status, body) = match result {
        Ok(response) => (
            StatusCode::OK,
            response.storage.map(|s| s.0).unwrap_or_default(),
        ),
        Err(TezosXRuntimeError::BadRequest(msg)) => {
            (StatusCode::BAD_REQUEST, msg.into_bytes())
        }
        Err(TezosXRuntimeError::NotFound(msg)) => {
            (StatusCode::NOT_FOUND, msg.into_bytes())
        }
        Err(e) => (
            StatusCode::INTERNAL_SERVER_ERROR,
            format!("{e:?}").into_bytes(),
        ),
    };
    http::Response::builder()
        .status(status)
        .header(X_TEZOS_GAS_CONSUMED, &gas_header)
        .body(body)
        .map_err(|e| TezosXRuntimeError::Custom(format!("Failed to build response: {e}")))
}

/// Build a serialized two-step receipt for an incoming CRAC (EVM → Michelson).
///
/// The RFC (CRAC Derived Block Contents) specifies the receipt structure:
///
/// **Top-level**: handler (tz1) → source_alias (alias(E_0)), Applied
///   - **Internal op #0** (if CRAC event): CRAC event with CRAC-ID
///   - **Internal op #1**: sender_alias (alias(E_1)) → target, Applied(transfer result)
///   - **Internal ops 2..N**: further ops from Michelson execution
///
/// `source_contract` is alias(E_0) — the alias of the EVM tx originator
/// (from `X-Tezos-Source`). It is the destination of the top-level op.
///
/// `sender_contract` is alias(E_1) — the alias of the immediate EVM caller
/// (from `X-Tezos-Sender`). It is the sender of the internal op.
///
/// Returns the `AppliedOperation` that the block builder will store
/// in the Michelson runtime block.
#[allow(clippy::too_many_arguments, dead_code)]
fn build_crac_receipt(
    null_pkh: &PublicKeyHash,
    source_contract: &Contract,
    sender_contract: &Contract,
    amount: &Narith,
    destination: &Contract,
    parameters: &Parameters,
    target: TransferTarget,
    internal_receipts: Vec<InternalOperationSum>,
    crac_id: Option<&str>,
) -> Result<AppliedOperation, TezosXRuntimeError> {
    // Combine: [CRAC event, alias(E_1)→target, ...further internal ops]
    // Per RFC, the CRAC event is always the first internal operation (#0).
    let mut all_internal = Vec::new();

    if let Some(id) = crac_id {
        use mir::{
            ast::annotations::NO_ANNS, ast::micheline::Micheline, ast::Entrypoint, lexer,
        };
        let ty = Micheline::App(lexer::Prim::string, &[], NO_ANNS).encode();
        let payload = Micheline::from(id.to_string()).encode();
        all_internal.push(InternalOperationSum::Event(InternalContentWithMetadata {
            content: EventContent {
                tag: Some(Entrypoint::from_string_unchecked("crac".into())),
                payload: Some(payload.into()),
                ty: ty.into(),
            },
            sender: Contract::Implicit(null_pkh.clone()),
            nonce: 0,
            result: ContentResult::Applied(EventSuccess {
                consumed_milligas: Narith(0u64.into()),
            }),
        }));
    }

    // The transfer nonce starts after the event (if present).
    let transfer_nonce = u16::try_from(all_internal.len()).unwrap_or(u16::MAX);
    let transfer_internal = InternalOperationSum::Transfer(InternalContentWithMetadata {
        sender: sender_contract.clone(),
        nonce: transfer_nonce,
        content: TransferContent {
            amount: amount.clone(),
            destination: destination.clone(),
            parameters: parameters.clone(),
        },
        result: ContentResult::Applied(target),
    });
    all_internal.push(transfer_internal);
    all_internal.extend(internal_receipts);

    // Top-level result: handler → alias(E_0) (applied, with all internals nested).
    // TransferSuccess::default() is intentional — this is a synthetic wrapper;
    // actual consumed_milligas, storage_size, etc. are in the internal ops.
    let top_level_result = OperationResult {
        balance_updates: vec![],
        result: ContentResult::Applied(TransferTarget::from(TransferSuccess::default())),
        internal_operation_results: all_internal,
    };

    let signature =
        UnknownSignature::try_from(ZERO_SIGNATURE.as_slice()).map_err(|e| {
            TezosXRuntimeError::Custom(format!("Failed to build zero signature: {e}"))
        })?;

    let op_data =
        OperationDataAndMetadata::OperationWithMetadata(OperationBatchWithMetadata {
            operations: vec![OperationWithMetadata {
                content: ManagerOperationContent::Transaction(ManagerOperation {
                    source: null_pkh.clone(),
                    fee: Narith(0u64.into()),
                    counter: Narith(0u64.into()),
                    gas_limit: Narith(0u64.into()),
                    storage_limit: Narith(0u64.into()),
                    operation: TransferContent {
                        // The top-level op is a synthetic container
                        // (handler → alias(E_0)); real amount is on the
                        // internal op, so we zero it like fee/gas_limit.
                        amount: Narith(0u64.into()),
                        destination: source_contract.clone(),
                        // Only the internal op carries the real target
                        // parameters.
                        parameters: Parameters::default(),
                    },
                }),
                receipt: OperationResultSum::Transfer(top_level_result),
            }],
            signature,
        });

    Ok(AppliedOperation {
        hash: OperationHash::default(),
        branch: BlockHash::default(),
        op_and_receipt: op_data,
    })
}

/// Build a failed CRAC receipt (RFC Example 4).
///
/// When `cross_runtime_transfer` fails, we still need a receipt in the
/// Michelson block so indexers see the failed CRAC.  The top-level
/// operation has `status: failed` and carries any partial internal
/// operations (with backtracked / failed / skipped statuses) so
/// indexers can see what was attempted.
#[allow(clippy::too_many_arguments, dead_code)]
fn build_failed_crac_receipt(
    null_pkh: &PublicKeyHash,
    source_contract: &Contract,
    sender_contract: &Contract,
    amount: &Narith,
    destination: &Contract,
    parameters: &Parameters,
    error: TransferError,
    internal_receipts: Vec<InternalOperationSum>,
    crac_id: Option<&str>,
) -> Result<AppliedOperation, TezosXRuntimeError> {
    // Per RFC, the CRAC event is always first, even on failure.
    // Since the downstream transfer failed, the event is backtracked
    // (matching Tezos protocol semantics where all applied internal ops
    // preceding a failure are backtracked).
    let mut all_internal = Vec::new();
    if let Some(id) = crac_id {
        use mir::{
            ast::annotations::NO_ANNS, ast::micheline::Micheline, ast::Entrypoint, lexer,
        };
        let ty = Micheline::App(lexer::Prim::string, &[], NO_ANNS).encode();
        let payload = Micheline::from(id.to_string()).encode();
        all_internal.push(InternalOperationSum::Event(InternalContentWithMetadata {
            content: EventContent {
                tag: Some(Entrypoint::from_string_unchecked("crac".into())),
                payload: Some(payload.into()),
                ty: ty.into(),
            },
            sender: Contract::Implicit(null_pkh.clone()),
            nonce: 0,
            result: ContentResult::BackTracked(BacktrackedResult {
                errors: None,
                result: EventSuccess {
                    consumed_milligas: Narith(0u64.into()),
                },
            }),
        }));
    }

    // Per RFC, include the failed transfer (alias(E_1) → target) so
    // indexers can see which contract call was attempted.
    let transfer_nonce = u16::try_from(all_internal.len()).unwrap_or(u16::MAX);
    all_internal.push(InternalOperationSum::Transfer(
        InternalContentWithMetadata {
            sender: sender_contract.clone(),
            nonce: transfer_nonce,
            content: TransferContent {
                amount: amount.clone(),
                destination: destination.clone(),
                parameters: parameters.clone(),
            },
            result: ContentResult::Failed(ApplyOperationErrors::from(
                ApplyOperationError::Transfer(error.clone()),
            )),
        },
    ));
    // Internal receipts already have block-global nonces from execution.
    all_internal.extend(internal_receipts);

    let top_level_result = OperationResult {
        balance_updates: vec![],
        result: ContentResult::Failed(ApplyOperationErrors::from(
            ApplyOperationError::Transfer(error),
        )),
        internal_operation_results: all_internal,
    };

    let signature =
        UnknownSignature::try_from(ZERO_SIGNATURE.as_slice()).map_err(|e| {
            TezosXRuntimeError::Custom(format!("Failed to build zero signature: {e}"))
        })?;

    let op_data =
        OperationDataAndMetadata::OperationWithMetadata(OperationBatchWithMetadata {
            operations: vec![OperationWithMetadata {
                content: ManagerOperationContent::Transaction(ManagerOperation {
                    source: null_pkh.clone(),
                    fee: Narith(0u64.into()),
                    counter: Narith(0u64.into()),
                    gas_limit: Narith(0u64.into()),
                    storage_limit: Narith(0u64.into()),
                    operation: TransferContent {
                        amount: Narith(0u64.into()),
                        destination: source_contract.clone(),
                        parameters: Parameters::default(),
                    },
                }),
                receipt: OperationResultSum::Transfer(top_level_result),
            }],
            signature,
        });

    Ok(AppliedOperation {
        hash: OperationHash::default(),
        branch: BlockHash::default(),
        op_and_receipt: op_data,
    })
}

/// Execute a cross-runtime request: parse the URL, extract parameters,
/// and call the Michelson VM. Returns the response body on success.
///
/// This is the core logic behind [`TezosRuntime::serve`], separated so
/// that `serve` only handles the error-to-HTTP-status mapping.
///
/// `consumed_milligas` is an output parameter rather than part of the
/// return type so that early `?` returns keep their ergonomics, since
/// this result is used as-is to build the http reponse.
fn execute_request<Host>(
    chain_id: &ChainId,
    registry: &impl Registry,
    host: &mut Host,
    journal: &mut TezosXJournal,
    request: http::Request<Vec<u8>>,
    consumed_milligas: &mut u64,
) -> Result<TransferSuccess, TezosXRuntimeError>
where
    Host: StorageV1,
{
    let parsed = url::parse_tezos_url(request.uri())?;
    let hdrs = headers::parse_request_headers(request.headers())?;

    let body = request.into_body();
    // An empty body means "no parameters" which defaults to Micheline Unit.
    // This is required for implicit account transfers where the Michelson VM
    // checks that param == Unit.
    let value = if body.is_empty() {
        mir::ast::micheline::Micheline::from(()).encode()
    } else {
        body
    };
    let parameters = Parameters {
        entrypoint: parsed.entrypoint,
        value,
    };

    let context = TezosRuntimeContext::from_root(&ETHERLINK_SAFE_STORAGE_ROOT_PATH)?;

    let sender_account = context.originated_from_kt1(&hdrs.sender).map_err(|e| {
        TezosXRuntimeError::Custom(format!("Failed to fetch sender account: {e:?}"))
    })?;

    let source_pkh = PublicKeyHash::from_b58check(NULL_PKH).map_err(|e| {
        TezosXRuntimeError::ConversionError(format!("Failed to parse null address: {e}"))
    })?;
    let source_account =
        context
            .implicit_from_public_key_hash(&source_pkh)
            .map_err(|e| {
                TezosXRuntimeError::Custom(format!(
                    "Failed to fetch source account: {e:?}"
                ))
            })?;

    let mut gas = TezlinkOperationGas::start_milligas(hdrs.gas_limit)
        .map_err(|e| TezosXRuntimeError::Custom(format!("Failed to start gas: {e:?}")))?;
    let mut next_temp_id = BigMapId {
        value: Zarith(0.into()),
    };
    let mut tc_ctx = TcCtx {
        host,
        context: &context,
        operation_gas: &mut gas,
        big_map_diff: BTreeMap::new(),
        next_temporary_id: &mut next_temp_id,
    };
    let mut nonce = OriginationNonce::initial(OperationHash::default());
    let mut counter = 0u128;
    let mut operation_ctx = OperationCtx {
        source: &source_account,
        origination_nonce: &mut nonce,
        counter: &mut counter,
        level: &hdrs.block_number,
        now: &hdrs.timestamp,
        chain_id,
    };
    let parser = mir::parser::Parser::new();

    // TODO: thread block-global nonce counter from the caller.
    let mut nonce_counter: u16 = 0;
    let result = cross_runtime_transfer(
        &mut tc_ctx,
        &mut operation_ctx,
        registry,
        journal,
        &sender_account,
        &hdrs.amount,
        &parsed.destination,
        &parameters,
        &parser,
        &mut nonce_counter,
    )
    .map(|s| s.target.into())
    .map_err(|e| TezosXRuntimeError::Custom(format!("{:?}", e.error)));

    *consumed_milligas = gas
        .get_and_reset_milligas_consumed()
        .map_err(|_| TezosXRuntimeError::OutOfGas)?
        .0
        .clone()
        .try_into()
        .map_err(|_| {
            TezosXRuntimeError::Custom("consumed milligas overflows u64".to_string())
        })?;

    result
}

impl RuntimeInterface for TezosRuntime {
    fn generate_alias<Host>(
        &self,
        _registry: &impl Registry,
        host: &mut Host,
        _journal: &mut TezosXJournal,
        native_address: &str,
        _context: CrossRuntimeContext,
        gas_remaining: u64,
    ) -> Result<(String, u64), TezosXRuntimeError>
    where
        Host: StorageV1,
    {
        // Gas costs in milligas, charged incrementally so we fail early.
        let mut remaining = gas_remaining;
        let mut consume = |cost: u64| -> Result<(), TezosXRuntimeError> {
            remaining = remaining.checked_sub(cost).ok_or_else(|| {
                TezosXRuntimeError::Custom(
                    "Out of gas during alias generation".to_string(),
                )
            })?;
            Ok(())
        };

        // BLAKE2b-160 hash: 430 + (size/8 + size) milligas
        consume(477)?;
        let kt1 = ContractKt1Hash::from(blake2b::digest_160(native_address.as_bytes()));

        // Context load + account lookup
        consume(2_100)?; // cold storage read
        let context = TezosRuntimeContext::from_root(&ETHERLINK_SAFE_STORAGE_ROOT_PATH)?;
        let account = context.originated_from_kt1(&kt1)?;

        // Decode forwarder code
        let code = alias_forwarder::forwarder_code().map_err(|e| {
            TezosXRuntimeError::Custom(format!(
                "Failed to decode forwarder code from hex: {e}"
            ))
        })?;
        let storage = alias_forwarder::forwarder_storage(native_address);

        // Contract init: code + storage writes
        consume(100_000)?; // manager_operation overhead + origination
        account.init(host, &code, &storage).map_err(|e| {
            TezosXRuntimeError::Custom(format!(
                "Failed to initialize alias forwarder contract: {e}"
            ))
        })?;

        // Balance write
        consume(2_560)?; // storage write
        account.set_balance(host, &0u64.into()).map_err(|e| {
            TezosXRuntimeError::Custom(format!("Failed to set alias balance: {e}"))
        })?;

        Ok((kt1.to_base58_check(), remaining))
    }

    /// Execute a cross-runtime call where the sender's balance was already
    /// debited by the calling runtime (e.g. EVM gateway). This handles both
    /// implicit and originated destinations, including Michelson code execution
    /// and internal operations.
    fn serve<Host>(
        &self,
        registry: &impl Registry,
        host: &mut Host,
        journal: &mut TezosXJournal,
        request: http::Request<Vec<u8>>,
    ) -> Result<http::Response<Vec<u8>>, TezosXRuntimeError>
    where
        Host: StorageV1,
    {
        // Default to max: if execute_request fails before writing the
        // actual value (early setup error), the caller sees full gas
        // consumption rather than a free call.
        let mut consumed_milligas = u64::MAX;
        let result = execute_request(
            &self.0,
            registry,
            host,
            journal,
            request,
            &mut consumed_milligas,
        );
        build_response(result, consumed_milligas)
    }

    fn host(&self) -> &'static str {
        "tezos"
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
    fn get_balance(
        &self,
        _host: &mut impl StorageV1,
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
    pub fn new(chain_id: ChainId) -> Self {
        Self(chain_id)
    }

    pub fn add_balance(
        host: &mut impl StorageV1,
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
        host: &impl StorageV1,
        kt1: &ContractKt1Hash,
    ) -> Result<U256, TezosXRuntimeError> {
        let context = TezosRuntimeContext::from_root(&ETHERLINK_SAFE_STORAGE_ROOT_PATH)?;
        let originated_account = context.originated_from_kt1(kt1)?;
        let balance = originated_account.balance(host)?;
        narith_to_u256(&balance)
    }
}

#[cfg(test)]
mod tests {
    use tezos_data_encoding::types::Narith;

    use super::*;

    fn make_success(storage: Option<Vec<u8>>) -> TransferSuccess {
        make_success_with_milligas(storage, 0)
    }

    fn make_success_with_milligas(
        storage: Option<Vec<u8>>,
        milligas: u64,
    ) -> TransferSuccess {
        TransferSuccess {
            storage: storage.map(Into::into),
            balance_updates: vec![],
            ticket_receipt: vec![],
            originated_contracts: vec![],
            consumed_milligas: Narith(milligas.into()),
            storage_size: Zarith(0.into()),
            paid_storage_size_diff: Zarith(0.into()),
            allocated_destination_contract: false,
            lazy_storage_diff: None,
            address_registry_diff: vec![],
        }
    }

    #[test]
    fn build_response_success() {
        let resp = build_response(Ok(make_success(Some(b"hello".to_vec()))), 0).unwrap();
        assert_eq!(resp.status(), StatusCode::OK);
        assert_eq!(resp.body(), b"hello");
    }

    #[test]
    fn build_response_success_empty_body() {
        let resp = build_response(Ok(make_success(None)), 0).unwrap();
        assert_eq!(resp.status(), StatusCode::OK);
        assert!(resp.body().is_empty());
    }

    #[test]
    fn build_response_bad_request() {
        let resp =
            build_response(Err(TezosXRuntimeError::BadRequest("invalid URL".into())), 0)
                .unwrap();
        assert_eq!(resp.status(), StatusCode::BAD_REQUEST);
        assert!(String::from_utf8_lossy(resp.body()).contains("invalid URL"));
    }

    #[test]
    fn build_response_not_found() {
        let resp =
            build_response(Err(TezosXRuntimeError::NotFound("KT1 not found".into())), 0)
                .unwrap();
        assert_eq!(resp.status(), StatusCode::NOT_FOUND);
        assert!(String::from_utf8_lossy(resp.body()).contains("KT1 not found"));
    }

    #[test]
    fn build_response_success_has_gas_consumed_header() {
        // consumed_milligas = 0 → reported as 0
        let resp = build_response(Ok(make_success(None)), 0).unwrap();
        assert_eq!(
            resp.headers()
                .get(X_TEZOS_GAS_CONSUMED)
                .and_then(|v| v.to_str().ok()),
            Some("0")
        );
    }

    #[test]
    fn build_response_gas_consumed_reports_milligas() {
        // 5000 milligas → reported as 5000
        let resp =
            build_response(Ok(make_success_with_milligas(None, 5000)), 5000).unwrap();
        assert_eq!(
            resp.headers()
                .get(X_TEZOS_GAS_CONSUMED)
                .and_then(|v| v.to_str().ok()),
            Some("5000")
        );
    }

    #[test]
    fn build_response_error_has_no_gas_consumed_header() {
        let resp =
            build_response(Err(TezosXRuntimeError::BadRequest("err".into())), 0).unwrap();
        assert_eq!(
            resp.headers()
                .get(X_TEZOS_GAS_CONSUMED)
                .and_then(|v| v.to_str().ok()),
            Some("0")
        );
    }

    // --- generate_alias tests ---

    use tezos_crypto_rs::hash::ChainId;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezosx_interfaces::RuntimeId;

    struct StubRegistry;

    impl Registry for StubRegistry {
        fn generate_alias<Host>(
            &self,
            _host: &mut Host,
            _journal: &mut TezosXJournal,
            _native_address: &str,
            runtime_id: RuntimeId,
            _context: CrossRuntimeContext,
            _gas_remaining: u64,
        ) -> Result<(String, u64), TezosXRuntimeError>
        where
            Host: StorageV1,
        {
            Err(TezosXRuntimeError::RuntimeNotFound(runtime_id))
        }

        fn address_from_string(
            &self,
            _address_str: &str,
            runtime_id: RuntimeId,
        ) -> Result<Vec<u8>, TezosXRuntimeError> {
            Err(TezosXRuntimeError::RuntimeNotFound(runtime_id))
        }

        fn serve<Host>(
            &self,
            _host: &mut Host,
            _journal: &mut TezosXJournal,
            _request: http::Request<Vec<u8>>,
        ) -> Result<http::Response<Vec<u8>>, TezosXRuntimeError>
        where
            Host: StorageV1,
        {
            Err(TezosXRuntimeError::Custom("stub".into()))
        }
    }

    fn test_context() -> CrossRuntimeContext {
        CrossRuntimeContext {
            gas_limit: 1_000_000,
            timestamp: U256::from(0),
            block_number: U256::from(0),
        }
    }

    fn test_runtime() -> TezosRuntime {
        TezosRuntime::new(ChainId::default())
    }

    #[test]
    fn generate_alias_returns_valid_kt1_string() {
        let mut host = MockKernelHost::default();
        let mut journal = TezosXJournal::default();
        let runtime = test_runtime();

        let alias = runtime
            .generate_alias(
                &StubRegistry,
                &mut host,
                &mut journal,
                "0x1234567890abcdef1234567890abcdef12345678",
                test_context(),
                1_000_000,
            )
            .expect("generate_alias should succeed");

        // The alias should be a valid KT1 base58check string
        assert!(
            alias.0.starts_with("KT1"),
            "Alias should be a KT1 address: {}",
            alias.0
        );
    }

    #[test]
    fn generate_alias_deploys_forwarder_code() {
        let mut host = MockKernelHost::default();
        let mut journal = TezosXJournal::default();
        let runtime = test_runtime();
        let evm_address = "0x1234567890abcdef1234567890abcdef12345678";

        runtime
            .generate_alias(
                &StubRegistry,
                &mut host,
                &mut journal,
                evm_address,
                test_context(),
                1_000_000,
            )
            .expect("generate_alias should succeed");

        let kt1 = ContractKt1Hash::from(blake2b::digest_160(evm_address.as_bytes()));
        let context =
            TezosRuntimeContext::from_root(&ETHERLINK_SAFE_STORAGE_ROOT_PATH).unwrap();
        let account = context.originated_from_kt1(&kt1).unwrap();

        let code = account.code(&host).unwrap();
        match code {
            tezos_execution::account_storage::Code::Code(bytes) => {
                assert_eq!(
                    bytes,
                    alias_forwarder::forwarder_code()
                        .expect("FORWARDER_CODE_HEX is a valid hex constant")
                );
            }
            _ => panic!("Expected regular code, not enshrined"),
        }
    }

    #[test]
    fn generate_alias_stores_evm_address_in_storage() {
        let mut host = MockKernelHost::default();
        let mut journal = TezosXJournal::default();
        let runtime = test_runtime();
        let evm_address = "0xdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef";

        runtime
            .generate_alias(
                &StubRegistry,
                &mut host,
                &mut journal,
                evm_address,
                test_context(),
                1_000_000,
            )
            .expect("generate_alias should succeed");

        let kt1 = ContractKt1Hash::from(blake2b::digest_160(evm_address.as_bytes()));
        let context =
            TezosRuntimeContext::from_root(&ETHERLINK_SAFE_STORAGE_ROOT_PATH).unwrap();
        let account = context.originated_from_kt1(&kt1).unwrap();

        let storage = account.storage(&host).unwrap();
        let expected = alias_forwarder::forwarder_storage(evm_address);
        assert_eq!(storage, expected);
    }

    #[test]
    fn generate_alias_sets_zero_balance() {
        let mut host = MockKernelHost::default();
        let mut journal = TezosXJournal::default();
        let runtime = test_runtime();
        let evm_address = "0xabcdef";

        runtime
            .generate_alias(
                &StubRegistry,
                &mut host,
                &mut journal,
                evm_address,
                test_context(),
                1_000_000,
            )
            .expect("generate_alias should succeed");

        let kt1 = ContractKt1Hash::from(blake2b::digest_160(evm_address.as_bytes()));
        let balance = TezosRuntime::get_originated_account_balance(&host, &kt1)
            .expect("should read balance");
        assert_eq!(balance, U256::zero());
    }

    #[test]
    fn generate_alias_is_deterministic() {
        let mut host1 = MockKernelHost::default();
        let mut host2 = MockKernelHost::default();
        let mut journal = TezosXJournal::default();
        let runtime = test_runtime();
        let evm_address = "0x1111111111111111111111111111111111111111";

        let alias1 = runtime
            .generate_alias(
                &StubRegistry,
                &mut host1,
                &mut journal,
                evm_address,
                test_context(),
                1_000_000,
            )
            .unwrap();
        let alias2 = runtime
            .generate_alias(
                &StubRegistry,
                &mut host2,
                &mut journal,
                evm_address,
                test_context(),
                1_000_000,
            )
            .unwrap();

        assert_eq!(alias1.0, alias2.0);
    }

    #[test]
    fn generate_alias_different_addresses_produce_different_aliases() {
        let mut host = MockKernelHost::default();
        let mut journal = TezosXJournal::default();
        let runtime = test_runtime();

        let alias1 = runtime
            .generate_alias(
                &StubRegistry,
                &mut host,
                &mut journal,
                "0x1111111111111111111111111111111111111111",
                test_context(),
                1_000_000,
            )
            .unwrap();
        let alias2 = runtime
            .generate_alias(
                &StubRegistry,
                &mut host,
                &mut journal,
                "0x2222222222222222222222222222222222222222",
                test_context(),
                1_000_000,
            )
            .unwrap();

        assert_ne!(alias1.0, alias2.0);
    }
}

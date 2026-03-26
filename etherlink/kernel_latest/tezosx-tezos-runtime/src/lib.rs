// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use http::StatusCode;
use mir::ast::big_map::BigMapId;
use primitive_types::U256;
use std::collections::BTreeMap;
use tezos_crypto_rs::{
    blake2b, hash::ChainId, hash::ContractKt1Hash, hash::HashTrait, hash::OperationHash,
};
use tezos_data_encoding::{enc::BinWriter, types::Zarith};
use tezos_evm_logging::Logging;
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
use tezos_tezlink::{operation::Parameters, operation_result::TransferSuccess};
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
) -> Result<http::Response<Vec<u8>>, TezosXRuntimeError> {
    let builder_result = match result {
        Ok(response) => {
            // X-Tezos-Gas-Consumed is in Tezos milligas.
            // The calling runtime is responsible for converting to its own units.
            let consumed_milligas: u64 = response
                .consumed_milligas
                .0
                .clone()
                .try_into()
                .map_err(|_| {
                    TezosXRuntimeError::Custom(
                        "consumed milligas overflows u64".to_string(),
                    )
                })?;
            // TODO: Do something meaningful with the rest of the response
            http::Response::builder()
                .status(StatusCode::OK)
                .header(X_TEZOS_GAS_CONSUMED, &consumed_milligas.to_string())
                .body(response.storage.unwrap_or(vec![]))
        }
        Err(TezosXRuntimeError::BadRequest(msg)) => http::Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(msg.into_bytes()),
        Err(TezosXRuntimeError::NotFound(msg)) => http::Response::builder()
            .status(StatusCode::NOT_FOUND)
            .body(msg.into_bytes()),
        Err(e) => return Err(e),
    };
    builder_result.or_else(|_| {
        http::Response::builder()
            .status(StatusCode::INTERNAL_SERVER_ERROR)
            .body(b"internal server error".to_vec())
            .map_err(|e| {
                TezosXRuntimeError::Custom(format!("Failed to build response: {e}"))
            })
    })
}

/// Execute a cross-runtime request: parse the URL, extract parameters,
/// and call the Michelson VM. Returns the response body on success.
///
/// This is the core logic behind [`TezosRuntime::serve`], separated so
/// that `serve` only handles the error-to-HTTP-status mapping.
fn execute_request<Host>(
    registry: &impl Registry,
    host: &mut Host,
    journal: &mut TezosXJournal,
    request: http::Request<Vec<u8>>,
) -> Result<TransferSuccess, TezosXRuntimeError>
where
    Host: StorageV1 + Logging,
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

    let source_pkh = hdrs.source.ok_or_else(|| {
        TezosXRuntimeError::HeaderError("X-Tezos-Source header missing".into())
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
    let chain_id = ChainId::try_from_bytes(&[0u8; 4]).unwrap();
    let mut nonce = OriginationNonce::initial(OperationHash::default());
    let mut counter = 0u128;
    let mut operation_ctx = OperationCtx {
        source: &source_account,
        origination_nonce: &mut nonce,
        counter: &mut counter,
        level: &hdrs.block_number,
        now: &hdrs.timestamp,
        chain_id: &chain_id,
    };
    let parser = mir::parser::Parser::new();

    cross_runtime_transfer(
        &mut tc_ctx,
        &mut operation_ctx,
        registry,
        journal,
        &sender_account,
        &hdrs.amount,
        &parsed.destination,
        &parameters,
        &parser,
    )
    .map(|s| s.into())
    .map_err(|e| TezosXRuntimeError::Custom(format!("{e}")))
}

impl RuntimeInterface for TezosRuntime {
    fn generate_alias<Host>(
        &self,
        _registry: &impl Registry,
        host: &mut Host,
        _journal: &mut TezosXJournal,
        native_address: &[u8],
        _context: CrossRuntimeContext,
    ) -> Result<Vec<u8>, TezosXRuntimeError>
    where
        Host: StorageV1 + Logging,
    {
        let kt1 = ContractKt1Hash::from(blake2b::digest_160(native_address));

        // TODO: Change everywhere to have a String type for addresses instead of raw bytes, to avoid this UTF-8 parsing logic.
        // native_address may arrive as a UTF-8 string (e.g. "0x2E2A..."
        // from enshrined_contracts) or as raw address bytes (e.g. the
        // 20-byte EVM address from journal.rs). When raw, hex-encode
        // with a "0x" prefix so the forwarder storage holds a valid
        // address string the TezosXGateway can route to.
        let native_address_str = match std::str::from_utf8(native_address) {
            Ok(s) => s.to_string(),
            Err(_) => format!("0x{}", hex::encode(native_address)),
        };

        let context = TezosRuntimeContext::from_root(&ETHERLINK_SAFE_STORAGE_ROOT_PATH)?;
        let account = context.originated_from_kt1(&kt1)?;

        let code = alias_forwarder::forwarder_code().map_err(|e| {
            TezosXRuntimeError::Custom(format!(
                "Failed to decode forwarder code from hex: {e}"
            ))
        })?;
        let storage = alias_forwarder::forwarder_storage(&native_address_str);

        account.init(host, &code, &storage).map_err(|e| {
            TezosXRuntimeError::Custom(format!(
                "Failed to initialize alias forwarder contract: {e}"
            ))
        })?;

        account.set_balance(host, &0u64.into()).map_err(|e| {
            TezosXRuntimeError::Custom(format!("Failed to set alias balance: {e}"))
        })?;

        let contract = Contract::Originated(kt1);
        contract.to_bytes().map_err(|e| {
            TezosXRuntimeError::ConversionError(format!(
                "Failed to encode address to bytes: {e}"
            ))
        })
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
        Host: StorageV1 + Logging,
    {
        build_response(execute_request(registry, host, journal, request))
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
            storage,
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
        let resp = build_response(Ok(make_success(Some(b"hello".to_vec())))).unwrap();
        assert_eq!(resp.status(), StatusCode::OK);
        assert_eq!(resp.body(), b"hello");
    }

    #[test]
    fn build_response_success_empty_body() {
        let resp = build_response(Ok(make_success(None))).unwrap();
        assert_eq!(resp.status(), StatusCode::OK);
        assert!(resp.body().is_empty());
    }

    #[test]
    fn build_response_bad_request() {
        let resp =
            build_response(Err(TezosXRuntimeError::BadRequest("invalid URL".into())))
                .unwrap();
        assert_eq!(resp.status(), StatusCode::BAD_REQUEST);
        assert!(String::from_utf8_lossy(resp.body()).contains("invalid URL"));
    }

    #[test]
    fn build_response_not_found() {
        let resp =
            build_response(Err(TezosXRuntimeError::NotFound("KT1 not found".into())))
                .unwrap();
        assert_eq!(resp.status(), StatusCode::NOT_FOUND);
        assert!(String::from_utf8_lossy(resp.body()).contains("KT1 not found"));
    }

    #[test]
    fn build_response_success_has_gas_consumed_header() {
        // consumed_milligas = 0 → reported as 0
        let resp = build_response(Ok(make_success(None))).unwrap();
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
        let resp = build_response(Ok(make_success_with_milligas(None, 5000))).unwrap();
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
            build_response(Err(TezosXRuntimeError::BadRequest("err".into()))).unwrap();
        assert!(resp.headers().get(X_TEZOS_GAS_CONSUMED).is_none());
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
            _native_address: &[u8],
            runtime_id: RuntimeId,
            _context: CrossRuntimeContext,
        ) -> Result<Vec<u8>, TezosXRuntimeError>
        where
            Host: StorageV1 + Logging,
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
            Host: StorageV1 + Logging,
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
        TezosRuntime::new(ChainId::try_from_bytes(&[0u8; 4]).unwrap())
    }

    #[test]
    fn generate_alias_returns_valid_kt1_bytes() {
        let mut host = MockKernelHost::default();
        let mut journal = TezosXJournal::default();
        let runtime = test_runtime();

        let result = runtime.generate_alias(
            &StubRegistry,
            &mut host,
            &mut journal,
            b"0x1234567890abcdef1234567890abcdef12345678",
            test_context(),
        );

        let alias_bytes = result.expect("generate_alias should succeed");
        // KT1 contract encoding: 1 byte tag (0x01) + 20 bytes hash + 1 byte padding
        assert_eq!(alias_bytes.len(), 22);
        assert_eq!(alias_bytes[0], 0x01); // originated contract tag
    }

    #[test]
    fn generate_alias_deploys_forwarder_code() {
        let mut host = MockKernelHost::default();
        let mut journal = TezosXJournal::default();
        let runtime = test_runtime();
        let evm_address = b"0x1234567890abcdef1234567890abcdef12345678";

        runtime
            .generate_alias(
                &StubRegistry,
                &mut host,
                &mut journal,
                evm_address,
                test_context(),
            )
            .expect("generate_alias should succeed");

        // Verify the contract was deployed by reading it back
        let kt1 = ContractKt1Hash::from(blake2b::digest_160(evm_address));
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
        let evm_address = b"0xdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef";

        runtime
            .generate_alias(
                &StubRegistry,
                &mut host,
                &mut journal,
                evm_address,
                test_context(),
            )
            .expect("generate_alias should succeed");

        let kt1 = ContractKt1Hash::from(blake2b::digest_160(evm_address));
        let context =
            TezosRuntimeContext::from_root(&ETHERLINK_SAFE_STORAGE_ROOT_PATH).unwrap();
        let account = context.originated_from_kt1(&kt1).unwrap();

        let storage = account.storage(&host).unwrap();
        let expected = alias_forwarder::forwarder_storage(
            "0xdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
        );
        assert_eq!(storage, expected);
    }

    #[test]
    fn generate_alias_sets_zero_balance() {
        let mut host = MockKernelHost::default();
        let mut journal = TezosXJournal::default();
        let runtime = test_runtime();
        let evm_address = b"0xabcdef";

        runtime
            .generate_alias(
                &StubRegistry,
                &mut host,
                &mut journal,
                evm_address,
                test_context(),
            )
            .expect("generate_alias should succeed");

        let kt1 = ContractKt1Hash::from(blake2b::digest_160(evm_address));
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
        let evm_address = b"0x1111111111111111111111111111111111111111";

        let alias1 = runtime
            .generate_alias(
                &StubRegistry,
                &mut host1,
                &mut journal,
                evm_address,
                test_context(),
            )
            .unwrap();
        let alias2 = runtime
            .generate_alias(
                &StubRegistry,
                &mut host2,
                &mut journal,
                evm_address,
                test_context(),
            )
            .unwrap();

        assert_eq!(alias1, alias2);
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
                b"0x1111111111111111111111111111111111111111",
                test_context(),
            )
            .unwrap();
        let alias2 = runtime
            .generate_alias(
                &StubRegistry,
                &mut host,
                &mut journal,
                b"0x2222222222222222222222222222222222222222",
                test_context(),
            )
            .unwrap();

        assert_ne!(alias1, alias2);
    }

    #[test]
    fn generate_alias_accepts_raw_evm_address_bytes() {
        let mut host = MockKernelHost::default();
        let mut journal = TezosXJournal::default();
        let runtime = test_runtime();

        // Raw 20-byte EVM address (not valid UTF-8), as passed by journal.rs
        let raw_address: [u8; 20] = [
            0x2E, 0x2A, 0xC8, 0x69, 0x9A, 0xD0, 0x2E, 0x71, 0x09, 0x51, 0xEA, 0x0F, 0x56,
            0xB8, 0x92, 0xED, 0x36, 0x91, 0x6C, 0xD5,
        ];

        let result = runtime.generate_alias(
            &StubRegistry,
            &mut host,
            &mut journal,
            &raw_address,
            test_context(),
        );

        let alias_bytes = result.expect("generate_alias should succeed with raw bytes");
        assert_eq!(alias_bytes.len(), 22);
        assert_eq!(alias_bytes[0], 0x01);

        // Verify the storage contains the hex-encoded address
        let kt1 = ContractKt1Hash::from(blake2b::digest_160(&raw_address));
        let context =
            TezosRuntimeContext::from_root(&ETHERLINK_SAFE_STORAGE_ROOT_PATH).unwrap();
        let account = context.originated_from_kt1(&kt1).unwrap();
        let storage = account.storage(&host).unwrap();
        let expected = alias_forwarder::forwarder_storage(
            "0x2e2ac8699ad02e710951ea0f56b892ed36916cd5",
        );
        assert_eq!(storage, expected);
    }
}

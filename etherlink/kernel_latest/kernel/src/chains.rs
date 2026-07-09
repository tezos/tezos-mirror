// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    apply::{extract_cross_runtime_effects, RuntimeExecutionInfo},
    block_in_progress::BlockInProgress,
    blueprint_storage::{DelayedTransactionFetchingResult, EVMBlockHeader},
    bridge::{execute_tezlink_deposit, Deposit, TEZLINK_DEPOSITOR},
    configuration::EVM_CHAIN_ID,
    delayed_inbox::DelayedInbox,
    error,
    fees::{DEFAULT_MICHELSON_TO_EVM_GAS_MULTIPLIER, MINIMUM_BASE_FEE_PER_GAS},
    l2block::L2Block,
    registry_impl::RegistryImpl,
    simulation::start_simulation_mode,
    storage::{
        read_michelson_to_evm_gas_multiplier, store_michelson_to_evm_gas_multiplier,
    },
    tick_model::constants::MAXIMUM_GAS_LIMIT,
    transaction::TransactionContent,
};
use mir::ast::PublicKeyHash;
use num_traits::ToPrimitive;
use primitive_types::{H160, H256, U256};
use revm::primitives::hardfork::SpecId;
use revm_etherlink::{
    helpers::legacy::{h160_to_alloy, u256_to_alloy},
    inspectors::TracerInput,
    storage::{block::BLOCKS_STORED, world_state_handler::StorageAccount},
};
use rlp::{Decodable, DecoderError, Encodable};
use sha3::{Digest, Keccak256};
use std::fmt::Debug;
use tezos_crypto_rs::hash::{BlockHash, ChainId, UnknownSignature};
use tezos_data_encoding::nom::NomReader;
use tezos_ethereum::tx_common::EthereumTransactionCommon;
use tezos_ethereum::{
    rlp_helpers::{decode_field, decode_tx_hash, next},
    transaction::TransactionHash,
    wei::{eth_from_mutez, michelson_gas_to_mutez, mutez_from_wei},
};
use tezos_evm_logging::{log, Level::*};
use tezos_tezlink::operation::ManagerOperationField;

use tezos_execution::{
    get_required_da_fees, mir_ctx::BlockCtx, FeeRefundConfig, ProcessedOperation,
};
use tezos_smart_rollup::{outbox::OutboxQueue, types::Timestamp};
use tezos_smart_rollup_host::path::{OwnedPath, Path, RefPath};
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_host::wasm::WasmHost;
use tezos_tezlink::{
    block::{AppliedOperation, TezBlock},
    enc_wrappers::BlockNumber,
    operation::{ManagerOperation, ManagerOperationContent, Operation},
    operation_result::{
        OperationBatchWithMetadata, OperationDataAndMetadata, OperationError,
        OperationResult, OperationResultSum, OperationWithMetadata,
    },
};
use tezosx_interfaces::{Registry, RuntimeId};
use tezosx_journal::TezosXJournal;

use crate::apply::push_withdrawals_to_outbox;
use revm_etherlink::journal::commit_evm_journal_from_external;

pub use tezos_evm_runtime::safe_storage::ETHERLINK_SAFE_STORAGE_ROOT_PATH;

pub const TEZ_SAFE_STORAGE_ROOT_PATH: RefPath = RefPath::assert_from(b"/tez/world_state");

/// Path for TezBlock storage. Sits under TEZ_SAFE_STORAGE_ROOT_PATH so it's
/// included in SafeStorage transactions and snapshotted as part of the
/// Michelson world state.
pub const TEZ_BLOCKS_PATH: RefPath = RefPath::assert_from(b"/tez/world_state/tez_blocks");

/// True if `branch` is a recent block of this instance; genesis is accepted only while the chain is under [`BLOCKS_STORED`] blocks.
pub(crate) fn is_valid_tez_branch<Host: StorageV1>(
    host: &Host,
    branch: &H256,
) -> Result<bool, crate::Error> {
    if crate::block_storage::is_recent_block_hash(host, &TEZ_BLOCKS_PATH, branch)? {
        return Ok(true);
    }
    if *branch == H256(*TezBlock::genesis_block_hash()) {
        let current = crate::block_storage::read_current_number(host, &TEZ_BLOCKS_PATH)
            .unwrap_or_else(|_| U256::zero());
        return Ok(current < U256::from(BLOCKS_STORED));
    }
    Ok(false)
}

/// Unified SafeStorage root for all Tezos account state. Holds Michelson
/// contract / big_map state directly (ex `/tezlink/context/`) and TezosX
/// projected accounts under `tezosx/`. Re-exported from `tezos_execution` so
/// the account path-builders and the SafeStorage root enumeration share a
/// single source of truth.
pub use tezos_execution::context::TEZOS_ACCOUNTS_ROOT;

/// SafeStorage root for all EVM account state (balances, nonces, code, storage)
/// and deduplicated bytecode.
pub const EVM_ETH_ACCOUNTS_SAFE_STORAGE_ROOT_PATH: RefPath =
    RefPath::assert_from(b"/evm/eth_accounts");

/// Choose the SafeStorage roots to snapshot for a single Tezos operation.
///
/// [validate_and_apply_operation] wraps each operation in a transactional
/// snapshot of every root in `full_roots` (one `store_copy` + `store_move`
/// per root, twice — once for validation, once for application). Most
/// operations only read or write account state under
/// [TEZOS_ACCOUNTS_ROOT]; snapshotting the EVM roots and
/// the Tez block/global-state root just to roll them back on failure is pure
/// overhead. For batches that provably touch nothing else (see
/// [Operation::touches_only_accounts]), narrow the snapshot to the accounts
/// root alone; otherwise keep the full conservative set.
fn operation_safe_roots(
    operation: &Operation,
    full_roots: &[OwnedPath],
) -> Vec<OwnedPath> {
    if operation.touches_only_accounts() {
        let narrowed: Vec<OwnedPath> = full_roots
            .iter()
            .filter(|root| root.as_bytes() == TEZOS_ACCOUNTS_ROOT.as_bytes())
            .cloned()
            .collect();
        // Fall back to the full set if the accounts root is unexpectedly
        // absent, so we never snapshot fewer roots than the operation needs.
        if !narrowed.is_empty() {
            return narrowed;
        }
    }
    full_roots.to_vec()
}

#[derive(Debug, Default)]
pub struct DebugFeatures {
    pub enable_debug_precompiles: bool,
}

impl DebugFeatures {
    pub fn read_from_storage<Host>(host: &mut Host) -> Self
    where
        Host: StorageV1,
    {
        let enable_debug_precompiles = crate::storage::enable_debug_precompiles(host);

        DebugFeatures {
            enable_debug_precompiles,
        }
    }
}

#[derive(Debug, Default)]
pub struct ExperimentalFeatures {
    enable_michelson_gas_refund: bool,
    /// If `Some(level)`, the Michelson runtime is enabled and activates at the
    /// given EVM block number. `None` means the feature flag is not set.
    enabled_michelson_target_sunrise_level: Option<U256>,
}

impl ExperimentalFeatures {
    pub fn read_from_storage(host: &mut impl StorageV1) -> Self {
        let enable_michelson_gas_refund =
            crate::storage::enable_michelson_gas_refund(host);

        let enabled_michelson_target_sunrise_level =
            if crate::storage::enable_tezos_runtime(host) {
                // The target sunrise level defaults to 0 if not set. This allows
                // current deployment where only enable_tezos_runtime is set to still
                // activate the Michelson runtime.
                Some(
                    crate::storage::read_michelson_runtime_target_sunrise_level(host)
                        .unwrap_or(U256::zero()),
                )
            } else {
                None
            };

        ExperimentalFeatures {
            enable_michelson_gas_refund,
            enabled_michelson_target_sunrise_level,
        }
    }

    pub fn is_tezos_runtime_enabled(&self, current_level: U256) -> bool {
        self.enabled_michelson_target_sunrise_level
            .is_some_and(|target| current_level >= target)
    }

    pub fn tezos_runtime_feature_flag(&self) -> bool {
        self.enabled_michelson_target_sunrise_level.is_some()
    }

    pub(crate) fn is_michelson_gas_refund_enabled(&self) -> bool {
        self.enable_michelson_gas_refund
    }
}

#[derive(Debug)]
pub struct TezosXChainConfig {
    evm_chain_id: U256,
    pub limits: EvmLimits,
    pub spec_id: SpecId,
    pub experimental_features: ExperimentalFeatures,
    pub debug_features: DebugFeatures,
    michelson_chain_id: ChainId,
}

impl TezosXChainConfig {
    pub fn tezos_runtime_feature_flag(&self) -> bool {
        self.experimental_features.tezos_runtime_feature_flag()
    }
}

const TEZOS_OP_TAG: u8 = 1;
const DEPOSIT_OP_TAG: u8 = 2;

#[derive(Debug, PartialEq)]
pub struct TezlinkOperation {
    pub tx_hash: TransactionHash,
    pub content: TezlinkContent,
}

impl Encodable for TezlinkOperation {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        stream.append_iter(self.tx_hash);
        stream.append(&self.content);
    }
}

impl Decodable for TezlinkOperation {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 2 {
            return Err(DecoderError::RlpIncorrectListLen);
        }
        let mut it = decoder.iter();
        let tx_hash: TransactionHash = decode_tx_hash(next(&mut it)?)?;
        let content: TezlinkContent =
            decode_field(&next(&mut it)?, "Transaction content")?;
        Ok(TezlinkOperation { tx_hash, content })
    }
}

#[derive(Debug, PartialEq)]
pub enum TezlinkContent {
    Tezos(Operation),
    Deposit(Deposit),
}

impl Encodable for TezlinkContent {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        match &self {
            Self::Tezos(tez) => {
                stream.append(&TEZOS_OP_TAG);
                stream.append(tez);
            }
            Self::Deposit(dep) => {
                stream.append(&DEPOSIT_OP_TAG);
                dep.rlp_append(stream)
            }
        }
    }
}

impl Decodable for TezlinkContent {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 2 {
            return Err(DecoderError::RlpIncorrectListLen);
        }
        let tag: u8 = decoder.at(0)?.as_val()?;
        let tx = decoder.at(1)?;
        match tag {
            DEPOSIT_OP_TAG => {
                let deposit = Deposit::decode(&tx)?;
                Ok(Self::Deposit(deposit))
            }
            TEZOS_OP_TAG => {
                let eth = Operation::decode(&tx)?;
                Ok(Self::Tezos(eth))
            }
            _ => Err(DecoderError::Custom("Unknown operation tag.")),
        }
    }
}

impl From<crate::transaction::Transaction> for TezosXTransaction {
    fn from(tx: crate::transaction::Transaction) -> Self {
        Self::Ethereum(Box::new(tx))
    }
}

impl TryFrom<TezosXTransaction> for crate::transaction::Transaction {
    type Error = ();

    fn try_from(tx: TezosXTransaction) -> Result<Self, Self::Error> {
        match tx {
            TezosXTransaction::Ethereum(tx) => Ok(*tx),
            TezosXTransaction::Tezos(_) => Err(()),
        }
    }
}

impl TezlinkOperation {
    pub fn is_delayed(&self) -> bool {
        match self.content {
            TezlinkContent::Tezos(_) => false,
            TezlinkContent::Deposit(_) => true,
        }
    }

    pub fn data_size(&self) -> u64 {
        0
    }
}

impl From<TezlinkOperation> for TezosXTransaction {
    fn from(op: TezlinkOperation) -> Self {
        Self::Tezos(op)
    }
}

impl TryFrom<TezosXTransaction> for TezlinkOperation {
    type Error = ();

    fn try_from(tx: TezosXTransaction) -> Result<Self, Self::Error> {
        match tx {
            TezosXTransaction::Tezos(op) => Ok(op),
            TezosXTransaction::Ethereum(_) => Err(()),
        }
    }
}

impl crate::blueprint_storage::EVMBlockHeader {
    pub fn hash(&self) -> H256 {
        self.hash
    }

    pub fn genesis_header() -> Self {
        EVMBlockHeader {
            hash: crate::block::GENESIS_PARENT_HASH,
            receipts_root: vec![0; 32],
            transactions_root: vec![0; 32],
        }
    }
}

fn ethereum_transaction_from_bytes(
    bytes: &[u8],
) -> anyhow::Result<crate::transaction::Transaction> {
    let tx_hash = Keccak256::digest(bytes).into();
    let tx_common = EthereumTransactionCommon::from_bytes(bytes)?;

    Ok(crate::transaction::Transaction {
        tx_hash,
        content: TransactionContent::Ethereum(tx_common),
    })
}

#[derive(Debug, PartialEq)]
pub enum TezosXTransaction {
    Ethereum(Box<crate::transaction::Transaction>),
    Tezos(TezlinkOperation),
}

impl Decodable for TezosXTransaction {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 2 {
            return Err(DecoderError::RlpIncorrectListLen);
        }
        let mut it = decoder.iter();
        let runtime_id: u8 = decode_field(&next(&mut it)?, "runtime_id")?;
        match RuntimeId::try_from(runtime_id) {
            Ok(RuntimeId::Tezos) => {
                let op = decode_field(&next(&mut it)?, "tezos_operation")?;
                Ok(Self::Tezos(op))
            }
            Ok(RuntimeId::Ethereum) => {
                let tx = decode_field(&next(&mut it)?, "ethereum_transaction")?;
                Ok(TezosXTransaction::Ethereum(Box::new(tx)))
            }
            Err(message) => Err(rlp::DecoderError::Custom(message)),
        }
    }
}

impl Encodable for TezosXTransaction {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        match self {
            Self::Ethereum(tx) => {
                let tag: u8 = RuntimeId::Ethereum.into();
                stream.append(&tag);
                stream.append(tx);
            }
            Self::Tezos(op) => {
                let tag: u8 = RuntimeId::Tezos.into();
                stream.append(&tag);
                stream.append(op);
            }
        }
    }
}

impl TezosXTransaction {
    pub fn is_delayed(&self) -> bool {
        match self {
            Self::Ethereum(tx) => tx.is_delayed(),
            Self::Tezos(op) => op.is_delayed(),
        }
    }

    pub fn tx_hash(&self) -> TransactionHash {
        match self {
            Self::Ethereum(tx) => tx.tx_hash,
            Self::Tezos(op) => op.tx_hash,
        }
    }

    pub fn data_size(&self) -> u64 {
        match self {
            Self::Ethereum(tx) => tx.data_size(),
            Self::Tezos(op) => op.data_size(),
        }
    }
}

pub struct TezosXBlockConstants {
    pub evm_runtime_block_constants: tezos_ethereum::block::BlockConstants,
    pub michelson_runtime_block_constants: TezlinkBlockConstants,
}

fn read_or_init_michelson_to_evm_gas_multiplier(host: &mut impl StorageV1) -> u64 {
    match read_michelson_to_evm_gas_multiplier(host) {
        Ok(value) => value,
        Err(_) => {
            let _ = store_michelson_to_evm_gas_multiplier(
                host,
                DEFAULT_MICHELSON_TO_EVM_GAS_MULTIPLIER,
            );
            DEFAULT_MICHELSON_TO_EVM_GAS_MULTIPLIER
        }
    }
}

impl TezosXChainConfig {
    pub fn get_evm_chain_id(&self) -> U256 {
        self.evm_chain_id
    }

    pub fn is_tezos_runtime_enabled(&self, current_level: U256) -> bool {
        self.experimental_features
            .is_tezos_runtime_enabled(current_level)
    }

    pub fn init_registry(&self) -> RegistryImpl {
        RegistryImpl::new(self.evm_chain_id, self.michelson_chain_id.clone())
    }

    pub fn constants(
        &self,
        host: &mut impl StorageV1,
        block_in_progress: &BlockInProgress,
        da_fee_per_byte: U256,
        coinbase: H160,
    ) -> anyhow::Result<TezosXBlockConstants> {
        let level: BlockNumber = block_in_progress.number.try_into()?;
        let da_fee_per_byte_mutez = mutez_from_wei(da_fee_per_byte)
            .map_err(|_| crate::Error::InvalidConversion)?;
        let michelson_to_evm_gas_multiplier =
            read_or_init_michelson_to_evm_gas_multiplier(host);
        let safe_roots = self
            .storage_root_paths(level.into())
            .iter()
            .map(OwnedPath::from)
            .collect();
        Ok(TezosXBlockConstants {
            evm_runtime_block_constants: block_in_progress.constants(
                self.evm_chain_id,
                self.limits.minimum_base_fee_per_gas,
                da_fee_per_byte,
                crate::block::GAS_LIMIT,
                coinbase,
                self.is_tezos_runtime_enabled(level.into()),
            ),
            michelson_runtime_block_constants: TezlinkBlockConstants {
                level,
                da_fee_per_byte_mutez,
                michelson_to_evm_gas_multiplier,
                safe_roots,
            },
        })
    }

    pub fn michelson_to_evm_gas_multiplier(
        &self,
        constants: &TezosXBlockConstants,
    ) -> u64 {
        constants
            .michelson_runtime_block_constants
            .michelson_to_evm_gas_multiplier
    }

    pub fn base_fee_per_gas(&self, host: &impl StorageV1, timestamp: Timestamp) -> U256 {
        crate::gas_price::base_fee_per_gas(
            host,
            timestamp,
            self.get_limits().minimum_base_fee_per_gas,
        )
    }

    pub fn transaction_from_bytes(
        bytes: &[u8],
        blueprint_version: u8,
    ) -> anyhow::Result<TezosXTransaction> {
        match blueprint_version {
            0 => {
                // Blueprints version 0 can only contain Ethereum
                // transactions, they are not prefixed by a tag.
                let tx = ethereum_transaction_from_bytes(bytes)?;
                Ok(TezosXTransaction::Ethereum(Box::new(tx)))
            }
            1 => {
                // Blueprints version 1 can contain both Tezos
                // operations and Ethereum transactions, prefixed by
                // 0x00 or 0x01 respectively.
                let Some((tag, tx_common)) = bytes.split_first() else {
                    return Err(error::Error::NomReadError(
                        "Unexpected empty transaction.".to_string(),
                    ))?;
                };
                match RuntimeId::try_from(*tag) {
                    Ok(RuntimeId::Tezos) => {
                        let op = tezos_operation_from_bytes(tx_common)?;
                        Ok(TezosXTransaction::Tezos(op))
                    }
                    Ok(RuntimeId::Ethereum) => {
                        let tx = ethereum_transaction_from_bytes(tx_common)?;
                        Ok(TezosXTransaction::Ethereum(Box::new(tx)))
                    }
                    Err(message) => {
                        log!(Error, "Unknown runtime id tag: {tag}, {message}");
                        Err(DecoderError::Custom("Unknown runtime id").into())
                    }
                }
            }
            _ => {
                log!(Error, "Unknown blueprint version: {blueprint_version:?}");
                Err(DecoderError::Custom("Unknown blueprint version").into())
            }
        }
    }

    pub fn fetch_hashes_from_delayed_inbox<Host>(
        host: &mut Host,
        delayed_hashes: Vec<crate::delayed_inbox::Hash>,
        delayed_inbox: &DelayedInbox,
        current_blueprint_size: usize,
        block_number: U256,
    ) -> anyhow::Result<(DelayedTransactionFetchingResult<TezosXTransaction>, usize)>
    where
        Host: StorageV1,
    {
        crate::blueprint_storage::fetch_hashes_from_delayed_inbox(
            host,
            delayed_hashes,
            delayed_inbox,
            current_blueprint_size,
            block_number,
        )
    }

    pub fn can_fit_in_reboot(
        &self,
        executed_gas: U256,
        transaction: &TezosXTransaction,
        block_constants: &TezosXBlockConstants,
    ) -> anyhow::Result<bool> {
        let evm_gas_limit = match transaction {
            TezosXTransaction::Ethereum(transaction) => transaction.execution_gas_limit(
                &block_constants.evm_runtime_block_constants.block_fees,
            )?,
            TezosXTransaction::Tezos(operation) => tezos_op_evm_gas_limit(
                operation,
                block_constants
                    .michelson_runtime_block_constants
                    .michelson_to_evm_gas_multiplier,
            )?,
        };
        Ok(crate::block::can_fit_in_reboot(
            &self.limits,
            executed_gas,
            evm_gas_limit,
        ))
    }

    #[allow(clippy::too_many_arguments)]
    pub fn apply_transaction<Host>(
        &self,
        block_in_progress: &BlockInProgress,
        host: &mut Host,
        registry: &impl Registry,
        outbox_queue: &OutboxQueue<'_, impl Path>,
        block_constants: &TezosXBlockConstants,
        transaction: TezosXTransaction,
        index: u32,
        sequencer_pool_address: Option<H160>,
        tracer_input: Option<TracerInput>,
        // skip_signature_check can be used to simulate the application of
        // ill-signed Tezos operations, it has no effect in the EVM case.
        skip_signature_check: bool,
        // skip_fees_check can be used to simulate the application of Tezos
        // operations with no fees, it has no effect in the EVM case.
        skip_fees_check: bool,
        http_trace_enabled: bool,
    ) -> Result<crate::apply::ExecutionResult<RuntimeExecutionInfo>, anyhow::Error>
    where
        Host: StorageV1,
    {
        match transaction {
            TezosXTransaction::Ethereum(transaction) => {
                let (origin_runtime, id) = match &transaction.content {
                    TransactionContent::Ethereum(_)
                    | TransactionContent::EthereumDelayed(_)
                    | TransactionContent::Deposit(_)
                    | TransactionContent::FaDeposit(_) => (1, index), // Ethereum
                    TransactionContent::TezosDelayed(raw_operation) => {
                        let operation = TezlinkOperation {
                            tx_hash: transaction.tx_hash,
                            content: TezlinkContent::Tezos(raw_operation.clone()),
                        };

                        return self.apply_tezos_operation(
                            block_in_progress,
                            host,
                            registry,
                            outbox_queue,
                            operation,
                            block_constants,
                            sequencer_pool_address,
                            tracer_input,
                            false, // da fees are disabled for delayed operations
                            skip_signature_check,
                            skip_fees_check,
                            true, // branch already checked at delayed-inbox entry
                            http_trace_enabled,
                        );
                    } // Tezos
                };
                let crac_id = tezosx_journal::CracId::new(origin_runtime, id);
                // Cumulative base for the per-block internal-op cap.
                let internal_operations_base = crate::apply::count_internal_operations(
                    &block_in_progress.cumulative_tezos_operation_receipts.list,
                );
                crate::apply::apply_transaction(
                    host,
                    registry,
                    outbox_queue,
                    &block_constants.evm_runtime_block_constants,
                    *transaction,
                    crac_id,
                    index,
                    sequencer_pool_address,
                    tracer_input,
                    &self.spec_id,
                    &self.limits,
                    http_trace_enabled,
                    internal_operations_base,
                    &self.debug_features,
                )
            }
            TezosXTransaction::Tezos(operation) => self.apply_tezos_operation(
                block_in_progress,
                host,
                registry,
                outbox_queue,
                operation,
                block_constants,
                sequencer_pool_address,
                tracer_input,
                true, // da fees are enabled when the sequencer injects a transaction
                skip_signature_check,
                skip_fees_check,
                false, // branch validated at inclusion for sequencer-injected ops
                http_trace_enabled,
            ),
        }
    }

    pub fn finalize_and_store<Host>(
        &self,
        host: &mut Host,
        block_in_progress: BlockInProgress,
        block_constants: &TezosXBlockConstants,
        _chain_header: EVMBlockHeader,
    ) -> anyhow::Result<L2Block>
    where
        Host: StorageV1,
    {
        let current_level = block_in_progress.number;
        block_in_progress.finalize_and_store(
            host,
            block_constants,
            self.is_tezos_runtime_enabled(current_level),
        )
    }

    pub fn start_simulation_mode<Host>(
        &self,
        host: &mut Host,
        registry: &impl Registry,
    ) -> anyhow::Result<()>
    where
        Host: StorageV1 + WasmHost,
    {
        start_simulation_mode(host, registry, &self.spec_id)
    }

    pub fn storage_root_paths(&self, block_number: U256) -> Vec<RefPath> {
        if self.is_tezos_runtime_enabled(block_number) {
            vec![
                ETHERLINK_SAFE_STORAGE_ROOT_PATH,
                EVM_ETH_ACCOUNTS_SAFE_STORAGE_ROOT_PATH,
                TEZ_SAFE_STORAGE_ROOT_PATH,
                TEZOS_ACCOUNTS_ROOT,
            ]
        } else {
            vec![
                ETHERLINK_SAFE_STORAGE_ROOT_PATH,
                EVM_ETH_ACCOUNTS_SAFE_STORAGE_ROOT_PATH,
            ]
        }
    }
}

impl TezosXChainConfig {
    #[allow(clippy::too_many_arguments)]
    fn apply_tezos_operation<Host>(
        &self,
        block_in_progress: &BlockInProgress,
        host: &mut Host,
        registry: &impl Registry,
        outbox_queue: &OutboxQueue<'_, impl Path>,
        operation: TezlinkOperation,
        block_constants: &TezosXBlockConstants,
        sequencer_pool_address: Option<H160>,
        tracer_input: Option<TracerInput>,
        da_fees_enabled: bool,
        skip_signature_check: bool,
        skip_fees_check: bool,
        skip_branch_check: bool,
        http_trace_enabled: bool,
    ) -> Result<crate::apply::ExecutionResult<RuntimeExecutionInfo>, anyhow::Error>
    where
        Host: StorageV1,
    {
        let tx_hash = operation.tx_hash;
        let crac_id = tezosx_journal::CracId::new(0, block_in_progress.michelson_index);
        // The seed must NOT be the operation's real hash: the
        // normal Michelson path inside `apply_tezos_operation`
        // builds its own `OriginationNonce::initial(real_hash)`
        // starting at index 0, and if the journal's CRAC nonce
        // shared the same seed both nonces would derive
        // colliding KT1s at index 1.  Use a synthetic seed so
        // the two nonce universes stay disjoint.
        let operation_hash = TezosXJournal::synthetic_operation_hash(
            &crac_id,
            self.evm_chain_id.low_u64(),
            block_in_progress.number.low_u64(),
        );
        // Seed the journal with this block's EVM environment so any
        // inbound CRAC the Michelson operation dispatches to the EVM
        // runtime exposes the live block observables (BASEFEE,
        // GASLIMIT, ...) rather than zero.
        let mut journal = TezosXJournal::new(
            crac_id,
            operation_hash,
            block_constants.evm_runtime_block_constants.clone(),
        );
        journal.set_http_trace_enabled(http_trace_enabled);
        let result = apply_tezos_operation(
            &self.michelson_chain_id,
            block_in_progress,
            host,
            registry,
            &block_constants.michelson_runtime_block_constants,
            operation,
            sequencer_pool_address,
            skip_signature_check,
            skip_fees_check,
            skip_branch_check,
            Some(outbox_queue),
            Some(&block_constants.evm_runtime_block_constants),
            &mut journal,
            self.experimental_features.is_michelson_gas_refund_enabled(),
            tracer_input.is_some(),
            http_trace_enabled,
            da_fees_enabled,
        );
        // Persist HTTP traces regardless of [result] so that partial
        // traces from an operation that failed mid-execution remain
        // observable through the [http_trace*] RPCs. Matches the
        // Ethereum / TezosDelayed apply sites in apply.rs.
        //
        // TODO: https://linear.app/tezos/issue/L2-1243
        // The Tezlink-submitted path stores traces under the Tezos
        // operation hash only, so [http_traceTransaction] queries
        // that pass the fake Ethereum hash of a Tezos→EVM CRAC
        // (the hash users see in [eth_getBlockByNumber]) return
        // the empty list. Extend this site to dual-index the
        // traces under each generated Ethereum fake hash once the
        // mapping is available here.
        crate::storage::maybe_store_http_traces_for_tx(
            host,
            http_trace_enabled,
            &tx_hash,
            &journal,
        );
        result
    }
}

impl TezosXChainConfig {
    pub fn create_config(
        evm_chain_id: U256,
        limits: EvmLimits,
        spec_id: SpecId,
        experimental_features: ExperimentalFeatures,
        debug_features: DebugFeatures,
        michelson_chain_id: ChainId,
    ) -> Self {
        Self {
            evm_chain_id,
            limits,
            spec_id,
            experimental_features,
            debug_features,
            michelson_chain_id,
        }
    }

    pub fn get_limits(&self) -> &EvmLimits {
        &self.limits
    }

    pub fn limits_mut(&mut self) -> &mut EvmLimits {
        &mut self.limits
    }

    pub fn michelson_chain_id(&self) -> &ChainId {
        &self.michelson_chain_id
    }
}

fn tezos_operation_from_bytes(bytes: &[u8]) -> anyhow::Result<TezlinkOperation> {
    let operation = Operation::nom_read_exact(bytes).map_err(|decode_error| {
        error::Error::NomReadError(format!("{decode_error:?}"))
    })?;
    let tx_hash = operation.hash()?.into();
    Ok(TezlinkOperation {
        tx_hash,
        content: TezlinkContent::Tezos(operation),
    })
}

pub struct TezlinkBlockConstants {
    pub level: BlockNumber,
    /// DA fee per byte in mutez, read once at block start from durable storage.
    /// When `disable_da_fees` is set, this is 0.
    pub da_fee_per_byte_mutez: u64,
    /// Conversion factor from Michelson gas to EVM gas, read once at block
    /// start from durable storage (falls back to the compile-time constant).
    pub michelson_to_evm_gas_multiplier: u64,
    /// SafeStorage roots to snapshot when validating/applying a Tezos
    /// operation. Mirrors `TezosXChainConfig::storage_root_paths` so the
    /// inner transactional wrap covers every subtree that a Tezos operation
    /// may read or write.
    pub safe_roots: Vec<OwnedPath>,
}

fn credit_da_fees<Host>(
    host: &mut Host,
    sequencer_pool_address: Option<H160>,
    da_fees_mutez: u64,
) -> Result<(), anyhow::Error>
where
    Host: StorageV1,
{
    if let Some(sequencer_pool_address) = sequencer_pool_address {
        let da_fees_wei = eth_from_mutez(da_fees_mutez);
        let mut account =
            StorageAccount::from_address(&h160_to_alloy(&sequencer_pool_address))?;
        if account
            .add_balance(host, u256_to_alloy(&da_fees_wei))
            .is_err()
        {
            return Err(anyhow::anyhow!(
                "Failed to compensate sequencer with da fees",
            ));
        }
    };

    Ok(())
}

enum CreditDaFees {
    Skip,
    Execute {
        sequencer_pool_address: Option<H160>,
        da_fees_mutez: u64,
    },
}

impl CreditDaFees {
    fn da_fees(&self) -> u64 {
        match self {
            CreditDaFees::Skip => 0,
            CreditDaFees::Execute { da_fees_mutez, .. } => *da_fees_mutez,
        }
    }

    fn apply<Host: StorageV1>(self, host: &mut Host) -> Result<(), anyhow::Error> {
        match self {
            CreditDaFees::Skip => Ok(()),
            CreditDaFees::Execute {
                sequencer_pool_address,
                da_fees_mutez,
            } => credit_da_fees(host, sequencer_pool_address, da_fees_mutez),
        }
    }
}

/// Fees computed from an operation before execution.
struct FeesData {
    /// Total required fees (DA + gas) to check against declared fees,
    /// or `None` in simulation (skip_fees_check).
    required_fees: Option<u64>,
    /// Action to credit the sequencer pool with DA fees after execution.
    /// Also carries the DA fees amount (via `da_fees()`).
    credit_da_fees: CreditDaFees,
}

/// Convert Michelson gas (in Tezos-gas units) into EVM-gas units,
/// using the cross-runtime conversion coefficient `multiplier`
/// (evm_gas / michelson_gas).
pub(crate) fn michelson_gas_to_evm_gas(
    michelson_gas: u64,
    michelson_to_evm_gas_multiplier: u64,
) -> u64 {
    michelson_gas.saturating_mul(michelson_to_evm_gas_multiplier)
}

/// Convert Michelson milligas (the unit returned by the Michelson
/// runtime after execution) into EVM-gas units. Mirrors the
/// post-execution conversion done in
/// `BlockInProgress::register_valid_transaction` so both the
/// declared-gas-limit path and the consumed-milligas path use the
/// same coefficient.
pub(crate) fn michelson_milligas_to_evm_gas(
    milligas: u64,
    michelson_to_evm_gas_multiplier: u64,
) -> u64 {
    michelson_gas_to_evm_gas(milligas / 1000, michelson_to_evm_gas_multiplier)
}

/// Convert an operation's declared gas limit (sum across its
/// manager contents) into EVM-gas units, using the same coefficient
/// as the post-execution conversion in
/// `BlockInProgress::register_valid_transaction`.
///
/// Deposits have no user-declared gas limit, so they are reported
/// as zero (always fitting). Their cost is bounded by the bridge
/// precompile.
fn tezos_op_evm_gas_limit(
    op: &TezlinkOperation,
    michelson_to_evm_gas_multiplier: u64,
) -> anyhow::Result<u64> {
    match &op.content {
        TezlinkContent::Tezos(operation) => {
            let total_michelson_gas: u64 = operation
                .content
                .iter()
                .filter_map(|c| c.gas_limit().ok())
                .filter_map(|gl| gl.0.to_u64())
                .try_fold(0u64, |acc, x| acc.checked_add(x))
                .ok_or_else(|| anyhow::anyhow!("gas limit sum overflow"))?;
            Ok(michelson_gas_to_evm_gas(
                total_michelson_gas,
                michelson_to_evm_gas_multiplier,
            ))
        }
        TezlinkContent::Deposit(_) => Ok(0),
    }
}

/// Returns the required fees if any and the function to execute
/// regarding DA fees, either skipped if operation is a simulation
/// or credit sequencer pool address accordingly.
fn get_fees_data(
    operation: &Operation,
    skip_fees_check: bool,
    da_fee_per_byte_mutez: u64,
    base_fee_per_gas: U256,
    michelson_to_evm_gas_multiplier: u64,
    sequencer_pool_address: Option<H160>,
    enable_da_fees: bool,
) -> Result<FeesData, anyhow::Error> {
    if skip_fees_check {
        Ok(FeesData {
            required_fees: None,
            credit_da_fees: CreditDaFees::Skip,
        })
    } else {
        let required_da_fees = if enable_da_fees {
            get_required_da_fees(operation, da_fee_per_byte_mutez)?
        } else {
            0
        };

        let total_gas_limit: u64 = operation
            .content
            .iter()
            .filter_map(|c| c.gas_limit().ok())
            .filter_map(|gl| gl.0.to_u64())
            .try_fold(0u64, |acc, x| acc.checked_add(x))
            .ok_or_else(|| anyhow::anyhow!("gas limit sum overflow"))?;

        let required_execution_gas_fees = michelson_gas_to_mutez(
            base_fee_per_gas,
            michelson_to_evm_gas_multiplier,
            total_gas_limit,
        );

        let required_fees = required_da_fees.saturating_add(required_execution_gas_fees);

        Ok(FeesData {
            required_fees: Some(required_fees),
            credit_da_fees: CreditDaFees::Execute {
                sequencer_pool_address,
                da_fees_mutez: required_da_fees,
            },
        })
    }
}

#[allow(clippy::too_many_arguments)]
pub fn apply_tezos_operation<Host>(
    chain_id: &ChainId,
    block_in_progress: &BlockInProgress,
    host: &mut Host,
    registry: &impl Registry,
    block_constants: &TezlinkBlockConstants,
    operation: TezlinkOperation,
    sequencer_pool_address: Option<H160>,
    skip_signature_check: bool,
    skip_fees_check: bool,
    // Delayed-origin ops are branch-checked once at delayed-inbox entry; skip the
    // check here so a branch aging out before forced inclusion cannot drop them.
    skip_branch_check: bool,
    outbox_queue: Option<&OutboxQueue<'_, impl Path>>,
    evm_block_constants: Option<&tezos_ethereum::block::BlockConstants>,
    external_journal: &mut TezosXJournal,
    enable_gas_refund: bool,
    // Forwarded to validate_and_apply_operation to gate per-op trace promotion
    // (and its store_has probe) on whether each tracer is actually active.
    tracing_enabled: bool,
    http_trace_enabled: bool,
    enable_da_fees: bool,
) -> Result<crate::apply::ExecutionResult<RuntimeExecutionInfo>, anyhow::Error>
where
    Host: StorageV1,
{
    let level = block_constants.level;
    let now = block_in_progress.timestamp;
    let block_ctx = BlockCtx {
        level: &level,
        now: &now,
        chain_id,
        internal_operations_base: crate::apply::count_internal_operations(
            &block_in_progress.cumulative_tezos_operation_receipts.list,
        ),
        tracing_enabled,
        http_trace_enabled,
    };

    match operation.content {
        TezlinkContent::Tezos(operation) => {
            // Compute the hash of the operation
            let hash = operation.hash()?;

            let FeesData {
                required_fees: fees,
                credit_da_fees,
            } = get_fees_data(
                &operation,
                skip_fees_check,
                block_constants.da_fee_per_byte_mutez,
                block_in_progress.base_fee_per_gas,
                block_constants.michelson_to_evm_gas_multiplier,
                sequencer_pool_address,
                enable_da_fees,
            )?;

            let fee_refund_config = if enable_gas_refund {
                Some(FeeRefundConfig {
                    da_fees: credit_da_fees.da_fees(),
                    base_fee_per_gas: block_in_progress.base_fee_per_gas,
                    michelson_to_evm_gas_multiplier: block_constants
                        .michelson_to_evm_gas_multiplier,
                })
            } else {
                None
            };

            let branch = operation.branch.clone();
            let signature = operation.signature.clone();

            // The branch must be a recent block of this instance (see is_valid_tez_branch); reject otherwise.
            if !skip_branch_check && !is_valid_tez_branch(host, &H256(*branch))? {
                log!(
                    Error,
                    "Dropping Tezos operation {}: branch {} is not a recent block of this instance",
                    hex::encode(*hash),
                    hex::encode(*branch)
                );
                return Ok(crate::apply::ExecutionResult::Invalid);
            }

            // Try to apply the operation with the tezos_execution crate, return a receipt
            // on whether it failed or not
            let journal = external_journal;
            // Snapshot only the roots this operation can touch (often just the
            // accounts root) instead of the full conservative set.
            let safe_roots =
                operation_safe_roots(&operation, &block_constants.safe_roots);
            let processed_operations = match tezos_execution::validate_and_apply_operation(
                host,
                registry,
                journal,
                hash.clone(),
                operation,
                &block_ctx,
                skip_signature_check,
                fees,
                fee_refund_config,
                &safe_roots,
            ) {
                Ok(receipt) => receipt,
                Err(OperationError::Validation(err)) => {
                    log!(Error, "Found an invalid operation, dropping it: {:?}", err);
                    return Ok(crate::apply::ExecutionResult::Invalid);
                }
                Err(OperationError::RuntimeError(err)) => {
                    return Err(err.into());
                }
                Err(OperationError::BlockAbort(msg)) => {
                    return Err(anyhow::anyhow!("cross-runtime call block abort: {msg}"));
                }
            };

            let consumed_milligas =
                ProcessedOperation::total_consumed_milligas(&processed_operations);
            let operations = ProcessedOperation::into_receipts(processed_operations);

            // Add the applied operation in the block in progress
            let applied_operation = AppliedOperation {
                hash,
                branch,
                op_and_receipt: OperationDataAndMetadata::OperationWithMetadata(
                    OperationBatchWithMetadata {
                        operations,
                        signature,
                    },
                ),
            };

            credit_da_fees.apply(host)?;

            // Extract cross-runtime side effects accumulated
            // during the Michelson execution (e.g. CRAC into EVM)
            // BEFORE the commit: `commit_evm_journal_from_external`
            // runs `JournalInner::finalize()` which clears
            // `inner.logs` as part of revm's standard cleanup, so
            // reading them after gives an empty buffer.
            let cross_runtime_effects =
                extract_cross_runtime_effects(journal, consumed_milligas);

            if let (Some(outbox_queue), Some(evm_block_constants)) =
                (outbox_queue, evm_block_constants)
            {
                let etherlink_withdrawals = commit_evm_journal_from_external(
                    host,
                    registry,
                    evm_block_constants,
                    journal,
                )?;
                push_withdrawals_to_outbox(host, outbox_queue, etherlink_withdrawals)?;
            }

            Ok(crate::apply::ExecutionResult::Valid(
                RuntimeExecutionInfo::Tezos {
                    op: applied_operation,
                    cross_runtime_effects,
                    consumed_milligas,
                },
            ))
        }
        TezlinkContent::Deposit(deposit) => {
            log!(Debug, "Execute Tezlink deposit: {deposit:?}");

            let deposit_result = execute_tezlink_deposit(host, &deposit)?;

            let source = PublicKeyHash::nom_read_exact(&TEZLINK_DEPOSITOR[1..]).unwrap();

            let applied_operation = AppliedOperation {
                hash: operation.tx_hash.into(),
                branch: BlockHash::from(
                    block_in_progress.ethereum_parent_hash.to_fixed_bytes(),
                ),
                op_and_receipt: OperationDataAndMetadata::OperationWithMetadata(
                    OperationBatchWithMetadata {
                        operations: vec![OperationWithMetadata {
                            content: ManagerOperationContent::Transaction(
                                ManagerOperation {
                                    source,
                                    fee: 0.into(),
                                    counter: 0.into(),
                                    gas_limit: 0.into(),
                                    storage_limit: 0.into(),
                                    operation: deposit_result.outcome.1,
                                },
                            ),
                            receipt: OperationResultSum::Transfer(OperationResult {
                                balance_updates: vec![],
                                result: deposit_result.outcome.0,
                                internal_operation_results: vec![],
                            }),
                        }],
                        signature: UnknownSignature::nom_read_exact(&[0u8; 64]).unwrap(),
                    },
                ),
            };
            Ok(crate::apply::ExecutionResult::Valid(
                RuntimeExecutionInfo::Tezos {
                    op: applied_operation,
                    cross_runtime_effects: Vec::new(),
                    // Deposits bypass the Michelson interpreter — no gas consumed.
                    consumed_milligas: 0,
                },
            ))
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct EvmLimits {
    pub maximum_gas_limit: u64,
    pub minimum_base_fee_per_gas: U256,
}

impl Default for EvmLimits {
    fn default() -> Self {
        Self {
            maximum_gas_limit: MAXIMUM_GAS_LIMIT,
            minimum_base_fee_per_gas: MINIMUM_BASE_FEE_PER_GAS.into(),
        }
    }
}

impl Default for TezosXChainConfig {
    fn default() -> Self {
        Self::create_config(
            U256::from(EVM_CHAIN_ID),
            EvmLimits::default(),
            SpecId::default(),
            ExperimentalFeatures::default(),
            DebugFeatures::default(),
            ChainId::from(EVM_CHAIN_ID.to_le_bytes()),
        )
    }
}

#[cfg(test)]
pub fn test_tezosx_chain_config() -> TezosXChainConfig {
    TezosXChainConfig::create_config(
        U256::from(EVM_CHAIN_ID),
        EvmLimits::default(),
        SpecId::default(),
        ExperimentalFeatures::default(),
        DebugFeatures::default(),
        ChainId::from(EVM_CHAIN_ID.to_le_bytes()),
    )
}

/// Reveal operation bytes from protocol encoding regression tests.
#[cfg(test)]
pub(crate) const REVEAL_OP_HEX: &str = "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a86b0002298c03ed7d454a101eb7022bc95f7e5f41ac7821dc05edecc004adcacdb7d401004798d2cc98473d7e250c898885718afd2e4efbcb1a1595ab9730761ed830de0f0066804fe735e06e97e26da8236b6341b91c625d5e82b3524ec0a88cc982365e70f8a5b9bc65df2ea6d21ee244cc3a96fb33031c394c78b1179ff1b8a44237740c";

/// Builds a test [`Operation`] from [`REVEAL_OP_HEX`]. Shared by the test
/// modules that exercise Tezos operation encoding/decoding.
#[cfg(test)]
pub(crate) fn make_test_operation() -> Operation {
    let bytes = hex::decode(REVEAL_OP_HEX).unwrap();
    Operation::nom_read_exact(&bytes).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tezlink_content_tezos_rlp_roundtrip() {
        let op = make_test_operation();
        let content = TezlinkContent::Tezos(op);
        let encoded = rlp::encode(&content);
        let decoded: TezlinkContent = rlp::decode(&encoded).unwrap();
        assert_eq!(decoded, content);
    }
}

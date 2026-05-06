// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    apply::{extract_cross_runtime_effects, RuntimeExecutionInfo},
    block_in_progress::BlockInProgress,
    blueprint_storage::{
        read_current_blueprint_header, BlueprintHeader, DelayedTransactionFetchingResult,
        EVMBlockHeader, TezBlockHeader,
    },
    bridge::{execute_tezlink_deposit, Deposit, TEZLINK_DEPOSITOR},
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
    CHAIN_ID,
};
use anyhow::Context;
use mir::ast::PublicKeyHash;
use num_traits::ToPrimitive;
use primitive_types::{H160, H256, U256};
use revm::primitives::hardfork::SpecId;
use revm_etherlink::{
    helpers::legacy::{h160_to_alloy, u256_to_alloy},
    inspectors::TracerInput,
    storage::world_state_handler::StorageAccount,
};
use rlp::{Decodable, DecoderError, Encodable};
use sha3::{Digest, Keccak256};
use std::fmt::{Debug, Display};
use tezos_crypto_rs::hash::{BlockHash, ChainId, UnknownSignature};
use tezos_data_encoding::{enc::BinWriter, nom::NomReader};
use tezos_ethereum::tx_common::EthereumTransactionCommon;
use tezos_ethereum::{
    rlp_helpers::{decode_field, decode_tx_hash, next},
    transaction::TransactionHash,
    wei::{eth_from_mutez, michelson_gas_to_mutez, mutez_from_wei},
};
use tezos_evm_logging::{log, Level::*};
use tezos_tezlink::operation::ManagerOperationField;

use tezos_execution::{
    context::{self, Context as _, TezlinkContext},
    get_required_da_fees,
    mir_ctx::BlockCtx,
    FeeRefundConfig, ProcessedOperation,
};
use tezos_smart_rollup::{outbox::OutboxQueue, types::Timestamp};
use tezos_smart_rollup_host::path::{OwnedPath, Path, RefPath};
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_host::wasm::WasmHost;
use tezos_tezlink::protocol::TARGET_TEZOS_PROTOCOL;
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
use tezosx_tezos_runtime::context::TezosRuntimeContext;

pub use tezos_evm_runtime::safe_storage::ETHERLINK_SAFE_STORAGE_ROOT_PATH;

pub const TEZ_SAFE_STORAGE_ROOT_PATH: RefPath = RefPath::assert_from(b"/tez/world_state");

/// Path for TezBlock storage. Sits under TEZ_SAFE_STORAGE_ROOT_PATH so it's
/// included in SafeStorage transactions and snapshotted as part of the
/// Michelson world state.
pub const TEZ_BLOCKS_PATH: RefPath = RefPath::assert_from(b"/tez/world_state/tez_blocks");

/// Unified SafeStorage root for all Tezos account state. Holds Michelson
/// contract / big_map state directly (ex `/tezlink/context/`) and TezosX
/// projected accounts under `tezosx/`.
pub const TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH: RefPath =
    RefPath::assert_from(b"/tez/tez_accounts");

#[derive(Clone, Copy, Debug)]
pub enum ChainFamily {
    Evm,
    Michelson,
}

impl Default for ChainFamily {
    fn default() -> Self {
        Self::Evm
    }
}

impl From<String> for ChainFamily {
    fn from(value: String) -> Self {
        match value.as_str() {
            "Michelson" => Self::Michelson,
            "Evm" => Self::Evm,
            _ => Self::default(),
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
pub struct EvmChainConfig {
    chain_id: U256,
    pub limits: EvmLimits,
    pub spec_id: SpecId,
    pub experimental_features: ExperimentalFeatures,
    michelson_chain_config: MichelsonChainConfig,
}

impl EvmChainConfig {
    pub fn is_tezos_runtime_enabled(&self, current_level: U256) -> bool {
        self.experimental_features
            .is_tezos_runtime_enabled(current_level)
    }

    pub fn tezos_runtime_feature_flag(&self) -> bool {
        self.experimental_features.tezos_runtime_feature_flag()
    }
}

#[derive(Debug)]
pub struct MichelsonChainConfig {
    pub chain_id: ChainId,
}

pub enum ChainConfig {
    Evm(Box<EvmChainConfig>),
    Michelson(MichelsonChainConfig),
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
                // We don't want the kernel to panic if there's an error
                // and we can't print a log as we don't have access to
                // the host. So we just ignore the result.
                let _ = tez.rlp_append(stream);
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

pub trait TransactionTrait: Into<TezosXTransaction> + TryFrom<TezosXTransaction> {
    fn is_delayed(&self) -> bool;

    fn tx_hash(&self) -> TransactionHash;

    fn data_size(&self) -> u64;
}

impl TransactionTrait for crate::transaction::Transaction {
    fn is_delayed(&self) -> bool {
        self.is_delayed()
    }

    fn tx_hash(&self) -> TransactionHash {
        self.tx_hash
    }

    fn data_size(&self) -> u64 {
        self.data_size()
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

impl TransactionTrait for TezlinkOperation {
    fn is_delayed(&self) -> bool {
        match self.content {
            TezlinkContent::Tezos(_) => false,
            TezlinkContent::Deposit(_) => true,
        }
    }

    fn tx_hash(&self) -> TransactionHash {
        self.tx_hash
    }

    fn data_size(&self) -> u64 {
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

pub trait ChainHeaderTrait {
    fn hash(&self) -> H256;

    fn genesis_header() -> Self;
}

impl ChainHeaderTrait for crate::blueprint_storage::EVMBlockHeader {
    fn hash(&self) -> H256 {
        self.hash
    }
    fn genesis_header() -> Self {
        EVMBlockHeader {
            hash: crate::block::GENESIS_PARENT_HASH,
            receipts_root: vec![0; 32],
            transactions_root: vec![0; 32],
        }
    }
}

impl ChainHeaderTrait for crate::blueprint_storage::TezBlockHeader {
    fn hash(&self) -> H256 {
        self.hash
    }

    fn genesis_header() -> Self {
        Self {
            hash: H256(*TezBlock::genesis_block_hash()),
            next_protocol: TARGET_TEZOS_PROTOCOL,
        }
    }
}

pub trait ChainConfigTrait: Debug {
    type BlockConstants;

    type ChainHeader: ChainHeaderTrait + Decodable;

    fn get_chain_id(&self) -> U256;

    fn init_registry(&self) -> RegistryImpl;

    fn get_chain_family(&self) -> ChainFamily;

    fn is_tezos_runtime_enabled(&self, current_level: U256) -> bool;

    fn storage_root_paths(&self, block_number: U256) -> Vec<RefPath>;

    fn constants(
        &self,
        host: &mut impl StorageV1,
        block_in_progress: &BlockInProgress,
        da_fee_per_byte: U256,
        coinbase: H160,
    ) -> anyhow::Result<Self::BlockConstants>;

    fn michelson_to_evm_gas_multiplier(&self, constants: &Self::BlockConstants) -> u64;

    fn fmt_with_family(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let chain_family = self.get_chain_family();
        write!(f, "{{Chain family: {chain_family}, {self:?}}}")
    }

    fn fetch_hashes_from_delayed_inbox<Host>(
        host: &mut Host,
        delayed_hashes: Vec<crate::delayed_inbox::Hash>,
        delayed_inbox: &mut DelayedInbox,
        current_blueprint_size: usize,
        block_number: U256,
    ) -> anyhow::Result<(DelayedTransactionFetchingResult<TezosXTransaction>, usize)>
    where
        Host: StorageV1;
    fn transaction_from_bytes(
        bytes: &[u8],
        blueprint_version: u8,
    ) -> anyhow::Result<TezosXTransaction>;
    fn base_fee_per_gas(&self, host: &impl StorageV1, timestamp: Timestamp) -> U256;
    fn can_fit_in_reboot(
        &self,
        executed_gas: U256,
        tx: &TezosXTransaction,
        block_constants: &Self::BlockConstants,
    ) -> anyhow::Result<bool>;
    #[allow(clippy::too_many_arguments)]
    fn apply_transaction<Host>(
        &self,
        block_in_progress: &BlockInProgress,
        host: &mut Host,
        registry: &impl Registry,
        outbox_queue: &OutboxQueue<'_, impl Path>,
        block_constants: &Self::BlockConstants,
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
        // Per-replay flag (from `block::produce` / `handle_run_transaction`)
        // asking the apply sites to persist HTTP CRAC traces. Plumbed as a
        // bool so the apply path never needs to probe durable storage for
        // the flag on its own.
        http_trace_enabled: bool,
    ) -> Result<crate::apply::ExecutionResult<RuntimeExecutionInfo>, anyhow::Error>
    where
        Host: StorageV1;
    fn finalize_and_store<Host>(
        &self,
        host: &mut Host,
        block_in_progress: BlockInProgress,
        block_constants: &Self::BlockConstants,
        chain_header: Self::ChainHeader,
    ) -> anyhow::Result<L2Block>
    where
        Host: StorageV1;
    fn start_simulation_mode<Host>(
        &self,
        host: &mut Host,
        registry: &impl Registry,
    ) -> anyhow::Result<()>
    where
        Host: StorageV1 + WasmHost;
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

impl TransactionTrait for TezosXTransaction {
    fn is_delayed(&self) -> bool {
        match self {
            Self::Ethereum(tx) => tx.is_delayed(),
            Self::Tezos(op) => op.is_delayed(),
        }
    }

    fn tx_hash(&self) -> TransactionHash {
        match self {
            Self::Ethereum(tx) => tx.tx_hash(),
            Self::Tezos(op) => op.tx_hash(),
        }
    }

    fn data_size(&self) -> u64 {
        match self {
            Self::Ethereum(tx) => tx.data_size(),
            Self::Tezos(op) => op.data_size(),
        }
    }
}

pub struct TezosXBlockConstants {
    pub evm_runtime_block_constants: tezos_ethereum::block::BlockConstants,
    #[allow(dead_code)]
    pub michelson_runtime_block_constants: TezlinkBlockConstants<TezosRuntimeContext>,
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

impl ChainConfigTrait for EvmChainConfig {
    type BlockConstants = TezosXBlockConstants;

    type ChainHeader = crate::blueprint_storage::EVMBlockHeader;

    fn get_chain_id(&self) -> U256 {
        self.chain_id
    }

    fn is_tezos_runtime_enabled(&self, current_level: U256) -> bool {
        self.experimental_features
            .is_tezos_runtime_enabled(current_level)
    }

    fn init_registry(&self) -> RegistryImpl {
        RegistryImpl::new(self.chain_id, self.michelson_chain_config.chain_id.clone())
    }

    fn get_chain_family(&self) -> ChainFamily {
        ChainFamily::Evm
    }

    fn constants(
        &self,
        host: &mut impl StorageV1,
        block_in_progress: &BlockInProgress,
        da_fee_per_byte: U256,
        coinbase: H160,
    ) -> anyhow::Result<Self::BlockConstants> {
        let level: BlockNumber = block_in_progress.number.try_into()?;
        let context =
            TezosRuntimeContext::from_root(&TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH)?;
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
                self.chain_id,
                self.limits.minimum_base_fee_per_gas,
                da_fee_per_byte,
                crate::block::GAS_LIMIT,
                coinbase,
                self.is_tezos_runtime_enabled(level.into()),
            ),
            michelson_runtime_block_constants: TezlinkBlockConstants {
                level,
                context,
                da_fee_per_byte_mutez,
                michelson_to_evm_gas_multiplier,
                safe_roots,
            },
        })
    }

    fn michelson_to_evm_gas_multiplier(&self, constants: &Self::BlockConstants) -> u64 {
        constants
            .michelson_runtime_block_constants
            .michelson_to_evm_gas_multiplier
    }

    fn base_fee_per_gas(&self, host: &impl StorageV1, timestamp: Timestamp) -> U256 {
        crate::gas_price::base_fee_per_gas(
            host,
            timestamp,
            self.get_limits().minimum_base_fee_per_gas,
        )
    }

    fn transaction_from_bytes(
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

    fn fetch_hashes_from_delayed_inbox<Host>(
        host: &mut Host,
        delayed_hashes: Vec<crate::delayed_inbox::Hash>,
        delayed_inbox: &mut DelayedInbox,
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

    fn can_fit_in_reboot(
        &self,
        executed_gas: U256,
        transaction: &TezosXTransaction,
        block_constants: &Self::BlockConstants,
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

    fn apply_transaction<Host>(
        &self,
        block_in_progress: &BlockInProgress,
        host: &mut Host,
        registry: &impl Registry,
        outbox_queue: &OutboxQueue<'_, impl Path>,
        block_constants: &Self::BlockConstants,
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
                    TransactionContent::TezosDelayed(_) => {
                        (0, block_in_progress.michelson_index)
                    } // Tezos
                };
                let crac_id = tezosx_journal::CracId::new(origin_runtime, id);
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
                    &block_constants.michelson_runtime_block_constants.safe_roots,
                )
            }
            TezosXTransaction::Tezos(operation) => {
                let tx_hash = operation.tx_hash;
                let crac_id =
                    tezosx_journal::CracId::new(0, block_in_progress.michelson_index);
                let mut journal = TezosXJournal::new(crac_id);
                let result = apply_tezos_operation(
                    &self.michelson_chain_config.chain_id,
                    block_in_progress,
                    host,
                    registry,
                    &block_constants.michelson_runtime_block_constants,
                    operation,
                    sequencer_pool_address,
                    skip_signature_check,
                    skip_fees_check,
                    Some(outbox_queue),
                    Some(&block_constants.evm_runtime_block_constants),
                    &mut journal,
                    self.experimental_features.is_michelson_gas_refund_enabled(),
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
    }

    fn finalize_and_store<Host>(
        &self,
        host: &mut Host,
        block_in_progress: BlockInProgress,
        block_constants: &Self::BlockConstants,
        _chain_header: Self::ChainHeader,
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

    fn start_simulation_mode<Host>(
        &self,
        host: &mut Host,
        registry: &impl Registry,
    ) -> anyhow::Result<()>
    where
        Host: StorageV1 + WasmHost,
    {
        start_simulation_mode(host, registry, &self.spec_id)
    }

    fn storage_root_paths(&self, block_number: U256) -> Vec<RefPath> {
        if self.is_tezos_runtime_enabled(block_number) {
            vec![
                ETHERLINK_SAFE_STORAGE_ROOT_PATH,
                TEZ_SAFE_STORAGE_ROOT_PATH,
                TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH,
            ]
        } else {
            vec![ETHERLINK_SAFE_STORAGE_ROOT_PATH]
        }
    }
}

impl EvmChainConfig {
    pub fn create_config(
        chain_id: U256,
        limits: EvmLimits,
        spec_id: SpecId,
        experimental_features: ExperimentalFeatures,
        michelson_runtime_chain_id: ChainId,
    ) -> Self {
        Self {
            chain_id,
            limits,
            spec_id,
            experimental_features,
            michelson_chain_config: MichelsonChainConfig::create_config(
                michelson_runtime_chain_id,
            ),
        }
    }

    pub fn get_limits(&self) -> &EvmLimits {
        &self.limits
    }

    pub fn limits_mut(&mut self) -> &mut EvmLimits {
        &mut self.limits
    }

    pub fn get_spec_id(&self) -> &SpecId {
        &self.spec_id
    }

    pub fn michelson_chain_config(&self) -> &MichelsonChainConfig {
        &self.michelson_chain_config
    }
}

const TEZLINK_SIMULATION_RESULT_PATH: RefPath =
    RefPath::assert_from(b"/tez/world_state/simulation_result");

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

pub struct TezlinkBlockConstants<Context: context::Context> {
    pub level: BlockNumber,
    pub context: Context,
    /// DA fee per byte in mutez, read once at block start from durable storage.
    /// When `disable_da_fees` is set, this is 0.
    pub da_fee_per_byte_mutez: u64,
    /// Conversion factor from Michelson gas to EVM gas, read once at block
    /// start from durable storage (falls back to the compile-time constant).
    pub michelson_to_evm_gas_multiplier: u64,
    /// SafeStorage roots to snapshot when validating/applying a Tezos
    /// operation. Mirrors `ChainConfigTrait::storage_root_paths` so the
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
) -> Result<FeesData, anyhow::Error> {
    if skip_fees_check {
        Ok(FeesData {
            required_fees: None,
            credit_da_fees: CreditDaFees::Skip,
        })
    } else {
        let required_da_fees = get_required_da_fees(operation, da_fee_per_byte_mutez)?;

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
    block_constants: &TezlinkBlockConstants<impl context::Context>,
    operation: TezlinkOperation,
    sequencer_pool_address: Option<H160>,
    skip_signature_check: bool,
    skip_fees_check: bool,
    outbox_queue: Option<&OutboxQueue<'_, impl Path>>,
    evm_block_constants: Option<&tezos_ethereum::block::BlockConstants>,
    external_journal: &mut TezosXJournal,
    enable_gas_refund: bool,
) -> Result<crate::apply::ExecutionResult<RuntimeExecutionInfo>, anyhow::Error>
where
    Host: StorageV1,
{
    let context = &block_constants.context;

    let level = block_constants.level;
    let now = block_in_progress.timestamp;
    let block_ctx = BlockCtx {
        level: &level,
        now: &now,
        chain_id,
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

            // Try to apply the operation with the tezos_execution crate, return a receipt
            // on whether it failed or not
            let journal = external_journal;
            let processed_operations = match tezos_execution::validate_and_apply_operation(
                host,
                registry,
                journal,
                context,
                hash.clone(),
                operation,
                &block_ctx,
                skip_signature_check,
                fees,
                fee_refund_config,
                &block_constants.safe_roots,
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
                    return Err(anyhow::anyhow!("CRAC block abort: {msg}"));
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

            let deposit_result = execute_tezlink_deposit(host, context, &deposit)?;

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

impl ChainConfigTrait for MichelsonChainConfig {
    type BlockConstants = TezlinkBlockConstants<TezlinkContext>;
    type ChainHeader = TezBlockHeader;

    fn get_chain_id(&self) -> U256 {
        self.chain_id.as_ref().into()
    }

    fn is_tezos_runtime_enabled(&self, _current_level: U256) -> bool {
        true
    }

    fn init_registry(&self) -> RegistryImpl {
        RegistryImpl::new(self.get_chain_id(), self.chain_id.clone())
    }

    fn get_chain_family(&self) -> ChainFamily {
        ChainFamily::Michelson
    }

    fn constants(
        &self,
        host: &mut impl StorageV1,
        block_in_progress: &BlockInProgress,
        da_fee_per_byte: U256,
        _coinbase: H160,
    ) -> anyhow::Result<Self::BlockConstants> {
        let level: BlockNumber = block_in_progress.number.try_into()?;
        let context =
            context::TezlinkContext::from_root(&TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH)?;
        let da_fee_per_byte_mutez = mutez_from_wei(da_fee_per_byte)
            .map_err(|_| crate::Error::InvalidConversion)?;
        let michelson_to_evm_gas_multiplier =
            read_or_init_michelson_to_evm_gas_multiplier(host);
        let safe_roots = self
            .storage_root_paths(level.into())
            .iter()
            .map(OwnedPath::from)
            .collect();
        Ok(TezlinkBlockConstants {
            level,
            context,
            da_fee_per_byte_mutez,
            michelson_to_evm_gas_multiplier,
            safe_roots,
        })
    }

    fn michelson_to_evm_gas_multiplier(&self, constants: &Self::BlockConstants) -> u64 {
        constants.michelson_to_evm_gas_multiplier
    }

    // Tezlink (pure Michelson chain) does not use dynamic, EIP-1559-style
    // pricing — its fee model follows Tezos L1 (flat mutez-per-gas-unit
    // declared by the operation). Returning zero here disables the
    // Etherlink-style dynamic base_fee and keeps fee computation in
    // strict Tezos semantics.
    //
    // Tezos X (EVM primary with embedded Michelson runtime) uses
    // `EvmChainConfig::base_fee_per_gas` instead, which does return the
    // dynamic Etherlink base fee.
    fn base_fee_per_gas(&self, _host: &impl StorageV1, _timestamp: Timestamp) -> U256 {
        U256::zero()
    }

    fn fetch_hashes_from_delayed_inbox<Host>(
        host: &mut Host,
        delayed_hashes: Vec<crate::delayed_inbox::Hash>,
        delayed_inbox: &mut DelayedInbox,
        current_blueprint_size: usize,
        block_number: U256,
    ) -> anyhow::Result<(DelayedTransactionFetchingResult<TezosXTransaction>, usize)>
    where
        Host: StorageV1,
    {
        // By reusing 'fetch_hashes_from_delayed_inbox', Tezlink don't have to implement
        // the logic to retrieve delayed_inbox items.
        //
        // However, it still needs to do the conversion as the returned items are EthTxs
        let (delayed, total_size) =
            crate::blueprint_storage::fetch_hashes_from_delayed_inbox(
                host,
                delayed_hashes,
                delayed_inbox,
                current_blueprint_size,
                block_number,
            )?;
        Ok((
            match delayed {
                DelayedTransactionFetchingResult::Ok(txs) => {
                    let mut ops = vec![];
                    for tx in txs.into_iter() {
                        if let TezosXTransaction::Ethereum(tx) = tx {
                            if let TransactionContent::Deposit(deposit) = tx.content {
                                let operation = TezlinkOperation {
                                    tx_hash: tx.tx_hash,
                                    content: TezlinkContent::Deposit(deposit),
                                };
                                ops.push(TezosXTransaction::Tezos(operation))
                            }
                        }
                    }
                    DelayedTransactionFetchingResult::Ok(ops)
                }
                DelayedTransactionFetchingResult::BlueprintTooLarge => {
                    DelayedTransactionFetchingResult::BlueprintTooLarge
                }
                DelayedTransactionFetchingResult::DelayedHashMissing(hash) => {
                    DelayedTransactionFetchingResult::DelayedHashMissing(hash)
                }
            },
            total_size,
        ))
    }

    fn transaction_from_bytes(
        bytes: &[u8],
        _version: u8,
    ) -> anyhow::Result<TezosXTransaction> {
        let operation = tezos_operation_from_bytes(bytes)?;
        Ok(operation.into())
    }

    fn can_fit_in_reboot(
        &self,
        _executed_gas: U256,
        _tx: &TezosXTransaction,
        _block_constants: &Self::BlockConstants,
    ) -> anyhow::Result<bool> {
        Ok(true)
    }

    fn apply_transaction<Host>(
        &self,
        block_in_progress: &BlockInProgress,
        host: &mut Host,
        registry: &impl Registry,
        _outbox_queue: &OutboxQueue<'_, impl Path>,
        block_constants: &Self::BlockConstants,
        transaction: TezosXTransaction,
        _index: u32,
        sequencer_pool_address: Option<H160>,
        _tracer_input: Option<TracerInput>,
        skip_signature_check: bool,
        skip_fees_check: bool,
        http_trace_enabled: bool,
    ) -> Result<crate::apply::ExecutionResult<RuntimeExecutionInfo>, anyhow::Error>
    where
        Host: StorageV1,
    {
        match transaction {
            TezosXTransaction::Ethereum(_transaction) => {
                anyhow::bail!("Unexpected Ethereum transaction in Michelson chain family")
            }
            TezosXTransaction::Tezos(operation) => {
                let tx_hash = operation.tx_hash;
                let crac_id =
                    tezosx_journal::CracId::new(0, block_in_progress.michelson_index);
                let mut journal = TezosXJournal::new(crac_id);
                // Not supported in standalone Tezlink (no ExperimentalFeatures).
                let enable_gas_refund = false;
                let result = apply_tezos_operation(
                    &self.chain_id,
                    block_in_progress,
                    host,
                    registry,
                    block_constants,
                    operation,
                    sequencer_pool_address,
                    skip_signature_check,
                    skip_fees_check,
                    None::<&OutboxQueue<'_, RefPath>>,
                    None,
                    &mut journal,
                    enable_gas_refund,
                );
                crate::storage::maybe_store_http_traces_for_tx(
                    host,
                    http_trace_enabled,
                    &tx_hash,
                    &journal,
                );
                result
            }
        }
    }

    fn finalize_and_store<Host>(
        &self,
        host: &mut Host,
        mut block_in_progress: BlockInProgress,
        block_constants: &Self::BlockConstants,
        chain_header: Self::ChainHeader,
    ) -> anyhow::Result<L2Block>
    where
        Host: StorageV1,
    {
        // After a protocol upgrade, the first block has
        // chain_header.next_protocol != TARGET_TEZOS_PROTOCOL (it still
        // reflects the old protocol). In that case the block must be empty
        // since we cannot execute transactions from the previous protocol.
        anyhow::ensure!(
            chain_header.next_protocol == TARGET_TEZOS_PROTOCOL
                || block_in_progress
                    .cumulative_tezos_operation_receipts
                    .list
                    .is_empty(),
            "Non-empty block with mismatched protocol: expected {:?} or empty block, \
             got protocol {:?} with {} operations",
            TARGET_TEZOS_PROTOCOL,
            chain_header.next_protocol,
            block_in_progress
                .cumulative_tezos_operation_receipts
                .list
                .len()
        );

        // Assign block-sequential nonces to all internal operations.
        // Each operation uses 0-based local nonces during execution;
        // this final pass makes them L1-compliant (sequential across
        // the entire block).
        crate::apply::renumber_nonces(
            &mut block_in_progress.cumulative_tezos_operation_receipts.list,
        );
        // Standalone Tezlink has no EVM txs, so `blueprint_hash` only
        // commits to the Michelson ops and the timestamp. Kept in the
        // same shape as the TezosX path so a future extension (e.g.
        // delayed Tezlink ops) only needs to fill the empty vectors.
        let michelson_commitment = crate::state_hash::michelson_ops_commitment(
            &block_in_progress.cumulative_tezos_operation_receipts.list,
        );
        let blueprint_hash = crate::state_hash::blueprint_hash(
            &[],
            &[],
            &michelson_commitment,
            block_in_progress.timestamp,
        );
        let state_root =
            crate::state_hash::tez_accounts_state_hash(host, &blueprint_hash)
                .try_into()
                .expect("tez_accounts_state_hash must be 32 bytes");
        let tezblock = TezBlock::new(
            chain_header.next_protocol,
            TARGET_TEZOS_PROTOCOL,
            block_constants.level,
            block_in_progress.timestamp,
            block_in_progress.ethereum_parent_hash,
            block_in_progress.cumulative_tezos_operation_receipts.list,
            state_root,
        )?;
        let new_block = L2Block::Tezlink(tezblock);
        crate::block_storage::store_current(host, &TEZ_BLOCKS_PATH, &new_block)
            .context("Failed to store the current block")?;
        Ok(new_block)
    }

    fn start_simulation_mode<Host>(
        &self,
        host: &mut Host,
        registry: &impl Registry,
    ) -> anyhow::Result<()>
    where
        Host: StorageV1 + WasmHost,
    {
        fn read_inbox_message(
            host: &mut impl WasmHost,
        ) -> anyhow::Result<tezos_smart_rollup_host::input::Message> {
            match host.read_input()? {
                Some(input) => Ok(input),
                None => {
                    Err(crate::error::TezlinkSimulationError::UnexpectedEndOfInbox.into())
                }
            }
        }

        // We expect the inbox to contain exactly two messages; the
        // first message is a single byte indicating if signature must
        // be checked (0x01) or not (0x00); the second message is the
        // operation to simulate.
        let skip_signature_check = {
            let input = read_inbox_message(host)?;
            match input.as_ref() {
                &[0x00] => true,
                &[0x01] => false,
                input => {
                    return Err(
                        crate::error::TezlinkSimulationError::UnexpectedSkipSigTag(
                            input.to_vec(),
                        )
                        .into(),
                    );
                }
            }
        };

        let nb_chunks =
            u16::from_le_bytes(read_inbox_message(host)?.as_ref().try_into()?);

        let mut operation_bytes = vec![];
        for _chunk in 0..nb_chunks {
            operation_bytes.extend(read_inbox_message(host)?.as_ref())
        }

        let operation = {
            Operation::nom_read_exact(&operation_bytes)
                .map_err(|_| crate::error::Error::InvalidConversion)?
        };
        let hash = operation.hash()?;
        log!(Debug,
            "Tezlink simulation starts for operation hash {hash:?}, skip signature flag: {skip_signature_check:?}, operation length: {:?}, number of chunks: {nb_chunks:?}",
            operation_bytes.len()
        );
        let context =
            context::TezlinkContext::from_root(&TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH)?;

        let BlueprintHeader { number, timestamp } = read_current_blueprint_header(host)?;
        let block_ctx = BlockCtx {
            level: &number.as_u32().into(),
            now: &timestamp,
            chain_id: &self.chain_id,
        };
        let branch = operation.branch.clone();
        let signature = operation.signature.clone();
        // During simulation, skip the fee check: the client sends fee=0
        // because it doesn't know the fees yet (that's what simulation computes).
        // The OCaml node-side prevalidation also skips fees during simulation.
        let mut tezosx_journal = TezosXJournal::default();
        let safe_roots: Vec<OwnedPath> = self
            .storage_root_paths(number)
            .iter()
            .map(OwnedPath::from)
            .collect();
        let processed_operations = tezos_execution::validate_and_apply_operation(
            host,
            registry,
            &mut tezosx_journal,
            &context,
            hash.clone(),
            operation.clone(),
            &block_ctx,
            skip_signature_check,
            None,
            None, // No fee refund in Tezlink
            &safe_roots,
        )?;
        let operations = ProcessedOperation::into_receipts(processed_operations);
        let result = AppliedOperation {
            hash,
            branch,
            op_and_receipt: OperationDataAndMetadata::OperationWithMetadata(
                OperationBatchWithMetadata {
                    operations,
                    signature,
                },
            ),
        };

        log!(Debug, "Tezlink simulation finished, result: {:?}", result);
        host.store_write_all(&TEZLINK_SIMULATION_RESULT_PATH, &result.to_bytes()?)?;
        Ok(())
    }

    fn storage_root_paths(&self, _block_number: U256) -> Vec<RefPath> {
        vec![
            ETHERLINK_SAFE_STORAGE_ROOT_PATH,
            TEZ_SAFE_STORAGE_ROOT_PATH,
            TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH,
        ]
    }
}

impl MichelsonChainConfig {
    pub fn create_config(chain_id: ChainId) -> Self {
        Self { chain_id }
    }
}

impl ChainConfig {
    pub fn new_evm_config(
        chain_id: U256,
        limits: EvmLimits,
        spec_id: SpecId,
        experimental_features: ExperimentalFeatures,
        michelson_runtime_chain_id: ChainId,
    ) -> Self {
        ChainConfig::Evm(Box::new(EvmChainConfig::create_config(
            chain_id,
            limits,
            spec_id,
            experimental_features,
            michelson_runtime_chain_id,
        )))
    }

    pub fn new_michelson_config(chain_id: ChainId) -> Self {
        ChainConfig::Michelson(MichelsonChainConfig::create_config(chain_id))
    }

    pub fn get_chain_family(&self) -> ChainFamily {
        match self {
            ChainConfig::Evm(_) => ChainFamily::Evm,
            ChainConfig::Michelson(_) => ChainFamily::Michelson,
        }
    }

    pub fn get_chain_id(&self) -> U256 {
        match self {
            ChainConfig::Evm(evm_chain_config) => evm_chain_config.get_chain_id(),
            ChainConfig::Michelson(michelson_chain_config) => {
                michelson_chain_config.get_chain_id()
            }
        }
    }
}

impl Display for ChainFamily {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Evm => write!(f, "EVM"),
            Self::Michelson => write!(f, "Michelson"),
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

impl Default for ChainConfig {
    fn default() -> Self {
        ChainConfig::Evm(Box::default())
    }
}

impl Display for ChainConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ChainConfig::Evm(evm_chain_config) => evm_chain_config.fmt_with_family(f),
            ChainConfig::Michelson(michelson_chain_config) => {
                michelson_chain_config.fmt_with_family(f)
            }
        }
    }
}

impl Default for EvmChainConfig {
    fn default() -> Self {
        Self::create_config(
            U256::from(CHAIN_ID),
            EvmLimits::default(),
            SpecId::default(),
            ExperimentalFeatures::default(),
            ChainId::from(CHAIN_ID.to_le_bytes()),
        )
    }
}

#[cfg(test)]
pub fn test_evm_chain_config() -> EvmChainConfig {
    EvmChainConfig::create_config(
        U256::from(CHAIN_ID),
        EvmLimits::default(),
        SpecId::default(),
        ExperimentalFeatures::default(),
        ChainId::from(CHAIN_ID.to_le_bytes()),
    )
}

#[cfg(test)]
pub fn test_chain_config() -> ChainConfig {
    ChainConfig::Evm(Box::new(test_evm_chain_config()))
}

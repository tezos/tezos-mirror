// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    apply::RuntimeExecutionInfo,
    block_in_progress::BlockInProgress,
    blueprint_storage::{
        read_current_blueprint_header, BlueprintHeader, DelayedTransactionFetchingResult,
        EVMBlockHeader, TezBlockHeader,
    },
    bridge::{execute_tezlink_deposit, Deposit, TEZLINK_DEPOSITOR},
    delayed_inbox::DelayedInbox,
    error,
    fees::MINIMUM_BASE_FEE_PER_GAS,
    l2block::L2Block,
    simulation::start_simulation_mode,
    tick_model::constants::MAXIMUM_GAS_LIMIT,
    transaction::TransactionContent,
    CHAIN_ID,
};
use anyhow::Context;
use mir::ast::PublicKeyHash;
use primitive_types::{H160, H256, U256};
use revm::primitives::hardfork::SpecId;
use revm_etherlink::inspectors::TracerInput;
use rlp::{Decodable, DecoderError, Encodable};
use sha3::{Digest, Keccak256};
use std::fmt::{Debug, Display};
use tezos_crypto_rs::hash::{BlockHash, ChainId, UnknownSignature};
use tezos_data_encoding::{enc::BinWriter, nom::NomReader};
use tezos_ethereum::tx_common::EthereumTransactionCommon;
use tezos_ethereum::{
    rlp_helpers::{decode_field, decode_tx_hash, next},
    transaction::TransactionHash,
};
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_execution::{
    context::{self, Context as _},
    mir_ctx::BlockCtx,
};
use tezos_smart_rollup::{outbox::OutboxQueue, types::Timestamp};
use tezos_smart_rollup_host::path::{Path, RefPath};
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

pub use tezos_evm_runtime::safe_storage::ETHERLINK_SAFE_STORAGE_ROOT_PATH;

pub const TEZLINK_SAFE_STORAGE_ROOT_PATH: RefPath = RefPath::assert_from(b"/tezlink");

/// Path for TezBlock storage within the EVM world state (used by EVM chain config).
/// This path is under ETHERLINK_SAFE_STORAGE_ROOT_PATH so it's included in SafeStorage transactions.
pub const TEZOS_BLOCKS_PATH: RefPath = RefPath::assert_from(b"/evm/world_state/tezlink");

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
    enable_tezos_runtime: bool,
}

impl ExperimentalFeatures {
    pub fn read_from_storage(host: &mut impl Runtime) -> Self {
        let enable_tezos_runtime = crate::storage::enable_tezos_runtime(host);

        ExperimentalFeatures {
            enable_tezos_runtime,
        }
    }

    pub fn is_tezos_runtime_enabled(&self) -> bool {
        self.enable_tezos_runtime
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
    pub fn enable_tezos_runtime(&self) -> bool {
        self.experimental_features.enable_tezos_runtime
    }
}

#[derive(Debug)]
pub struct MichelsonChainConfig {
    chain_id: ChainId,
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
        }
    }
}

pub trait ChainConfigTrait: Debug {
    type BlockConstants;

    type Transaction: TransactionTrait + Encodable + Decodable + Debug;

    type ChainHeader: ChainHeaderTrait + Decodable;

    type ExecutionInfo;

    fn get_chain_id(&self) -> U256;

    fn get_chain_family(&self) -> ChainFamily;

    fn storage_root_paths(&self) -> Vec<RefPath>;

    fn constants<Tx: TransactionTrait>(
        &self,
        block_in_progress: &BlockInProgress<Tx>,
        da_fee_per_byte: U256,
        coinbase: H160,
    ) -> anyhow::Result<Self::BlockConstants>;

    fn fmt_with_family(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let chain_family = self.get_chain_family();
        write!(f, "{{Chain family: {chain_family}, {self:?}}}")
    }

    fn fetch_hashes_from_delayed_inbox(
        host: &mut impl Runtime,
        delayed_hashes: Vec<crate::delayed_inbox::Hash>,
        delayed_inbox: &mut DelayedInbox,
        current_blueprint_size: usize,
    ) -> anyhow::Result<(DelayedTransactionFetchingResult<TezosXTransaction>, usize)>;

    fn transaction_from_bytes(
        host: &mut impl Runtime,
        bytes: &[u8],
        blueprint_version: u8,
    ) -> anyhow::Result<TezosXTransaction>;

    fn base_fee_per_gas(&self, host: &impl Runtime, timestamp: Timestamp) -> U256;

    fn read_block_in_progress(
        host: &impl Runtime,
    ) -> anyhow::Result<Option<BlockInProgress<Self::Transaction>>>;

    fn can_fit_in_reboot(
        &self,
        executed_gas: U256,
        tx: &Self::Transaction,
        block_constants: &Self::BlockConstants,
    ) -> anyhow::Result<bool>;

    #[allow(clippy::too_many_arguments)]
    fn apply_transaction(
        &self,
        block_in_progress: &BlockInProgress<Self::Transaction>,
        host: &mut impl Runtime,
        registry: &impl Registry,
        outbox_queue: &OutboxQueue<'_, impl Path>,
        block_constants: &Self::BlockConstants,
        transaction: Self::Transaction,
        index: u32,
        sequencer_pool_address: Option<H160>,
        tracer_input: Option<TracerInput>,
    ) -> Result<crate::apply::ExecutionResult<Self::ExecutionInfo>, anyhow::Error>;

    fn register_valid_transaction(
        &self,
        block_in_progress: &mut BlockInProgress<Self::Transaction>,
        execution_info: Self::ExecutionInfo,
        host: &mut impl Runtime,
    ) -> anyhow::Result<()>;

    fn finalize_and_store(
        &self,
        host: &mut impl Runtime,
        block_in_progress: BlockInProgress<Self::Transaction>,
        block_constants: &Self::BlockConstants,
    ) -> anyhow::Result<L2Block>;

    fn start_simulation_mode(
        &self,
        host: &mut impl Runtime,
        registry: &impl Registry,
    ) -> anyhow::Result<()>;
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
    pub michelson_runtime_block_constants: TezlinkBlockConstants,
}

impl ChainConfigTrait for EvmChainConfig {
    type BlockConstants = TezosXBlockConstants;

    type Transaction = crate::transaction::Transaction;

    type ChainHeader = crate::blueprint_storage::EVMBlockHeader;

    type ExecutionInfo = crate::apply::RuntimeExecutionInfo;

    fn get_chain_id(&self) -> U256 {
        self.chain_id
    }

    fn get_chain_family(&self) -> ChainFamily {
        ChainFamily::Evm
    }

    fn constants<Tx: TransactionTrait>(
        &self,
        block_in_progress: &BlockInProgress<Tx>,
        da_fee_per_byte: U256,
        coinbase: H160,
    ) -> anyhow::Result<Self::BlockConstants> {
        Ok(TezosXBlockConstants {
            evm_runtime_block_constants: block_in_progress.constants(
                self.chain_id,
                self.limits.minimum_base_fee_per_gas,
                da_fee_per_byte,
                crate::block::GAS_LIMIT,
                coinbase,
                self.enable_tezos_runtime(),
            ),
            michelson_runtime_block_constants: self.michelson_chain_config.constants(
                block_in_progress,
                da_fee_per_byte,
                coinbase,
            )?,
        })
    }

    fn base_fee_per_gas(&self, host: &impl Runtime, timestamp: Timestamp) -> U256 {
        crate::gas_price::base_fee_per_gas(
            host,
            timestamp,
            self.get_limits().minimum_base_fee_per_gas,
        )
    }

    fn transaction_from_bytes(
        host: &mut impl Runtime,
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
                        log!(host, Error, "Unknown runtime id tag: {tag}, {message}");
                        Err(DecoderError::Custom("Unknown runtime id").into())
                    }
                }
            }
            _ => {
                log!(
                    host,
                    Error,
                    "Unknown blueprint version: {blueprint_version:?}"
                );
                Err(DecoderError::Custom("Unknown blueprint version").into())
            }
        }
    }

    fn fetch_hashes_from_delayed_inbox(
        host: &mut impl Runtime,
        delayed_hashes: Vec<crate::delayed_inbox::Hash>,
        delayed_inbox: &mut DelayedInbox,
        current_blueprint_size: usize,
    ) -> anyhow::Result<(DelayedTransactionFetchingResult<TezosXTransaction>, usize)>
    {
        crate::blueprint_storage::fetch_hashes_from_delayed_inbox(
            host,
            delayed_hashes,
            delayed_inbox,
            current_blueprint_size,
        )
    }

    fn read_block_in_progress(
        host: &impl Runtime,
    ) -> anyhow::Result<Option<BlockInProgress<Self::Transaction>>> {
        crate::storage::read_block_in_progress(host)
    }

    fn can_fit_in_reboot(
        &self,
        executed_gas: U256,
        transaction: &Self::Transaction,
        block_constants: &Self::BlockConstants,
    ) -> anyhow::Result<bool> {
        Ok(crate::block::can_fit_in_reboot(
            &self.limits,
            executed_gas,
            transaction.execution_gas_limit(
                &block_constants.evm_runtime_block_constants.block_fees,
            )?,
        ))
    }

    fn apply_transaction(
        &self,
        _block_in_progress: &BlockInProgress<Self::Transaction>,
        host: &mut impl Runtime,
        registry: &impl Registry,
        outbox_queue: &OutboxQueue<'_, impl Path>,
        block_constants: &Self::BlockConstants,
        transaction: Self::Transaction,
        index: u32,
        sequencer_pool_address: Option<H160>,
        tracer_input: Option<TracerInput>,
    ) -> Result<crate::apply::ExecutionResult<Self::ExecutionInfo>, anyhow::Error> {
        crate::apply::apply_transaction(
            host,
            registry,
            outbox_queue,
            &block_constants.evm_runtime_block_constants,
            transaction,
            index,
            sequencer_pool_address,
            tracer_input,
            &self.spec_id,
            &self.limits,
        )
    }

    fn register_valid_transaction(
        &self,
        block_in_progress: &mut BlockInProgress<Self::Transaction>,
        execution_info: Self::ExecutionInfo,
        host: &mut impl Runtime,
    ) -> anyhow::Result<()> {
        block_in_progress.register_valid_transaction(execution_info, host)
    }

    fn finalize_and_store(
        &self,
        host: &mut impl Runtime,
        block_in_progress: BlockInProgress<Self::Transaction>,
        block_constants: &Self::BlockConstants,
    ) -> anyhow::Result<L2Block> {
        block_in_progress.finalize_and_store(
            host,
            &block_constants.evm_runtime_block_constants,
            self.enable_tezos_runtime(),
        )
    }

    fn start_simulation_mode(
        &self,
        host: &mut impl Runtime,
        registry: &impl Registry,
    ) -> anyhow::Result<()> {
        start_simulation_mode(host, registry, &self.spec_id)
    }

    fn storage_root_paths(&self) -> Vec<RefPath> {
        if self.enable_tezos_runtime() {
            vec![
                ETHERLINK_SAFE_STORAGE_ROOT_PATH,
                TEZLINK_SAFE_STORAGE_ROOT_PATH,
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
}

const TEZLINK_SIMULATION_RESULT_PATH: RefPath =
    RefPath::assert_from(b"/tezlink/simulation_result");

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
    pub context: context::TezlinkContext,
}

fn apply_tezos_operation(
    chain_id: &ChainId,
    block_in_progress: &BlockInProgress<TezosXTransaction>,
    host: &mut impl Runtime,
    registry: &impl Registry,
    block_constants: &TezlinkBlockConstants,
    operation: TezlinkOperation,
) -> Result<crate::apply::ExecutionResult<RuntimeExecutionInfo>, anyhow::Error> {
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

            let skip_signature_check = false;

            let branch = operation.branch.clone();
            let signature = operation.signature.clone();

            // Try to apply the operation with the tezos_execution crate, return a receipt
            // on whether it failed or not
            let processed_operations = match tezos_execution::validate_and_apply_operation(
                host,
                registry,
                context,
                hash.clone(),
                operation,
                &block_ctx,
                skip_signature_check,
            ) {
                Ok(receipt) => receipt,
                Err(OperationError::Validation(err)) => {
                    log!(
                        host,
                        Error,
                        "Found an invalid operation, dropping it: {:?}",
                        err
                    );
                    return Ok(crate::apply::ExecutionResult::Invalid);
                }
                Err(OperationError::RuntimeError(err)) => {
                    return Err(err.into());
                }
            };

            // Add the applied operation in the block in progress
            let applied_operation = AppliedOperation {
                hash,
                branch,
                op_and_receipt: OperationDataAndMetadata::OperationWithMetadata(
                    OperationBatchWithMetadata {
                        operations: processed_operations,
                        signature,
                    },
                ),
            };
            Ok(crate::apply::ExecutionResult::Valid(
                RuntimeExecutionInfo::Tezos(applied_operation),
            ))
        }
        TezlinkContent::Deposit(deposit) => {
            log!(host, Debug, "Execute Tezlink deposit: {deposit:?}");

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
                RuntimeExecutionInfo::Tezos(applied_operation),
            ))
        }
    }
}

impl ChainConfigTrait for MichelsonChainConfig {
    type BlockConstants = TezlinkBlockConstants;
    type Transaction = TezosXTransaction;
    type ChainHeader = TezBlockHeader;
    type ExecutionInfo = RuntimeExecutionInfo;

    fn get_chain_id(&self) -> U256 {
        self.chain_id.as_ref().into()
    }

    fn get_chain_family(&self) -> ChainFamily {
        ChainFamily::Michelson
    }

    fn constants<Tx: TransactionTrait>(
        &self,
        block_in_progress: &BlockInProgress<Tx>,
        _da_fee_per_byte: U256,
        _coinbase: H160,
    ) -> anyhow::Result<Self::BlockConstants> {
        let level = block_in_progress.number.try_into()?;
        let context =
            context::TezlinkContext::from_root(&TEZLINK_SAFE_STORAGE_ROOT_PATH)?;
        Ok(TezlinkBlockConstants { level, context })
    }

    fn base_fee_per_gas(&self, _host: &impl Runtime, _timestamp: Timestamp) -> U256 {
        U256::zero()
    }

    fn fetch_hashes_from_delayed_inbox(
        host: &mut impl Runtime,
        delayed_hashes: Vec<crate::delayed_inbox::Hash>,
        delayed_inbox: &mut DelayedInbox,
        current_blueprint_size: usize,
    ) -> anyhow::Result<(DelayedTransactionFetchingResult<TezosXTransaction>, usize)>
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
        _host: &mut impl Runtime,
        bytes: &[u8],
        _version: u8,
    ) -> anyhow::Result<TezosXTransaction> {
        let operation = tezos_operation_from_bytes(bytes)?;
        Ok(operation.into())
    }

    fn read_block_in_progress(
        _host: &impl Runtime,
    ) -> anyhow::Result<Option<BlockInProgress<Self::Transaction>>> {
        Ok(None)
    }

    fn can_fit_in_reboot(
        &self,
        _executed_gas: U256,
        _tx: &Self::Transaction,
        _block_constants: &Self::BlockConstants,
    ) -> anyhow::Result<bool> {
        Ok(true)
    }

    fn apply_transaction(
        &self,
        block_in_progress: &BlockInProgress<Self::Transaction>,
        host: &mut impl Runtime,
        registry: &impl Registry,
        _outbox_queue: &OutboxQueue<'_, impl Path>,
        block_constants: &Self::BlockConstants,
        transaction: Self::Transaction,
        _index: u32,
        _sequencer_pool_address: Option<H160>,
        _tracer_input: Option<TracerInput>,
    ) -> Result<crate::apply::ExecutionResult<Self::ExecutionInfo>, anyhow::Error> {
        match transaction {
            TezosXTransaction::Ethereum(_transaction) => {
                anyhow::bail!("Unexpected Ethereum transaction in Michelson chain family")
            }
            TezosXTransaction::Tezos(operation) => apply_tezos_operation(
                &self.chain_id,
                block_in_progress,
                host,
                registry,
                block_constants,
                operation,
            ),
        }
    }

    fn register_valid_transaction(
        &self,
        block_in_progress: &mut BlockInProgress<Self::Transaction>,
        execution_info: Self::ExecutionInfo,
        host: &mut impl Runtime,
    ) -> anyhow::Result<()> {
        block_in_progress.register_valid_transaction(execution_info, host)
    }

    fn finalize_and_store(
        &self,
        host: &mut impl Runtime,
        block_in_progress: BlockInProgress<Self::Transaction>,
        block_constants: &Self::BlockConstants,
    ) -> anyhow::Result<L2Block> {
        // Create a Tezos block from the block in progress
        let tezblock = TezBlock::new(
            block_constants.level,
            block_in_progress.timestamp,
            block_in_progress.ethereum_parent_hash,
            block_in_progress.cumulative_tezos_operation_receipts.list,
        )?;
        let new_block = L2Block::Tezlink(tezblock);
        let root = TEZLINK_SAFE_STORAGE_ROOT_PATH;
        crate::block_storage::store_current(host, &root, &new_block)
            .context("Failed to store the current block")?;
        Ok(new_block)
    }

    fn start_simulation_mode(
        &self,
        host: &mut impl Runtime,
        registry: &impl Registry,
    ) -> anyhow::Result<()> {
        fn read_inbox_message(
            host: &mut impl Runtime,
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
        log!(
            host,
            Debug,
            "Tezlink simulation starts for operation hash {hash:?}, skip signature flag: {skip_signature_check:?}, operation length: {:?}, number of chunks: {nb_chunks:?}",
            operation_bytes.len()
        );
        let context =
            context::TezlinkContext::from_root(&TEZLINK_SAFE_STORAGE_ROOT_PATH)?;

        let BlueprintHeader { number, timestamp } = read_current_blueprint_header(host)?;
        let block_ctx = BlockCtx {
            level: &number.as_u32().into(),
            now: &timestamp,
            chain_id: &self.chain_id,
        };
        let branch = operation.branch.clone();
        let signature = operation.signature.clone();
        let operations = tezos_execution::validate_and_apply_operation(
            host,
            registry,
            &context,
            hash.clone(),
            operation.clone(),
            &block_ctx,
            skip_signature_check,
        )?;
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

        log!(
            host,
            Debug,
            "Tezlink simulation finished, result: {:?}",
            result
        );
        host.store_write_all(&TEZLINK_SIMULATION_RESULT_PATH, &result.to_bytes()?)?;
        Ok(())
    }

    fn storage_root_paths(&self) -> Vec<RefPath> {
        vec![TEZLINK_SAFE_STORAGE_ROOT_PATH]
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

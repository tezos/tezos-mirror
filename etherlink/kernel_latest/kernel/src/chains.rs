// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    block::{BlockComputationResult, TickCounter},
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
use std::fmt::{Debug, Display};
use tezos_crypto_rs::hash::{ChainId, UnknownSignature};
use tezos_data_encoding::{enc::BinWriter, nom::NomReader};
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
    operation::{ManagerOperation, ManagerOperationContent, Operation},
    operation_result::{
        OperationBatchWithMetadata, OperationDataAndMetadata, OperationError,
        OperationResult, OperationResultSum, OperationWithMetadata,
    },
};

pub const ETHERLINK_SAFE_STORAGE_ROOT_PATH: RefPath =
    RefPath::assert_from(b"/evm/world_state");

pub const TEZLINK_SAFE_STORAGE_ROOT_PATH: RefPath = RefPath::assert_from(b"/tezlink");

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

#[derive(Debug)]
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

#[derive(Debug)]
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

pub trait TransactionTrait {
    fn is_delayed(&self) -> bool;

    fn tx_hash(&self) -> TransactionHash;
}

impl TransactionTrait for crate::transaction::Transaction {
    fn is_delayed(&self) -> bool {
        self.is_delayed()
    }

    fn tx_hash(&self) -> TransactionHash {
        self.tx_hash
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
            hash: TezBlock::genesis_block_hash(),
        }
    }
}

pub trait ChainConfigTrait: Debug {
    type Transaction: TransactionTrait + Encodable + Decodable + Debug;

    type TransactionReceipt: Debug;

    type ChainHeader: ChainHeaderTrait + Decodable;

    fn get_chain_id(&self) -> U256;

    fn get_chain_family(&self) -> ChainFamily;

    fn storage_root_path(&self) -> RefPath;

    fn fmt_with_family(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let chain_family = self.get_chain_family();
        write!(f, "{{Chain family: {chain_family}, {self:?}}}")
    }

    fn fetch_hashes_from_delayed_inbox(
        host: &mut impl Runtime,
        delayed_hashes: Vec<crate::delayed_inbox::Hash>,
        delayed_inbox: &mut DelayedInbox,
        current_blueprint_size: usize,
    ) -> anyhow::Result<(DelayedTransactionFetchingResult<Self::Transaction>, usize)>;

    fn transactions_from_bytes(
        bytes: Vec<Vec<u8>>,
    ) -> anyhow::Result<Vec<Self::Transaction>>;

    fn base_fee_per_gas(&self, host: &impl Runtime, timestamp: Timestamp) -> U256;

    fn read_block_in_progress(
        host: &impl Runtime,
    ) -> anyhow::Result<
        Option<BlockInProgress<Self::Transaction, Self::TransactionReceipt>>,
    >;

    #[allow(clippy::too_many_arguments)]
    fn compute_bip<Host: Runtime>(
        &self,
        host: &mut Host,
        outbox_queue: &OutboxQueue<'_, impl Path>,
        block_in_progress: BlockInProgress<Self::Transaction, Self::TransactionReceipt>,
        tick_counter: &mut TickCounter,
        sequencer_pool_address: Option<H160>,
        maximum_allowed_ticks: u64,
        tracer_input: Option<TracerInput>,
        da_fee_per_byte: U256,
        coinbase: H160,
    ) -> anyhow::Result<BlockComputationResult>;

    fn start_simulation_mode(&self, host: &mut impl Runtime) -> anyhow::Result<()>;
}

impl ChainConfigTrait for EvmChainConfig {
    type Transaction = crate::transaction::Transaction;

    type TransactionReceipt = tezos_ethereum::transaction::TransactionReceipt;

    type ChainHeader = crate::blueprint_storage::EVMBlockHeader;

    fn get_chain_id(&self) -> U256 {
        self.chain_id
    }

    fn get_chain_family(&self) -> ChainFamily {
        ChainFamily::Evm
    }

    fn base_fee_per_gas(&self, host: &impl Runtime, timestamp: Timestamp) -> U256 {
        crate::gas_price::base_fee_per_gas(
            host,
            timestamp,
            self.get_limits().minimum_base_fee_per_gas,
        )
    }

    fn transactions_from_bytes(
        bytes: Vec<Vec<u8>>,
    ) -> anyhow::Result<Vec<Self::Transaction>> {
        crate::blueprint_storage::transactions_from_bytes(bytes)
    }

    fn fetch_hashes_from_delayed_inbox(
        host: &mut impl Runtime,
        delayed_hashes: Vec<crate::delayed_inbox::Hash>,
        delayed_inbox: &mut DelayedInbox,
        current_blueprint_size: usize,
    ) -> anyhow::Result<(DelayedTransactionFetchingResult<Self::Transaction>, usize)>
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
    ) -> anyhow::Result<
        Option<BlockInProgress<Self::Transaction, Self::TransactionReceipt>>,
    > {
        crate::storage::read_block_in_progress(host)
    }

    fn compute_bip<Host: Runtime>(
        &self,
        host: &mut Host,
        outbox_queue: &OutboxQueue<'_, impl Path>,
        block_in_progress: BlockInProgress<Self::Transaction, Self::TransactionReceipt>,
        tick_counter: &mut TickCounter,
        sequencer_pool_address: Option<H160>,
        _maximum_allowed_ticks: u64,
        tracer_input: Option<TracerInput>,
        da_fee_per_byte: U256,
        coinbase: H160,
    ) -> anyhow::Result<BlockComputationResult> {
        log!(host, Debug, "Computing the BlockInProgress for Etherlink");

        crate::block::compute_bip(
            host,
            outbox_queue,
            block_in_progress,
            tick_counter,
            sequencer_pool_address,
            &self.limits,
            tracer_input,
            self.chain_id,
            da_fee_per_byte,
            coinbase,
            &self.spec_id,
            self.enable_tezos_runtime(),
        )
    }

    fn start_simulation_mode(&self, host: &mut impl Runtime) -> anyhow::Result<()> {
        start_simulation_mode(host, &self.spec_id)
    }

    fn storage_root_path(&self) -> RefPath {
        ETHERLINK_SAFE_STORAGE_ROOT_PATH
    }
}

impl EvmChainConfig {
    pub fn create_config(
        chain_id: U256,
        limits: EvmLimits,
        spec_id: SpecId,
        experimental_features: ExperimentalFeatures,
    ) -> Self {
        Self {
            chain_id,
            limits,
            spec_id,
            experimental_features,
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

impl ChainConfigTrait for MichelsonChainConfig {
    type Transaction = TezlinkOperation;
    type TransactionReceipt = AppliedOperation;
    type ChainHeader = TezBlockHeader;

    fn get_chain_id(&self) -> U256 {
        self.chain_id.as_ref().into()
    }

    fn get_chain_family(&self) -> ChainFamily {
        ChainFamily::Michelson
    }

    fn base_fee_per_gas(&self, _host: &impl Runtime, _timestamp: Timestamp) -> U256 {
        U256::zero()
    }

    fn fetch_hashes_from_delayed_inbox(
        host: &mut impl Runtime,
        delayed_hashes: Vec<crate::delayed_inbox::Hash>,
        delayed_inbox: &mut DelayedInbox,
        current_blueprint_size: usize,
    ) -> anyhow::Result<(DelayedTransactionFetchingResult<Self::Transaction>, usize)>
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
                        if let TransactionContent::Deposit(deposit) = tx.content {
                            let operation = TezlinkOperation {
                                tx_hash: tx.tx_hash,
                                content: TezlinkContent::Deposit(deposit),
                            };
                            ops.push(operation)
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

    fn transactions_from_bytes(
        bytes: Vec<Vec<u8>>,
    ) -> anyhow::Result<Vec<Self::Transaction>> {
        let operations = bytes
            .iter()
            .map(|bytes| {
                let operation =
                    Operation::nom_read_exact(bytes).map_err(|decode_error| {
                        error::Error::NomReadError(format!("{decode_error:?}"))
                    })?;
                let tx_hash = operation.hash()?.0 .0;
                Ok(TezlinkOperation {
                    tx_hash,
                    content: TezlinkContent::Tezos(operation),
                })
            })
            .collect::<Result<Vec<TezlinkOperation>, error::Error>>()?;
        Ok(operations)
    }

    fn read_block_in_progress(
        _host: &impl Runtime,
    ) -> anyhow::Result<
        Option<BlockInProgress<Self::Transaction, Self::TransactionReceipt>>,
    > {
        Ok(None)
    }

    fn compute_bip<Host: Runtime>(
        &self,
        host: &mut Host,
        _outbox_queue: &OutboxQueue<'_, impl Path>,
        mut block_in_progress: BlockInProgress<
            Self::Transaction,
            Self::TransactionReceipt,
        >,
        _tick_counter: &mut TickCounter,
        _sequencer_pool_address: Option<H160>,
        _maximum_allowed_ticks: u64,
        _tracer_input: Option<TracerInput>,
        _da_fee_per_byte: U256,
        _coinbase: H160,
    ) -> anyhow::Result<BlockComputationResult> {
        log!(
            host,
            Debug,
            "Computing the BlockInProgress for Tezlink at level {}",
            block_in_progress.number
        );

        let context = context::TezlinkContext::from_root(&self.storage_root_path())?;

        let level = block_in_progress.number.try_into()?;
        let now = block_in_progress.timestamp;
        let block_ctx = BlockCtx {
            level: &level,
            now: &now,
            chain_id: &self.chain_id,
        };

        let mut included_delayed_transactions = vec![];
        // Compute operations that are in the block in progress
        while block_in_progress.has_tx() {
            let operation = block_in_progress.pop_tx().ok_or(error::Error::Reboot)?;

            match operation.content {
                TezlinkContent::Tezos(operation) => {
                    // Compute the hash of the operation
                    let hash = operation.hash()?;

                    let skip_signature_check = false;

                    let branch = operation.branch.clone();
                    let signature = operation.signature.clone();

                    // Try to apply the operation with the tezos_execution crate, return a receipt
                    // on whether it failed or not
                    let processed_operations =
                        match tezos_execution::validate_and_apply_operation(
                            host,
                            &context,
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
                                continue;
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
                    block_in_progress
                        .cumulative_receipts
                        .push(applied_operation);
                }
                TezlinkContent::Deposit(deposit) => {
                    log!(host, Debug, "Execute Tezlink deposit: {deposit:?}");

                    let deposit_result =
                        execute_tezlink_deposit(host, &context, &deposit)?;

                    let source =
                        PublicKeyHash::nom_read_exact(&TEZLINK_DEPOSITOR[1..]).unwrap();

                    let applied_operation = AppliedOperation {
                        hash: H256::from_slice(&operation.tx_hash).into(),
                        branch: block_in_progress.parent_hash.into(),
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
                                    receipt: OperationResultSum::Transfer(
                                        OperationResult {
                                            balance_updates: vec![],
                                            result: deposit_result.outcome.0,
                                            internal_operation_results: vec![],
                                        },
                                    ),
                                }],
                                signature: UnknownSignature::nom_read_exact(&[0u8; 64])
                                    .unwrap(),
                            },
                        ),
                    };
                    block_in_progress
                        .cumulative_receipts
                        .push(applied_operation);
                    included_delayed_transactions.push(operation.tx_hash);
                }
            };
        }

        // Create a Tezos block from the block in progress
        let tezblock = TezBlock::new(
            level,
            block_in_progress.timestamp,
            block_in_progress.parent_hash,
            block_in_progress.cumulative_receipts,
        )?;
        let new_block = L2Block::Tezlink(tezblock);
        let root = self.storage_root_path();
        crate::block_storage::store_current(host, &root, &new_block)
            .context("Failed to store the current block")?;
        Ok(BlockComputationResult::Finished {
            included_delayed_transactions,
            block: new_block,
        })
    }

    fn start_simulation_mode(&self, host: &mut impl Runtime) -> anyhow::Result<()> {
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
        let context = context::TezlinkContext::from_root(&self.storage_root_path())?;

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

    fn storage_root_path(&self) -> RefPath {
        TEZLINK_SAFE_STORAGE_ROOT_PATH
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
    ) -> Self {
        ChainConfig::Evm(Box::new(EvmChainConfig::create_config(
            chain_id,
            limits,
            spec_id,
            experimental_features,
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
    )
}

#[cfg(test)]
pub fn test_chain_config() -> ChainConfig {
    ChainConfig::Evm(Box::new(test_evm_chain_config()))
}

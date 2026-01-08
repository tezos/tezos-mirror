// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    block::{eth_bip_from_blueprint, BlockComputationResult, TickCounter},
    block_in_progress::EthBlockInProgress,
    blueprint::Blueprint,
    blueprint_storage::{
        DelayedTransactionFetchingResult, EVMBlockHeader, TezBlockHeader,
    },
    delayed_inbox::DelayedInbox,
    fees::MINIMUM_BASE_FEE_PER_GAS,
    l2block::L2Block,
    simulation::start_simulation_mode,
    tick_model::constants::MAXIMUM_GAS_LIMIT,
    transaction::Transactions::EthTxs,
    CHAIN_ID,
};
use anyhow::Context;
use evm_execution::{
    configuration::EVMVersion,
    precompiles::{precompile_set, PrecompileBTreeMap},
    trace::TracerInput,
};
use primitive_types::{H160, H256, U256};
use rlp::{Decodable, Encodable};
use std::fmt::{Debug, Display};
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::{outbox::OutboxQueue, types::Timestamp};
use tezos_smart_rollup_host::path::Path;
use tezos_tezlink::block::TezBlock;

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

#[derive(Debug)]
pub struct EvmChainConfig {
    chain_id: U256,
    limits: EvmLimits,
    evm_config: evm_execution::Config,
}

#[derive(Debug)]
pub struct MichelsonChainConfig {
    chain_id: U256,
}

pub trait BlockInProgressTrait {
    fn number(&self) -> U256;
}

impl BlockInProgressTrait for EthBlockInProgress {
    fn number(&self) -> U256 {
        self.number
    }
}

pub struct TezBlockInProgress {
    number: U256,
    timestamp: Timestamp,
    previous_hash: H256,
}

impl BlockInProgressTrait for TezBlockInProgress {
    fn number(&self) -> U256 {
        self.number
    }
}

pub enum ChainConfig {
    Evm(Box<EvmChainConfig>),
    Michelson(MichelsonChainConfig),
}

pub trait TransactionsTrait {
    fn extend(&mut self, other: Self);
    fn number_of_txs(&self) -> usize;
}

impl TransactionsTrait for crate::transaction::Transactions {
    fn extend(&mut self, other: Self) {
        let EthTxs(ref mut txs) = self;
        let EthTxs(other) = other;
        txs.extend(other)
    }
    fn number_of_txs(&self) -> usize {
        let EthTxs(txs) = self;
        txs.len()
    }
}

#[derive(Debug)]
pub struct TezTransactions {}

impl TransactionsTrait for TezTransactions {
    fn extend(&mut self, _: Self) {}

    fn number_of_txs(&self) -> usize {
        0
    }
}

impl Encodable for TezTransactions {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        let Self {} = self;
        stream.begin_list(0);
    }
}

impl Decodable for TezTransactions {
    fn decode(_decoder: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        Ok(Self {})
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
    type Transactions: TransactionsTrait + Encodable + Decodable + Debug;

    type BlockInProgress: BlockInProgressTrait;

    type ChainHeader: ChainHeaderTrait + Decodable;

    fn get_chain_id(&self) -> U256;

    fn get_chain_family(&self) -> ChainFamily;

    fn fmt_with_family(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let chain_family = self.get_chain_family();
        write!(f, "{{Chain family: {}, {:?}}}", chain_family, self)
    }

    fn fetch_hashes_from_delayed_inbox(
        host: &mut impl Runtime,
        delayed_hashes: Vec<crate::delayed_inbox::Hash>,
        delayed_inbox: &mut DelayedInbox,
        current_blueprint_size: usize,
    ) -> anyhow::Result<(DelayedTransactionFetchingResult<Self::Transactions>, usize)>;

    fn transactions_from_bytes(bytes: Vec<Vec<u8>>)
        -> anyhow::Result<Self::Transactions>;

    fn block_in_progress_from_blueprint(
        &self,
        host: &impl Runtime,
        tick_counter: &crate::block::TickCounter,
        current_block_number: U256,
        previous_chain_header: Self::ChainHeader,
        blueprint: Blueprint<Self::Transactions>,
    ) -> Self::BlockInProgress;

    fn read_block_in_progress(
        host: &impl Runtime,
    ) -> anyhow::Result<Option<Self::BlockInProgress>>;

    #[allow(clippy::too_many_arguments)]
    fn compute_bip<Host: Runtime>(
        &self,
        host: &mut Host,
        outbox_queue: &OutboxQueue<'_, impl Path>,
        block_in_progress: Self::BlockInProgress,
        precompiles: &PrecompileBTreeMap<Host>,
        tick_counter: &mut TickCounter,
        sequencer_pool_address: Option<H160>,
        maximum_allowed_ticks: u64,
        tracer_input: Option<TracerInput>,
        da_fee_per_byte: U256,
        coinbase: H160,
    ) -> anyhow::Result<BlockComputationResult>;

    fn start_simulation_mode(
        &self,
        host: &mut impl Runtime,
        enable_fa_bridge: bool,
    ) -> anyhow::Result<()>;

    fn precompiles_set<Host: Runtime>(
        &self,
        enable_fa_bridge: bool,
    ) -> PrecompileBTreeMap<Host>;
}

impl ChainConfigTrait for EvmChainConfig {
    type Transactions = crate::transaction::Transactions;

    type BlockInProgress = crate::block_in_progress::EthBlockInProgress;

    type ChainHeader = crate::blueprint_storage::EVMBlockHeader;

    fn get_chain_id(&self) -> U256 {
        self.chain_id
    }

    fn get_chain_family(&self) -> ChainFamily {
        ChainFamily::Evm
    }

    fn block_in_progress_from_blueprint(
        &self,
        host: &impl Runtime,
        tick_counter: &crate::block::TickCounter,
        current_block_number: U256,
        header: Self::ChainHeader,
        blueprint: Blueprint<Self::Transactions>,
    ) -> Self::BlockInProgress {
        eth_bip_from_blueprint(
            host,
            self,
            tick_counter,
            current_block_number,
            header,
            blueprint,
        )
    }

    fn transactions_from_bytes(
        bytes: Vec<Vec<u8>>,
    ) -> anyhow::Result<Self::Transactions> {
        Ok(EthTxs(crate::blueprint_storage::transactions_from_bytes(
            bytes,
        )?))
    }

    fn fetch_hashes_from_delayed_inbox(
        host: &mut impl Runtime,
        delayed_hashes: Vec<crate::delayed_inbox::Hash>,
        delayed_inbox: &mut DelayedInbox,
        current_blueprint_size: usize,
    ) -> anyhow::Result<(DelayedTransactionFetchingResult<Self::Transactions>, usize)>
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
    ) -> anyhow::Result<Option<Self::BlockInProgress>> {
        crate::storage::read_block_in_progress(host)
    }

    fn compute_bip<Host: Runtime>(
        &self,
        host: &mut Host,
        outbox_queue: &OutboxQueue<'_, impl Path>,
        block_in_progress: Self::BlockInProgress,
        precompiles: &PrecompileBTreeMap<Host>,
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
            precompiles,
            tick_counter,
            sequencer_pool_address,
            &self.limits,
            tracer_input,
            self.chain_id,
            da_fee_per_byte,
            coinbase,
            &self.evm_config,
        )
    }

    fn start_simulation_mode(
        &self,
        host: &mut impl Runtime,
        enable_fa_bridge: bool,
    ) -> anyhow::Result<()> {
        start_simulation_mode(host, enable_fa_bridge, &self.evm_config)
    }

    fn precompiles_set<Host: Runtime>(
        &self,
        enable_fa_bridge: bool,
    ) -> PrecompileBTreeMap<Host> {
        precompile_set::<Host>(enable_fa_bridge)
    }
}

impl EvmChainConfig {
    pub fn create_config(
        chain_id: U256,
        limits: EvmLimits,
        evm_config: evm_execution::Config,
    ) -> Self {
        Self {
            chain_id,
            limits,
            evm_config,
        }
    }

    pub fn get_limits(&self) -> &EvmLimits {
        &self.limits
    }

    pub fn limits_mut(&mut self) -> &mut EvmLimits {
        &mut self.limits
    }

    pub fn get_evm_config(&self) -> &evm_execution::Config {
        &self.evm_config
    }
}

impl ChainConfigTrait for MichelsonChainConfig {
    type Transactions = TezTransactions;
    type BlockInProgress = TezBlockInProgress;
    type ChainHeader = TezBlockHeader;

    fn get_chain_id(&self) -> U256 {
        self.chain_id
    }

    fn get_chain_family(&self) -> ChainFamily {
        ChainFamily::Michelson
    }

    fn block_in_progress_from_blueprint(
        &self,
        _host: &impl Runtime,
        _tick_counter: &crate::block::TickCounter,
        current_block_number: U256,
        header: Self::ChainHeader,
        blueprint: Blueprint<Self::Transactions>,
    ) -> Self::BlockInProgress {
        TezBlockInProgress {
            number: current_block_number,
            timestamp: blueprint.timestamp,
            previous_hash: header.hash,
        }
    }

    fn fetch_hashes_from_delayed_inbox(
        _host: &mut impl Runtime,
        _delayed_hashes: Vec<crate::delayed_inbox::Hash>,
        _delayed_inbox: &mut DelayedInbox,
        current_blueprint_size: usize,
    ) -> anyhow::Result<(DelayedTransactionFetchingResult<Self::Transactions>, usize)>
    {
        Ok((
            DelayedTransactionFetchingResult::Ok(TezTransactions {}),
            current_blueprint_size,
        ))
    }

    fn transactions_from_bytes(
        _bytes: Vec<Vec<u8>>,
    ) -> anyhow::Result<Self::Transactions> {
        Ok(TezTransactions {})
    }

    fn read_block_in_progress(
        _host: &impl Runtime,
    ) -> anyhow::Result<Option<Self::BlockInProgress>> {
        Ok(None)
    }

    fn compute_bip<Host: Runtime>(
        &self,
        host: &mut Host,
        _outbox_queue: &OutboxQueue<'_, impl Path>,
        block_in_progress: Self::BlockInProgress,
        _precompiles: &PrecompileBTreeMap<Host>,
        _tick_counter: &mut TickCounter,
        _sequencer_pool_address: Option<H160>,
        _maximum_allowed_ticks: u64,
        _tracer_input: Option<TracerInput>,
        _da_fee_per_byte: U256,
        _coinbase: H160,
    ) -> anyhow::Result<BlockComputationResult> {
        let TezBlockInProgress {
            number,
            timestamp,
            previous_hash,
        } = block_in_progress;
        log!(
            host,
            Debug,
            "Computing the BlockInProgress for Tezlink at level {}",
            number
        );

        let tezblock = TezBlock::new(number, timestamp, previous_hash);
        let new_block = L2Block::Tezlink(tezblock);
        crate::block_storage::store_current(host, &new_block)
            .context("Failed to store the current block")?;
        Ok(BlockComputationResult::Finished {
            included_delayed_transactions: vec![],
            block: new_block,
        })
    }

    fn start_simulation_mode(
        &self,
        _host: &mut impl Runtime,
        _enable_fa_bridge: bool,
    ) -> anyhow::Result<()> {
        Ok(())
    }

    fn precompiles_set<Host: Runtime>(
        &self,
        _enable_fa_bridge: bool,
    ) -> PrecompileBTreeMap<Host> {
        PrecompileBTreeMap::new()
    }
}

impl MichelsonChainConfig {
    pub fn create_config(chain_id: U256) -> Self {
        Self { chain_id }
    }
}

impl ChainConfig {
    pub fn new_evm_config(
        chain_id: U256,
        limits: EvmLimits,
        evm_config: evm_execution::Config,
    ) -> Self {
        ChainConfig::Evm(Box::new(EvmChainConfig::create_config(
            chain_id, limits, evm_config,
        )))
    }

    pub fn new_michelson_config(chain_id: U256) -> Self {
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
            EVMVersion::to_config(&EVMVersion::default()),
        )
    }
}

#[cfg(test)]
pub fn test_evm_chain_config() -> EvmChainConfig {
    EvmChainConfig::create_config(
        U256::from(CHAIN_ID),
        EvmLimits::default(),
        EVMVersion::current_test_config(),
    )
}

#[cfg(test)]
pub fn test_chain_config() -> ChainConfig {
    ChainConfig::Evm(Box::new(test_evm_chain_config()))
}

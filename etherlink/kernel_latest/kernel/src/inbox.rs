// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use crate::blueprint_storage::store_sequencer_blueprint;
use crate::bridge::Deposit;
use crate::chains::{ChainConfigTrait, EvmChainConfig, ExperimentalFeatures};
use crate::configuration::{DalConfiguration, TezosContracts};
use crate::dal::fetch_and_parse_sequencer_blueprint_from_dal;
use crate::dal_slot_import_signal::DalSlotImportSignals;
use crate::delayed_inbox::DelayedInbox;
use crate::parsing::{
    Input, InputResult, Parsable, ProxyInput, SequencerBlueprintRes::*, SequencerInput,
    SequencerParsingContext, MAX_SIZE_PER_CHUNK,
};

use crate::registry_impl::RegistryImpl;
use crate::sequencer_blueprint::UnsignedSequencerBlueprint;
use crate::storage::{
    chunked_transaction_hash_exists, chunked_transaction_num_chunks,
    chunked_transaction_path, clear_events, create_chunked_transaction, read_l1_level,
    read_last_info_per_level_timestamp, remove_chunked_transaction, remove_sequencer,
    store_l1_level, store_last_info_per_level_timestamp, store_transaction_chunk,
};
use crate::tick_model::constants::TICKS_FOR_BLUEPRINT_INTERCEPT;
use crate::tick_model::maximum_ticks_for_sequencer_chunk;
use crate::transaction::{Transaction, TransactionContent};
use crate::upgrade;
use crate::upgrade::*;
use crate::Error;
use primitive_types::U256;
use revm_etherlink::helpers::legacy::FaDeposit;
use sha3::{Digest, Keccak256};
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_ethereum::transaction::{TransactionHash, TRANSACTION_HASH_SIZE};
use tezos_ethereum::tx_common::EthereumTransactionCommon;
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_encoding::public_key::PublicKey;

#[derive(Debug, PartialEq)]
pub struct ProxyInboxContent {
    pub transactions: Vec<Transaction>,
}

pub fn read_input<Host: Runtime, Mode: Parsable>(
    host: &mut Host,
    smart_rollup_address: [u8; 20],
    tezos_contracts: &TezosContracts,
    inbox_is_empty: &mut bool,
    parsing_context: &mut Mode::Context,
    enable_fa_deposits: bool,
) -> Result<InputResult<Mode>, Error> {
    let input = host.read_input()?;

    match input {
        Some(input) => {
            *inbox_is_empty = false;
            Ok(InputResult::parse(
                host,
                input,
                smart_rollup_address,
                tezos_contracts,
                parsing_context,
                enable_fa_deposits,
            ))
        }
        None => Ok(InputResult::NoInput),
    }
}

/// The InputHandler abstracts how the input is handled once it has been parsed.
pub trait InputHandler
where
    Self: Parsable + std::fmt::Debug,
{
    /// Abstracts the type used to store the inputs once handled
    type Inbox;

    fn handle_input<Host: Runtime>(
        host: &mut Host,
        input: Self,
        inbox_content: &mut Self::Inbox,
    ) -> anyhow::Result<()>;

    fn handle_deposit<Host: Runtime>(
        host: &mut Host,
        deposit: Deposit,
        chain_id: Option<U256>,
        inbox_content: &mut Self::Inbox,
    ) -> anyhow::Result<()>;

    fn handle_fa_deposit<Host: Runtime>(
        host: &mut Host,
        fa_deposit: FaDeposit,
        chain_id: Option<U256>,
        inbox_content: &mut Self::Inbox,
    ) -> anyhow::Result<()>;
}

impl InputHandler for ProxyInput {
    // In case of the proxy, the Inbox is unchanged: we keep the InboxContent as
    // everything is doable in a single kernel_run.
    type Inbox = ProxyInboxContent;

    fn handle_input<Host: Runtime>(
        host: &mut Host,
        input: Self,
        inbox_content: &mut Self::Inbox,
    ) -> anyhow::Result<()> {
        match input {
            Self::SimpleTransaction(tx) => inbox_content.transactions.push(*tx),
            Self::NewChunkedTransaction {
                tx_hash,
                num_chunks,
                chunk_hashes,
            } => create_chunked_transaction(host, &tx_hash, num_chunks, chunk_hashes)?,
            Self::TransactionChunk {
                tx_hash,
                i,
                chunk_hash,
                data,
            } => {
                if let Some(tx) =
                    handle_transaction_chunk(host, tx_hash, i, chunk_hash, data)?
                {
                    inbox_content.transactions.push(tx)
                }
            }
        }
        Ok(())
    }

    fn handle_deposit<Host: Runtime>(
        host: &mut Host,
        deposit: Deposit,
        _chain_id: Option<U256>,
        inbox_content: &mut Self::Inbox,
    ) -> anyhow::Result<()> {
        inbox_content
            .transactions
            .push(handle_deposit(host, deposit)?);
        Ok(())
    }

    #[cfg_attr(feature = "benchmark", inline(never))]
    fn handle_fa_deposit<Host: Runtime>(
        host: &mut Host,
        fa_deposit: FaDeposit,
        _chain_id: Option<U256>,
        inbox_content: &mut Self::Inbox,
    ) -> anyhow::Result<()> {
        inbox_content
            .transactions
            .push(handle_fa_deposit(host, fa_deposit)?);
        Ok(())
    }
}

fn handle_blueprint_chunk<Host: Runtime>(
    host: &mut Host,
    blueprint: UnsignedSequencerBlueprint,
) -> anyhow::Result<()> {
    log!(host, Benchmarking, "Handling a blueprint input");
    log!(
        host,
        Debug,
        "Storing chunk {} of sequencer blueprint number {}",
        blueprint.chunk_index,
        blueprint.number
    );
    store_sequencer_blueprint(host, blueprint).map_err(Error::into)
}

impl InputHandler for SequencerInput {
    // For the sequencer, inputs are stored directly in the storage. The delayed
    // inbox represents part of the storage, but `Unit` would also be enough as
    // there is nothing to return in the end.
    type Inbox = DelayedInbox;

    fn handle_input<Host: Runtime>(
        host: &mut Host,
        input: Self,
        delayed_inbox: &mut Self::Inbox,
    ) -> anyhow::Result<()> {
        log!(host, Debug, "Handling input in sequencer mode: {:?}", input);
        match input {
            Self::DelayedInput(tx) => {
                let previous_timestamp = read_last_info_per_level_timestamp(host)?;
                let level = read_l1_level(host)?;
                log!(host, Benchmarking, "Handling a delayed input");
                delayed_inbox.save_transaction(host, *tx, previous_timestamp, level)
            }
            Self::SequencerBlueprint(SequencerBlueprint(seq_blueprint)) => {
                handle_blueprint_chunk(host, seq_blueprint)
            }
            Self::SequencerBlueprint(
                InvalidNumberOfChunks | InvalidSignature | InvalidNumber | Unparsable,
            ) => {
                log!(
                    host,
                    Debug,
                    "Sequencer blueprint refused because: {:?}",
                    input
                );
                Ok(())
            }
            Self::DalSlotImportSignals(DalSlotImportSignals {
                signals,
                signature: _,
            }) => {
                log!(host, Debug, "Importing {} DAL signals", &signals.0.len());
                let params = host.reveal_dal_parameters();
                let next_blueprint_number: U256 =
                    crate::blueprint_storage::read_next_blueprint_number(host)?;
                for signal in signals.0.iter() {
                    let published_level = signal.published_level;
                    let slot_indices = &signal.slot_indices;
                    for slot_index in slot_indices.0.iter() {
                        log!(
                            host,
                            Debug,
                            "Handling a signal for slot index {} and published_level {}",
                            slot_index,
                            published_level
                        );
                        if let Some(unsigned_seq_blueprints) =
                            fetch_and_parse_sequencer_blueprint_from_dal(
                                host,
                                &params,
                                &next_blueprint_number,
                                *slot_index,
                                published_level,
                            )
                        {
                            log!(
                                host,
                                Debug,
                                "DAL slot is a blueprint of {} chunks",
                                unsigned_seq_blueprints.len()
                            );
                            for chunk in unsigned_seq_blueprints {
                                handle_blueprint_chunk(host, chunk)?
                            }
                        }
                    }
                }
                Ok(())
            }
        }
    }

    fn handle_deposit<Host: Runtime>(
        host: &mut Host,
        deposit: Deposit,
        _chain_id: Option<U256>,
        delayed_inbox: &mut Self::Inbox,
    ) -> anyhow::Result<()> {
        let previous_timestamp = read_last_info_per_level_timestamp(host)?;
        let level = read_l1_level(host)?;
        let tx = handle_deposit(host, deposit)?;
        delayed_inbox.save_transaction(host, tx, previous_timestamp, level)
    }

    #[cfg_attr(feature = "benchmark", inline(never))]
    fn handle_fa_deposit<Host: Runtime>(
        host: &mut Host,
        fa_deposit: FaDeposit,
        _chain_id: Option<U256>,
        delayed_inbox: &mut Self::Inbox,
    ) -> anyhow::Result<()> {
        let previous_timestamp = read_last_info_per_level_timestamp(host)?;
        let level = read_l1_level(host)?;
        let tx = handle_fa_deposit(host, fa_deposit)?;
        delayed_inbox.save_transaction(host, tx, previous_timestamp, level)
    }
}

fn handle_transaction_chunk<Host: Runtime>(
    host: &mut Host,
    tx_hash: TransactionHash,
    i: u16,
    chunk_hash: TransactionHash,
    data: Vec<u8>,
) -> Result<Option<Transaction>, Error> {
    // If the number of chunks doesn't exist in the storage, the chunked
    // transaction wasn't created, so the chunk is ignored.
    let num_chunks = match chunked_transaction_num_chunks(host, &tx_hash) {
        Ok(x) => x,
        Err(_) => {
            log!(
                host,
                Info,
                "Ignoring chunk {} of unknown transaction {}",
                i,
                hex::encode(tx_hash)
            );
            return Ok(None);
        }
    };
    // Checks that the transaction is not out of bounds.
    if i >= num_chunks {
        return Ok(None);
    }
    // Check if the chunk hash is part of the announced chunked hashes.
    let chunked_transaction_path = chunked_transaction_path(&tx_hash)?;
    if !chunked_transaction_hash_exists(host, &chunked_transaction_path, &chunk_hash)? {
        return Ok(None);
    }
    // Sanity check to verify that the transaction chunk uses the maximum
    // space capacity allowed.
    if i != num_chunks - 1 && data.len() < MAX_SIZE_PER_CHUNK {
        remove_chunked_transaction(host, &tx_hash)?;
        return Ok(None);
    };
    // When the transaction is stored in the storage, it returns the full transaction
    // if `data` was the missing chunk.
    if let Some(data) = store_transaction_chunk(host, &tx_hash, i, data)? {
        let full_data_hash: [u8; TRANSACTION_HASH_SIZE] = Keccak256::digest(&data).into();
        if full_data_hash == tx_hash {
            if let Ok(tx) = EthereumTransactionCommon::from_bytes(&data) {
                let content = TransactionContent::Ethereum(tx);
                return Ok(Some(Transaction { tx_hash, content }));
            }
        }
    }
    Ok(None)
}

fn handle_deposit<Host: Runtime>(
    host: &mut Host,
    deposit: Deposit,
) -> Result<Transaction, Error> {
    let seed = host.reveal_metadata().raw_rollup_address;
    let tx_hash = deposit.hash(&seed).into();
    Ok(Transaction {
        tx_hash,
        content: TransactionContent::Deposit(deposit),
    })
}

#[cfg_attr(feature = "benchmark", inline(never))]
fn handle_fa_deposit<Host: Runtime>(
    host: &mut Host,
    fa_deposit: FaDeposit,
) -> Result<Transaction, Error> {
    let seed = host.reveal_metadata().raw_rollup_address;
    let tx_hash = fa_deposit.hash(&seed).into();
    Ok(Transaction {
        tx_hash,
        content: TransactionContent::FaDeposit(fa_deposit),
    })
}

fn force_kernel_upgrade(host: &mut impl Runtime) -> anyhow::Result<()> {
    match upgrade::read_kernel_upgrade(host)? {
        Some(kernel_upgrade) => {
            let current_timestamp = read_last_info_per_level_timestamp(host)?.i64();
            let activation_timestamp = kernel_upgrade.activation_timestamp.i64();

            if current_timestamp >= (activation_timestamp + 86400i64) {
                // If the kernel upgrade still exist 1 day after it was supposed
                // to be activated. It is possible to force its execution.
                upgrade::upgrade(host, kernel_upgrade.preimage_hash)?
            };
            Ok(())
        }
        None => Ok(()),
    }
}

pub fn handle_input<Mode: Parsable + InputHandler>(
    host: &mut impl Runtime,
    input: Input<Mode>,
    inbox_content: &mut Mode::Inbox,
) -> anyhow::Result<()> {
    match input {
        Input::ModeSpecific(input) => Mode::handle_input(host, input, inbox_content)?,
        Input::Upgrade(kernel_upgrade) => store_kernel_upgrade(host, &kernel_upgrade)?,
        Input::SequencerUpgrade(sequencer_upgrade) => {
            store_sequencer_upgrade(host, sequencer_upgrade)?
        }
        Input::RemoveSequencer => remove_sequencer(host)?,
        Input::Info(info) => {
            // New inbox level detected, remove all previous events.
            clear_events(host)?;
            store_last_info_per_level_timestamp(host, info.info.predecessor_timestamp)?;
            store_l1_level(host, info.level)?
        }
        Input::Deposit((deposit, chain_id)) => {
            Mode::handle_deposit(host, deposit, chain_id, inbox_content)?
        }
        Input::FaDeposit((fa_deposit, chain_id)) => {
            Mode::handle_fa_deposit(host, fa_deposit, chain_id, inbox_content)?
        }
        Input::ForceKernelUpgrade => force_kernel_upgrade(host)?,
    }
    Ok(())
}

enum ReadStatus {
    FinishedIgnore,
    FinishedRead,
    Ongoing,
}

#[allow(clippy::too_many_arguments)]
fn read_and_dispatch_input<
    Host: Runtime,
    Mode: Parsable + InputHandler,
    ChainConfig: ChainConfigTrait,
>(
    host: &mut Host,
    smart_rollup_address: [u8; 20],
    tezos_contracts: &TezosContracts,
    parsing_context: &mut Mode::Context,
    inbox_is_empty: &mut bool,
    res: &mut Mode::Inbox,
    enable_fa_bridge: bool,
    chain_configuration: &ChainConfig,
) -> anyhow::Result<ReadStatus> {
    let input: InputResult<Mode> = read_input(
        host,
        smart_rollup_address,
        tezos_contracts,
        inbox_is_empty,
        parsing_context,
        enable_fa_bridge,
    )?;
    match input {
        InputResult::NoInput => {
            if *inbox_is_empty {
                // If `inbox_is_empty` is true, that means we haven't see
                // any input in the current call of `read_inbox`. Therefore,
                // the inbox of this level has already been consumed.
                log!(host, Debug, "Inbox is empty, moving to stage 2.");
                Ok(ReadStatus::FinishedIgnore)
            } else {
                // If it's a `NoInput` and `inbox_is_empty` is false, we
                // have simply reached the end of the inbox.
                log!(host, Debug, "Reaching end of inbox.");
                Ok(ReadStatus::FinishedRead)
            }
        }
        InputResult::Unparsable => Ok(ReadStatus::Ongoing),
        InputResult::Simulation => {
            // kernel enters in simulation mode, reading will be done by the
            // simulation and all the previous and next transactions are
            // discarded.
            let registry = RegistryImpl::new(chain_configuration.get_chain_id());
            chain_configuration.start_simulation_mode(host, &registry)?;
            Ok(ReadStatus::FinishedIgnore)
        }
        InputResult::Input(input) => {
            handle_input(host, input, res)?;
            Ok(ReadStatus::Ongoing)
        }
    }
}

pub fn read_proxy_inbox<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; 20],
    tezos_contracts: &TezosContracts,
    enable_fa_bridge: bool,
    chain_configuration: &EvmChainConfig,
) -> Result<Option<ProxyInboxContent>, anyhow::Error> {
    let mut res = ProxyInboxContent {
        transactions: vec![],
    };
    // The mutable variable is used to retrieve the information of whether the
    // inbox was empty or not. As we consume all the inbox in one go, if the
    // variable remains true, that means that the inbox was already consumed
    // during this kernel run.
    let mut inbox_is_empty = true;
    loop {
        match read_and_dispatch_input::<Host, ProxyInput, EvmChainConfig>(
            host,
            smart_rollup_address,
            tezos_contracts,
            &mut (),
            &mut inbox_is_empty,
            &mut res,
            enable_fa_bridge,
            chain_configuration,
        ) {
            Err(err) =>
            // If we failed to read or dispatch the input.
            // We allow ourselves to continue with the inbox consumption.
            // In order to make sure we can retrieve any kernel upgrade
            // present in the inbox.
            {
                log!(
                    host,
                    Fatal,
                    "An input made `read_and_dispatch_input` fail, we ignore it ({:?})",
                    err
                )
            }
            Ok(ReadStatus::Ongoing) => (),
            Ok(ReadStatus::FinishedRead) => return Ok(Some(res)),
            Ok(ReadStatus::FinishedIgnore) => return Ok(None),
        }
    }
}

/// The StageOne can yield with three possible states:
///
/// - Done: the inbox has been fully read during the current `kernel_run`
///
/// - Reboot: the inbox cannot been read further as there are not enough ticks
///   and needs a reboot before continuing. This is only supported in sequencer
///   mode as the inputs are stored directly in the process.
///
/// - Skipped: the inbox was empty during the current `kernel_run`, implying it
///   has been emptied during a previous `kernel_run` and the kernel is
///   currently processing blueprints. It prevents the automatic reboot after
///   completing the stage one to start the block production, and producing an
///   empty blueprint in proxy mode.
pub enum StageOneStatus {
    Done,
    Reboot,
    Skipped,
}

#[allow(clippy::too_many_arguments)]
pub fn read_sequencer_inbox<Host: Runtime, ChainConfig: ChainConfigTrait>(
    host: &mut Host,
    smart_rollup_address: [u8; 20],
    tezos_contracts: &TezosContracts,
    delayed_bridge: ContractKt1Hash,
    sequencer: PublicKey,
    delayed_inbox: &mut DelayedInbox,
    enable_fa_bridge: bool,
    maximum_allowed_ticks: u64,
    dal: Option<DalConfiguration>,
    chain_configuration: &ChainConfig,
) -> Result<StageOneStatus, anyhow::Error> {
    // The mutable variable is used to retrieve the information of whether the
    // inbox was empty or not. As we consume all the inbox in one go, if the
    // variable remains true, that means that the inbox was already consumed
    // during this kernel run.
    let mut inbox_is_empty = true;
    let next_blueprint_number: U256 =
        crate::blueprint_storage::read_next_blueprint_number(host)?;
    let experimental_features = ExperimentalFeatures::read_from_storage(host);
    let mut parsing_context = SequencerParsingContext {
        sequencer,
        delayed_bridge,
        allocated_ticks: maximum_allowed_ticks
            .saturating_sub(TICKS_FOR_BLUEPRINT_INTERCEPT),
        dal_configuration: dal,
        buffer_transaction_chunks: None,
        next_blueprint_number,
        experimental_features,
    };
    loop {
        // Checks there will be enough ticks to handle at least another chunk of
        // full size. If it is not the case, asks for reboot.
        if parsing_context.allocated_ticks < maximum_ticks_for_sequencer_chunk() {
            log!(
                host,
                Benchmarking,
                "Estimated ticks: {}",
                maximum_allowed_ticks.saturating_sub(parsing_context.allocated_ticks)
            );
            return Ok(StageOneStatus::Reboot);
        };
        match read_and_dispatch_input::<Host, SequencerInput, ChainConfig>(
            host,
            smart_rollup_address,
            tezos_contracts,
            &mut parsing_context,
            &mut inbox_is_empty,
            delayed_inbox,
            enable_fa_bridge,
            chain_configuration,
        ) {
            Err(err) =>
            // If we failed to read or dispatch the input.
            // We allow ourselves to continue with the inbox consumption.
            // In order to make sure we can retrieve any kernel upgrade
            // present in the inbox.
            {
                log!(
                    host,
                    Fatal,
                    "An input made `read_and_dispatch_input` fail, we ignore it ({:?})",
                    err
                )
            }
            Ok(ReadStatus::Ongoing) => (),
            Ok(ReadStatus::FinishedRead) => {
                log!(
                    host,
                    Benchmarking,
                    "Estimated ticks: {}",
                    maximum_allowed_ticks.saturating_sub(parsing_context.allocated_ticks)
                );
                return Ok(StageOneStatus::Done);
            }
            Ok(ReadStatus::FinishedIgnore) => return Ok(StageOneStatus::Skipped),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::blueprint_storage::{
        blueprint_path, store_current_block_header, BlockHeader, BlueprintHeader,
        ChainHeader, EVMBlockHeader,
    };
    use crate::chains::test_evm_chain_config;
    use crate::configuration::TezosContracts;
    use crate::dal_slot_import_signal::{
        DalSlotIndicesList, DalSlotIndicesOfLevel, UnsignedDalSlotSignals,
    };
    use crate::parsing::RollupType;
    use crate::storage::*;
    use crate::tick_model::constants::MAX_ALLOWED_TICKS;
    use crate::transaction::TransactionContent::Ethereum;
    use primitive_types::U256;
    use rlp::Encodable;
    use std::fmt::Write;
    use tezos_crypto_rs::hash::SmartRollupHash;
    use tezos_crypto_rs::hash::UnknownSignature;
    use tezos_crypto_rs::hash::{HashTrait, SecretKeyEd25519};
    use tezos_data_encoding::types::Bytes;
    use tezos_ethereum::transaction::TRANSACTION_HASH_SIZE;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_protocol::contract::Contract;
    use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
    use tezos_smart_rollup_encoding::inbox::ExternalMessageFrame;
    use tezos_smart_rollup_encoding::michelson::{MichelsonBytes, MichelsonOr};
    use tezos_smart_rollup_encoding::public_key_hash::PublicKeyHash;
    use tezos_smart_rollup_encoding::smart_rollup::SmartRollupAddress;
    use tezos_smart_rollup_encoding::timestamp::Timestamp;
    use tezos_smart_rollup_host::storage::StorageV1;
    use tezos_smart_rollup_mock::TransferMetadata;

    const SMART_ROLLUP_ADDRESS: [u8; 20] = [
        20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1,
    ];

    const ZERO_TX_HASH: TransactionHash = [0; TRANSACTION_HASH_SIZE];

    fn smart_rollup_address() -> SmartRollupAddress {
        SmartRollupAddress::new(
            SmartRollupHash::try_from_bytes(&SMART_ROLLUP_ADDRESS).unwrap(),
        )
    }

    fn input_to_bytes(
        smart_rollup_address: [u8; 20],
        input: Input<ProxyInput>,
    ) -> Vec<u8> {
        let mut buffer = Vec::new();
        // Targetted framing protocol
        buffer.push(0);
        buffer.extend_from_slice(&smart_rollup_address);
        match input {
            Input::ModeSpecific(ProxyInput::SimpleTransaction(tx)) => {
                // Simple transaction tag
                buffer.push(0);
                buffer.extend_from_slice(&tx.tx_hash);
                let mut tx_bytes = match tx.content {
                    Ethereum(tx) => tx.into(),
                    _ => panic!(
                        "Simple transaction can contain only ethereum transactions"
                    ),
                };

                buffer.append(&mut tx_bytes)
            }
            Input::ModeSpecific(ProxyInput::NewChunkedTransaction {
                tx_hash,
                num_chunks,
                chunk_hashes,
            }) => {
                // New chunked transaction tag
                buffer.push(1);
                buffer.extend_from_slice(&tx_hash);
                buffer.extend_from_slice(&u16::to_le_bytes(num_chunks));
                for chunk_hash in chunk_hashes.iter() {
                    buffer.extend_from_slice(chunk_hash)
                }
            }
            Input::ModeSpecific(ProxyInput::TransactionChunk {
                tx_hash,
                i,
                chunk_hash,
                data,
            }) => {
                // Transaction chunk tag
                buffer.push(2);
                buffer.extend_from_slice(&tx_hash);
                buffer.extend_from_slice(&u16::to_le_bytes(i));
                buffer.extend_from_slice(&chunk_hash);
                buffer.extend_from_slice(&data);
            }
            _ => (),
        };
        buffer
    }

    fn make_chunked_transactions(
        tx_hash: TransactionHash,
        data: Vec<u8>,
    ) -> Vec<Input<ProxyInput>> {
        let mut chunk_hashes = vec![];
        let mut chunks: Vec<Input<ProxyInput>> = data
            .chunks(MAX_SIZE_PER_CHUNK)
            .enumerate()
            .map(|(i, bytes)| {
                let data = bytes.to_vec();
                let chunk_hash = Keccak256::digest(&data).into();
                chunk_hashes.push(chunk_hash);
                Input::ModeSpecific(ProxyInput::TransactionChunk {
                    tx_hash,
                    i: i as u16,
                    chunk_hash,
                    data,
                })
            })
            .collect();
        let number_of_chunks = chunks.len() as u16;

        let new_chunked_transaction =
            Input::ModeSpecific(ProxyInput::NewChunkedTransaction {
                tx_hash,
                num_chunks: number_of_chunks,
                chunk_hashes,
            });

        let mut buffer = Vec::new();
        buffer.push(new_chunked_transaction);
        buffer.append(&mut chunks);
        buffer
    }

    fn large_transaction() -> (Vec<u8>, EthereumTransactionCommon) {
        let data: Vec<u8> = hex::decode(["f917e180843b9aca0082520894b53dc01974176e5dff2298c5a94343c2585e3c548a021dfe1f5c5363780000b91770".to_string(), "a".repeat(12_000), "820a96a07fd9567a72223bbc8f70bd2b42011339b61044d16b5a2233534db8ca01f3e57aa03ea489c4bb2b2b52f3c1a18966881114767654c9ab61d46b1fbff78a498043c2".to_string()].join("")).unwrap();
        let tx = EthereumTransactionCommon::from_bytes(&data).unwrap();
        (data, tx)
    }

    #[test]
    fn parse_valid_simple_transaction() {
        let mut host = MockKernelHost::default();

        let tx_bytes = &hex::decode("f86d80843b9aca00825208940b52d4d3be5d18a7ab5e4476a2f5382bbf2b38d888016345785d8a000080820a95a0d9ef1298c18c88604e3f08e14907a17dfa81b1dc6b37948abe189d8db5cb8a43a06fc7040a71d71d3cb74bd05ead7046b10668ad255da60391c017eea31555f156").unwrap();
        let tx = EthereumTransactionCommon::from_bytes(tx_bytes).unwrap();
        let tx_hash = Keccak256::digest(tx_bytes).into();
        let input =
            Input::ModeSpecific(ProxyInput::SimpleTransaction(Box::new(Transaction {
                tx_hash,
                content: Ethereum(tx.clone()),
            })));

        host.host
            .add_external(Bytes::from(input_to_bytes(SMART_ROLLUP_ADDRESS, input)));

        let inbox_content = read_proxy_inbox(
            &mut host,
            SMART_ROLLUP_ADDRESS,
            &TezosContracts::default(),
            false,
            &test_evm_chain_config(),
        )
        .unwrap()
        .unwrap();
        let expected_transactions = vec![Transaction {
            tx_hash,
            content: Ethereum(tx),
        }];
        assert_eq!(inbox_content.transactions, expected_transactions);
    }

    #[test]
    fn parse_valid_chunked_transaction() {
        let address = smart_rollup_address();
        let mut host = MockKernelHost::with_address(address);

        let (data, tx) = large_transaction();
        let tx_hash: [u8; TRANSACTION_HASH_SIZE] = Keccak256::digest(data.clone()).into();

        let inputs = make_chunked_transactions(tx_hash, data);

        for input in inputs {
            host.host
                .add_external(Bytes::from(input_to_bytes(SMART_ROLLUP_ADDRESS, input)))
        }

        let inbox_content = read_proxy_inbox(
            &mut host,
            SMART_ROLLUP_ADDRESS,
            &TezosContracts::default(),
            false,
            &test_evm_chain_config(),
        )
        .unwrap()
        .unwrap();
        let expected_transactions = vec![Transaction {
            tx_hash,
            content: Ethereum(tx),
        }];
        assert_eq!(inbox_content.transactions, expected_transactions);
    }

    #[test]
    fn parse_valid_kernel_upgrade() {
        let mut host = MockKernelHost::default();

        // Prepare the upgrade's payload
        let preimage_hash: [u8; PREIMAGE_HASH_SIZE] = hex::decode(
            "004b28109df802cb1885ab29461bc1b410057a9f3a848d122ac7a742351a3a1f4e",
        )
        .unwrap()
        .try_into()
        .unwrap();
        let activation_timestamp = Timestamp::from(0i64);

        let kernel_upgrade = KernelUpgrade {
            preimage_hash,
            activation_timestamp,
        };
        let kernel_upgrade_payload = kernel_upgrade.rlp_bytes().to_vec();

        // Create a transfer from the bridge contract, that act as the
        // dictator (or administrator).
        let source =
            PublicKeyHash::from_b58check("tz1NiaviJwtMbpEcNqSP6neeoBYj8Brb3QPv").unwrap();
        let contract =
            Contract::from_b58check("KT1HJphVV3LUxqZnc7YSH6Zdfd3up1DjLqZv").unwrap();
        let sender = match contract {
            Contract::Originated(kt1) => kt1,
            _ => panic!("The contract must be a KT1"),
        };
        let payload: RollupType =
            MichelsonOr::Right(MichelsonBytes(kernel_upgrade_payload));

        let transfer_metadata = TransferMetadata::new(sender.clone(), source);
        host.host.add_transfer(payload, &transfer_metadata);
        let _inbox_content = read_proxy_inbox(
            &mut host,
            [0; 20],
            &TezosContracts {
                ticketer: None,
                admin: Some(sender),
                sequencer_governance: None,
                kernel_governance: None,
                kernel_security_governance: None,
            },
            false,
            &test_evm_chain_config(),
        )
        .unwrap()
        .unwrap();
        let expected_upgrade = Some(KernelUpgrade {
            preimage_hash,
            activation_timestamp,
        });

        let stored_kernel_upgrade = crate::upgrade::read_kernel_upgrade(&host).unwrap();
        assert_eq!(stored_kernel_upgrade, expected_upgrade);
    }

    #[test]
    // Assert that trying to create a chunked transaction has no impact. Only
    // the first `NewChunkedTransaction` should be considered.
    fn recreate_chunked_transaction() {
        let mut host = MockKernelHost::default();

        let chunk_hashes = vec![[1; TRANSACTION_HASH_SIZE], [2; TRANSACTION_HASH_SIZE]];
        let tx_hash = [0; TRANSACTION_HASH_SIZE];
        let new_chunk1 = Input::ModeSpecific(ProxyInput::NewChunkedTransaction {
            tx_hash,
            num_chunks: 2,
            chunk_hashes: chunk_hashes.clone(),
        });
        let new_chunk2 = Input::ModeSpecific(ProxyInput::NewChunkedTransaction {
            tx_hash,
            num_chunks: 42,
            chunk_hashes,
        });

        host.host.add_external(Bytes::from(input_to_bytes(
            SMART_ROLLUP_ADDRESS,
            new_chunk1,
        )));
        host.host.add_external(Bytes::from(input_to_bytes(
            SMART_ROLLUP_ADDRESS,
            new_chunk2,
        )));

        let _inbox_content = read_proxy_inbox(
            &mut host,
            SMART_ROLLUP_ADDRESS,
            &TezosContracts::default(),
            false,
            &test_evm_chain_config(),
        )
        .unwrap();

        let num_chunks = chunked_transaction_num_chunks(&mut host, &tx_hash)
            .expect("The number of chunks should exist");
        // Only the first `NewChunkedTransaction` should be considered.
        assert_eq!(num_chunks, 2);
    }

    #[test]
    // Assert that an out of bound chunk is simply ignored and does
    // not make the kernel fail.
    fn out_of_bound_chunk_is_ignored() {
        let mut host = MockKernelHost::default();

        let (data, _tx) = large_transaction();
        let tx_hash = ZERO_TX_HASH;

        let mut inputs = make_chunked_transactions(tx_hash, data);
        let new_chunk = inputs.remove(0);
        let chunk = inputs.remove(0);

        // Announce a chunked transaction.
        host.host
            .add_external(Bytes::from(input_to_bytes(SMART_ROLLUP_ADDRESS, new_chunk)));

        // Give a chunk with an invalid `i`.
        let out_of_bound_i = 42;
        let chunk = match chunk {
            Input::ModeSpecific(ProxyInput::TransactionChunk {
                tx_hash,
                i: _,
                chunk_hash,
                data,
            }) => Input::ModeSpecific(ProxyInput::TransactionChunk {
                tx_hash,
                i: out_of_bound_i,
                chunk_hash,
                data,
            }),
            _ => panic!("Expected a transaction chunk"),
        };
        host.host
            .add_external(Bytes::from(input_to_bytes(SMART_ROLLUP_ADDRESS, chunk)));

        let _inbox_content = read_proxy_inbox(
            &mut host,
            SMART_ROLLUP_ADDRESS,
            &TezosContracts::default(),
            false,
            &test_evm_chain_config(),
        )
        .unwrap();

        // The out of bounds chunk should not exist.
        let chunked_transaction_path = chunked_transaction_path(&tx_hash).unwrap();
        let transaction_chunk_path =
            transaction_chunk_path(&chunked_transaction_path, out_of_bound_i).unwrap();
        if read_transaction_chunk_data(&mut host, &transaction_chunk_path).is_ok() {
            panic!("The chunk should not exist in the storage")
        }
    }

    #[test]
    // Assert that an unknown chunk is simply ignored and does
    // not make the kernel fail.
    fn unknown_chunk_is_ignored() {
        let mut host = MockKernelHost::default();

        let (data, _tx) = large_transaction();
        let tx_hash = ZERO_TX_HASH;

        let mut inputs = make_chunked_transactions(tx_hash, data);
        let chunk = inputs.remove(1);

        // Extract the index of the non existing chunked transaction.
        let index = match chunk {
            Input::ModeSpecific(ProxyInput::TransactionChunk { i, .. }) => i,
            _ => panic!("Expected a transaction chunk"),
        };

        host.host
            .add_external(Bytes::from(input_to_bytes(SMART_ROLLUP_ADDRESS, chunk)));

        let _inbox_content = read_proxy_inbox(
            &mut host,
            SMART_ROLLUP_ADDRESS,
            &TezosContracts::default(),
            false,
            &test_evm_chain_config(),
        )
        .unwrap();

        // The unknown chunk should not exist.
        let chunked_transaction_path = chunked_transaction_path(&tx_hash).unwrap();
        let transaction_chunk_path =
            transaction_chunk_path(&chunked_transaction_path, index).unwrap();
        if read_transaction_chunk_data(&mut host, &transaction_chunk_path).is_ok() {
            panic!("The chunk should not exist in the storage")
        }
    }

    #[test]
    // Assert that a transaction is marked as complete only when each chunk
    // is stored in the storage. That is, if a transaction chunk is sent twice,
    // it rewrites the chunk.
    //
    // This serves as a non-regression test, a previous optimization made unwanted
    // behavior for very little gain:
    //
    // Level 0:
    // - New chunk of size 2
    // - Chunk 0
    //
    // Level 1:
    // - New chunk of size 2 (ignored)
    // - Chunk 0
    // |--> Oh great! I have the two chunks for my transaction, it is then complete!
    // - Chunk 1
    // |--> Fails because the chunk is unknown
    fn transaction_is_complete_when_each_chunk_is_stored() {
        let mut host = MockKernelHost::default();

        let (data, tx) = large_transaction();
        let tx_hash: [u8; TRANSACTION_HASH_SIZE] = Keccak256::digest(data.clone()).into();

        let inputs = make_chunked_transactions(tx_hash, data);
        // The test works if there are 3 inputs: new chunked of size 2, first and second
        // chunks.
        assert_eq!(inputs.len(), 3);

        let new_chunk = inputs[0].clone();
        let chunk0 = inputs[1].clone();

        host.host
            .add_external(Bytes::from(input_to_bytes(SMART_ROLLUP_ADDRESS, new_chunk)));

        host.host
            .add_external(Bytes::from(input_to_bytes(SMART_ROLLUP_ADDRESS, chunk0)));

        let inbox_content = read_proxy_inbox(
            &mut host,
            SMART_ROLLUP_ADDRESS,
            &TezosContracts::default(),
            false,
            &test_evm_chain_config(),
        )
        .unwrap()
        .unwrap();
        assert_eq!(
            inbox_content,
            ProxyInboxContent {
                transactions: vec![],
            }
        );

        // On the next level, try to re-give the chunks, but this time in full:
        for input in inputs {
            host.host
                .add_external(Bytes::from(input_to_bytes(SMART_ROLLUP_ADDRESS, input)))
        }
        let inbox_content = read_proxy_inbox(
            &mut host,
            SMART_ROLLUP_ADDRESS,
            &TezosContracts::default(),
            false,
            &test_evm_chain_config(),
        )
        .unwrap()
        .unwrap();

        let expected_transactions = vec![Transaction {
            tx_hash,
            content: Ethereum(tx),
        }];
        assert_eq!(inbox_content.transactions, expected_transactions);
    }

    #[test]
    fn parse_valid_simple_transaction_framed() {
        // Don't use zero-hash for rollup here - as the long string of zeros is still valid under the previous
        // parsing. This won't happen in practice, though
        let address = smart_rollup_address();

        let mut host = MockKernelHost::with_address(address.clone());

        let tx_bytes = &hex::decode("f86d80843b9aca00825208940b52d4d3be5d18a7ab5\
        e4476a2f5382bbf2b38d888016345785d8a000080820a95a0d9ef1298c18c88604e3f08e14907a17dfa81b1dc6b37948abe189d8db5cb8a43a06\
        fc7040a71d71d3cb74bd05ead7046b10668ad255da60391c017eea31555f156").unwrap();
        let tx_hash = Keccak256::digest(tx_bytes).into();
        let tx = EthereumTransactionCommon::from_bytes(tx_bytes).unwrap();

        let input =
            Input::ModeSpecific(ProxyInput::SimpleTransaction(Box::new(Transaction {
                tx_hash,
                content: Ethereum(tx.clone()),
            })));

        let mut buffer = Vec::new();
        match input {
            Input::ModeSpecific(ProxyInput::SimpleTransaction(tx)) => {
                // Simple transaction tag
                buffer.push(0);
                buffer.extend_from_slice(&tx.tx_hash);
                let mut tx_bytes = match tx.content {
                    Ethereum(tx) => tx.into(),
                    _ => panic!(
                        "Simple transaction can contain only ethereum transactions"
                    ),
                };

                buffer.append(&mut tx_bytes)
            }
            _ => unreachable!("Not tested"),
        };

        let framed = ExternalMessageFrame::Targetted {
            address,
            contents: buffer,
        };

        host.host.add_external(framed);

        let inbox_content = read_proxy_inbox(
            &mut host,
            SMART_ROLLUP_ADDRESS,
            &TezosContracts::default(),
            false,
            &test_evm_chain_config(),
        )
        .unwrap()
        .unwrap();
        let expected_transactions = vec![Transaction {
            tx_hash,
            content: Ethereum(tx),
        }];
        assert_eq!(inbox_content.transactions, expected_transactions);
    }

    #[test]
    fn empty_inbox_returns_none() {
        let mut host = MockKernelHost::default();

        // Even reading the inbox with only the default elements returns
        // an empty inbox content. As we test in isolation there is nothing
        // in the inbox, we mock it by adding a single input.
        host.host.add_external(Bytes::from(vec![]));
        let inbox_content = read_proxy_inbox(
            &mut host,
            SMART_ROLLUP_ADDRESS,
            &TezosContracts::default(),
            false,
            &test_evm_chain_config(),
        )
        .unwrap();
        assert!(inbox_content.is_some());

        // Reading again the inbox returns no inbox content at all.
        let inbox_content = read_proxy_inbox(
            &mut host,
            SMART_ROLLUP_ADDRESS,
            &TezosContracts::default(),
            false,
            &test_evm_chain_config(),
        )
        .unwrap();
        assert!(inbox_content.is_none());
    }

    fn bytes_to_hex(bytes: &[u8]) -> String {
        bytes.iter().fold(String::new(), |mut acc, &b| {
            write!(acc, "{b:02x}").expect("Failed to write to string");
            acc
        })
    }

    #[test]
    fn rlp_encode_decode_dal_slot_signals_with_signature() {
        let signal_1 = DalSlotIndicesOfLevel {
            published_level: 100,
            slot_indices: DalSlotIndicesList(vec![1, 2, 3]),
        };
        let signal_2 = DalSlotIndicesOfLevel {
            published_level: 200,
            slot_indices: DalSlotIndicesList(vec![4, 2, 6]),
        };
        let signal_3 = DalSlotIndicesOfLevel {
            published_level: 100,
            slot_indices: DalSlotIndicesList(vec![10, 2, 5]),
        };

        let signals = UnsignedDalSlotSignals(vec![signal_1, signal_2, signal_3]);

        let signature = UnknownSignature::from_base58_check(
            "sigdGBG68q2vskMuac4AzyNb1xCJTfuU8MiMbQtmZLUCYydYrtTd5Lessn1EFLTDJzjXoYxRasZxXbx6tHnirbEJtikcMHt3"
        ).expect("signature decoding should work");

        let dal_slot_signal_list = DalSlotImportSignals { signals, signature };

        println!("Initial dal_slot_signal_list: {dal_slot_signal_list:?}");

        // Encode the structure
        let encoded = rlp::encode(&dal_slot_signal_list);
        let encoded_hex = bytes_to_hex(&encoded);

        println!("Encoded DAL slot signal (hex): {encoded_hex}");

        // Decode the structure
        let decoded: DalSlotImportSignals =
            rlp::decode(&encoded).expect("RLP decoding should succeed.");

        println!("Decoded dal_slot_signal_list: {decoded:?}");

        // Ensure that the encoded and decoded structures match
        assert_eq!(dal_slot_signal_list, decoded);
    }

    fn insert_blueprint_and_read_inbox(
        head_level: U256,
        sk: &SecretKeyEd25519,
        pk: &PublicKey,
        unsigned_blueprint: &UnsignedSequencerBlueprint,
    ) -> bool {
        // Prepare the host.
        let mut host = MockKernelHost::default();
        let address = smart_rollup_address();
        let evm_block_header = EVMBlockHeader {
            hash: crate::block::GENESIS_PARENT_HASH,
            receipts_root: vec![],
            transactions_root: vec![],
        };
        store_current_block_header(
            &mut host,
            &BlockHeader {
                blueprint_header: BlueprintHeader {
                    number: head_level,
                    timestamp: 0.into(),
                },
                chain_header: ChainHeader::Eth(evm_block_header),
            },
        )
        .unwrap();

        // Prepare the blueprint.
        let mut blueprint_bytes =
            crate::parsing::tests::sequencer_signed_blueprint_chunk_bytes(
                unsigned_blueprint,
                sk.clone(),
            );
        blueprint_bytes.insert(0, 3);
        let framed = ExternalMessageFrame::Targetted {
            address: address.clone(),
            contents: blueprint_bytes,
        };
        // Add to the inbox.
        host.host.add_external(framed);
        // Consume the inbox
        let mut delayed_inbox = DelayedInbox::new(&mut host).unwrap();
        let _ = read_sequencer_inbox(
            &mut host,
            SMART_ROLLUP_ADDRESS,
            &TezosContracts::default(),
            ContractKt1Hash::from_b58check("KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT")
                .unwrap(),
            pk.clone(),
            &mut delayed_inbox,
            false,
            MAX_ALLOWED_TICKS,
            None,
            &test_evm_chain_config(),
        )
        .unwrap();

        let path = blueprint_path(unsigned_blueprint.number).unwrap();
        // The blueprint was valid if it was stored in the storage.
        host.host.store_has(&path).unwrap().is_some()
    }

    #[test]
    fn test_read_sequencer_inbox_blueprint_chunk() {
        let head_level: U256 = U256::from(6);

        // Pick a sequencer public key.
        let pk = PublicKey::from_b58check(
            "edpkv4NmL2YPe8eiVGXUDXmPQybD725ofKirTzGRxs1X9UmaG3voKw",
        )
        .unwrap();

        // This is the secret key associated to the sequencer key in
        // this test.
        let valid_sk = SecretKeyEd25519::from_base58_check(
            "edsk422LGdmDnai4Cya6csM6oFmgHpDQKUhatTURJRAY4h7NHNz9sz",
        )
        .unwrap();

        // Insert a valid blueprint chunk on level 7.
        let blueprint = UnsignedSequencerBlueprint {
            chunk: vec![],
            number: 7.into(),
            nb_chunks: 3,
            chunk_index: 0,
            chain_id: None,
        };
        let is_valid =
            insert_blueprint_and_read_inbox(head_level, &valid_sk, &pk, &blueprint);
        assert!(is_valid);

        // Insert a blueprint chunk on level 7 incorrectly signed.
        let invalid_sk = SecretKeyEd25519::from_base58_check(
            "edsk37VEgDUMt7wje8vxfao55y7JhiamjTbVM1xABSCamFgtcUqhdT",
        )
        .unwrap();
        let is_valid =
            insert_blueprint_and_read_inbox(head_level, &invalid_sk, &pk, &blueprint);
        assert!(!is_valid);

        // Insert a blueprint chunk on level 6.
        let is_valid = insert_blueprint_and_read_inbox(
            head_level,
            &valid_sk,
            &pk,
            &UnsignedSequencerBlueprint {
                number: 6.into(),
                ..blueprint.clone()
            },
        );
        assert!(!is_valid);

        // Insert a blueprint chunk on level 5.
        let is_valid = insert_blueprint_and_read_inbox(
            head_level,
            &valid_sk,
            &pk,
            &UnsignedSequencerBlueprint {
                number: 5.into(),
                ..blueprint
            },
        );
        assert!(!is_valid);
    }
}

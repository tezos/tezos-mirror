// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::blueprint_storage::MAXIMUM_NUMBER_OF_CHUNKS;
use crate::chains::ExperimentalFeatures;
use crate::configuration::{DalConfiguration, TezosContracts};
use crate::tick_model::constants::{
    TICKS_FOR_BLUEPRINT_CHUNK_SIGNATURE, TICKS_FOR_DELAYED_MESSAGES,
    TICKS_PER_DEPOSIT_PARSING,
};
use crate::{
    bridge::Deposit,
    dal_slot_import_signal::DalSlotImportSignals,
    sequencer_blueprint::{SequencerBlueprint, UnsignedSequencerBlueprint},
    transaction::{Transaction, TransactionContent},
    upgrade::KernelUpgrade,
    upgrade::SequencerUpgrade,
};
use primitive_types::U256;
use revm_etherlink::helpers::legacy::FaDeposit;
use revm_etherlink::precompiles::constants::TICKS_PER_FA_DEPOSIT_PARSING;
use rlp::Encodable;
use sha3::{Digest, Keccak256};
use tezos_crypto_rs::{hash::ContractKt1Hash, PublicKeySignatureVerifier};
use tezos_ethereum::{
    rlp_helpers::FromRlpBytes,
    transaction::{TransactionHash, TRANSACTION_HASH_SIZE},
    tx_common::EthereumTransactionCommon,
};
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_protocol::contract::Contract;
use tezos_smart_rollup_encoding::{
    inbox::{
        ExternalMessageFrame, InboxMessage, InfoPerLevel, InternalInboxMessage, Transfer,
    },
    michelson::{ticket::FA2_1Ticket, MichelsonBytes, MichelsonOr, MichelsonPair},
    public_key::PublicKey,
};
use tezos_smart_rollup_host::input::Message;

/// On an option, either the value, or if `None`, interrupt and return the
/// default value of the return type instead.
#[macro_export]
macro_rules! parsable {
    ($expr : expr) => {
        match $expr {
            Some(x) => x,
            None => return Default::default(),
        }
    };
}

/// Divides one slice into two at an index.
///
/// The first will contain all indices from `[0, mid)` (excluding the index
/// `mid` itself) and the second will contain all indices from `[mid, len)`
/// (excluding the index `len` itself).
///
/// Will return None if `mid > len`.
pub fn split_at(bytes: &[u8], mid: usize) -> Option<(&[u8], &[u8])> {
    let left = bytes.get(0..mid)?;
    let right = bytes.get(mid..)?;
    Some((left, right))
}

pub const SIMULATION_TAG: u8 = u8::MAX;

const SIMPLE_TRANSACTION_TAG: u8 = 0;

const NEW_CHUNKED_TRANSACTION_TAG: u8 = 1;

const TRANSACTION_CHUNK_TAG: u8 = 2;

const SEQUENCER_BLUEPRINT_TAG: u8 = 3;

pub const DAL_SLOT_IMPORT_SIGNAL_TAG: u8 = 4;

const FORCE_KERNEL_UPGRADE_TAG: u8 = 0xff;

pub const MAX_SIZE_PER_CHUNK: usize = 4095 // Max input size minus external tag
            - 1 // ExternalMessageFrame tag
            - 20 // Smart rollup address size (ExternalMessageFrame::Targetted)
            - 1  // Transaction chunk tag
            - 2  // Number of chunks (u16)
            - 32 // Transaction hash size
            - 32; // Chunk hash size

#[derive(Debug, PartialEq, Clone)]
pub struct LevelWithInfo {
    pub level: u32,
    pub info: InfoPerLevel,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ProxyInput {
    SimpleTransaction(Box<Transaction>),
    NewChunkedTransaction {
        tx_hash: TransactionHash,
        num_chunks: u16,
        chunk_hashes: Vec<TransactionHash>,
    },
    TransactionChunk {
        tx_hash: TransactionHash,
        i: u16,
        chunk_hash: TransactionHash,
        data: Vec<u8>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum SequencerBlueprintRes {
    SequencerBlueprint(UnsignedSequencerBlueprint),
    InvalidNumberOfChunks,
    InvalidSignature,
    InvalidNumber,
    Unparsable,
}

#[derive(Debug, PartialEq, Clone)]
pub enum SequencerInput {
    DelayedInput(Box<Transaction>),
    SequencerBlueprint(SequencerBlueprintRes),
    DalSlotImportSignals(DalSlotImportSignals),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Input<Mode> {
    ModeSpecific(Mode),
    Deposit((Deposit, Option<U256>)),
    FaDeposit((FaDeposit, Option<U256>)),
    Upgrade(KernelUpgrade),
    SequencerUpgrade(SequencerUpgrade),
    RemoveSequencer,
    Info(LevelWithInfo),
    ForceKernelUpgrade,
    /// DAL attested slots from the protocol - contains published_level, slot parameters, and list of attested slot indices.
    DalAttestedSlots {
        published_level: i32,
        slot_size: u64,
        page_size: u64,
        slot_indices: Vec<u8>,
    },
}

#[derive(Debug, PartialEq, Default)]
pub enum InputResult<Mode> {
    /// No further inputs
    NoInput,
    /// Some decoded input
    Input(Input<Mode>),
    /// Simulation mode starts after this input
    Simulation,
    #[default]
    /// Unparsable input, to be ignored
    Unparsable,
}

pub type RollupType = MichelsonOr<
    MichelsonOr<MichelsonPair<MichelsonBytes, FA2_1Ticket>, MichelsonBytes>,
    MichelsonBytes,
>;

/// Implements the trait for an input to be readable from the inbox, either
/// being an external input or an L1 smart contract input. It assumes all
/// verifications have already been done:
///
/// - The original inputs are prefixed by the frame protocol for the correct
///   rollup, and the prefix has been removed
///
/// - The internal message was addressed to the rollup, and `parse_internal`
///   expects the bytes from `Left (Right <bytes>)`
pub trait Parsable {
    type Context;

    fn parse_external<Host: Runtime>(
        host: &mut Host,
        tag: &u8,
        input: &[u8],
        context: &mut Self::Context,
    ) -> InputResult<Self>
    where
        Self: std::marker::Sized;

    fn parse_internal_bytes(
        source: ContractKt1Hash,
        bytes: &[u8],
        context: &mut Self::Context,
    ) -> InputResult<Self>
    where
        Self: std::marker::Sized;

    fn on_deposit(context: &mut Self::Context);

    fn on_fa_deposit(context: &mut Self::Context);

    /// Get the whitelist of authorized DAL publishers from the context.
    /// Returns an empty slice if no whitelist is configured or in proxy mode.
    /// NOTE: Empty whitelist means reject all publishers (therefore all slots).
    fn dal_publishers_whitelist(
        context: &Self::Context,
    ) -> &[tezos_smart_rollup_encoding::public_key_hash::PublicKeyHash];
}

impl ProxyInput {
    fn parse_simple_transaction(bytes: &[u8]) -> InputResult<Self> {
        // Next 32 bytes is the transaction hash.
        // Remaining bytes is the rlp encoded transaction.
        let (tx_hash, remaining) = parsable!(split_at(bytes, TRANSACTION_HASH_SIZE));
        let tx_hash: TransactionHash = parsable!(tx_hash.try_into().ok());
        let produced_hash: [u8; TRANSACTION_HASH_SIZE] =
            Keccak256::digest(remaining).into();
        if tx_hash != produced_hash {
            // The produced hash from the transaction data is not the same as the
            // one sent, the message is ignored.
            return InputResult::Unparsable;
        }
        let tx: EthereumTransactionCommon = parsable!(remaining.try_into().ok());
        InputResult::Input(Input::ModeSpecific(Self::SimpleTransaction(Box::new(
            Transaction {
                tx_hash,
                content: TransactionContent::Ethereum(tx),
            },
        ))))
    }

    fn parse_new_chunked_transaction(bytes: &[u8]) -> InputResult<Self> {
        // Next 32 bytes is the transaction hash.
        let (tx_hash, remaining) = parsable!(split_at(bytes, TRANSACTION_HASH_SIZE));
        let tx_hash: TransactionHash = parsable!(tx_hash.try_into().ok());
        // Next 2 bytes is the number of chunks.
        let (num_chunks, remaining) = parsable!(split_at(remaining, 2));
        let num_chunks = u16::from_le_bytes(num_chunks.try_into().unwrap());
        if remaining.len() != (TRANSACTION_HASH_SIZE * usize::from(num_chunks)) {
            return InputResult::Unparsable;
        }
        let mut chunk_hashes = vec![];
        let mut remaining = remaining;
        for _ in 0..num_chunks {
            let (chunk_hash, remaining_hashes) =
                parsable!(split_at(remaining, TRANSACTION_HASH_SIZE));
            let chunk_hash: TransactionHash = parsable!(chunk_hash.try_into().ok());
            remaining = remaining_hashes;
            chunk_hashes.push(chunk_hash)
        }
        InputResult::Input(Input::ModeSpecific(Self::NewChunkedTransaction {
            tx_hash,
            num_chunks,
            chunk_hashes,
        }))
    }

    fn parse_transaction_chunk(bytes: &[u8]) -> InputResult<Self> {
        // Next 32 bytes is the transaction hash.
        let (tx_hash, remaining) = parsable!(split_at(bytes, TRANSACTION_HASH_SIZE));
        let tx_hash: TransactionHash = parsable!(tx_hash.try_into().ok());
        // Next 2 bytes is the index.
        let (i, remaining) = parsable!(split_at(remaining, 2));
        let i = u16::from_le_bytes(i.try_into().unwrap());
        // Next 32 bytes is the chunk hash.
        let (chunk_hash, remaining) =
            parsable!(split_at(remaining, TRANSACTION_HASH_SIZE));
        let chunk_hash: TransactionHash = parsable!(chunk_hash.try_into().ok());
        let data_hash: [u8; TRANSACTION_HASH_SIZE] = Keccak256::digest(remaining).into();
        // Check if the produced hash from the data is the same as the chunk hash.
        if chunk_hash != data_hash {
            return InputResult::Unparsable;
        }
        InputResult::Input(Input::ModeSpecific(Self::TransactionChunk {
            tx_hash,
            i,
            chunk_hash,
            data: remaining.to_vec(),
        }))
    }
}

impl Parsable for ProxyInput {
    type Context = ();

    fn parse_external<Host: Runtime>(
        _host: &mut Host,
        tag: &u8,
        input: &[u8],
        _: &mut (),
    ) -> InputResult<Self> {
        // External transactions are only allowed in proxy mode
        match *tag {
            SIMPLE_TRANSACTION_TAG => Self::parse_simple_transaction(input),
            NEW_CHUNKED_TRANSACTION_TAG => Self::parse_new_chunked_transaction(input),
            TRANSACTION_CHUNK_TAG => Self::parse_transaction_chunk(input),
            _ => InputResult::Unparsable,
        }
    }

    fn parse_internal_bytes(
        _: ContractKt1Hash,
        _: &[u8],
        _: &mut (),
    ) -> InputResult<Self> {
        InputResult::Unparsable
    }

    fn on_deposit(_: &mut Self::Context) {}

    fn on_fa_deposit(_: &mut Self::Context) {}

    fn dal_publishers_whitelist(
        _context: &Self::Context,
    ) -> &[tezos_smart_rollup_encoding::public_key_hash::PublicKeyHash] {
        // Proxy mode doesn't have a whitelist - return empty slice (rejects all publishers)
        &[]
    }
}

pub struct BufferTransactionChunks {
    pub total: u8,
    pub accumulated: u8,
    pub chunks: Vec<u8>,
}

pub struct SequencerParsingContext {
    pub sequencer: PublicKey,
    pub delayed_bridge: ContractKt1Hash,
    pub allocated_ticks: u64,
    pub dal_configuration: Option<DalConfiguration>,
    // Delayed inbox transactions may come in chunks. If the buffer is
    // [Some _] a chunked transaction is being parsed,
    pub buffer_transaction_chunks: Option<BufferTransactionChunks>,
    // Number of the next expected blueprint, handling blueprints
    // before this is useless.
    pub next_blueprint_number: U256,
    pub experimental_features: ExperimentalFeatures,
    // When true, legacy DAL slot import signals are disabled.
    // The kernel will rely on DalAttestedSlots internal messages instead.
    pub legacy_dal_signals_disabled: bool,
    // Whitelist of authorized DAL publishers (public key hashes).
    // Only slots published by these keys will be accepted from DalAttestedSlots messages.
    pub dal_publishers_whitelist:
        Vec<tezos_smart_rollup_encoding::public_key_hash::PublicKeyHash>,
}

fn check_unsigned_blueprint_chunk(
    unsigned_seq_blueprint: UnsignedSequencerBlueprint,
    next_blueprint_number: &U256,
) -> SequencerBlueprintRes {
    // Important: These checks need to be replicated in the `Evm_context` worker
    // of the EVM node. If a change is not backward compatible, then a storage
    // version will have to be added so that the EVM node can distinguish between
    // the past and new behavior.

    if MAXIMUM_NUMBER_OF_CHUNKS < unsigned_seq_blueprint.nb_chunks {
        return SequencerBlueprintRes::InvalidNumberOfChunks;
    }

    if unsigned_seq_blueprint.number.lt(next_blueprint_number) {
        return SequencerBlueprintRes::InvalidNumber;
    }

    SequencerBlueprintRes::SequencerBlueprint(unsigned_seq_blueprint)
}

pub fn parse_unsigned_blueprint_chunk(
    bytes: &[u8],
    next_blueprint_number: &U256,
) -> SequencerBlueprintRes {
    // Parse an unsigned sequencer blueprint
    match UnsignedSequencerBlueprint::from_rlp_bytes(bytes).ok() {
        None => SequencerBlueprintRes::Unparsable,
        Some(unsigned_seq_blueprint) => {
            check_unsigned_blueprint_chunk(unsigned_seq_blueprint, next_blueprint_number)
        }
    }
}

pub fn parse_blueprint_chunk(
    bytes: &[u8],
    sequencer: &PublicKey,
    next_blueprint_number: &U256,
) -> SequencerBlueprintRes {
    // Parse the sequencer blueprint
    match SequencerBlueprint::from_rlp_bytes(bytes).ok() {
        None => SequencerBlueprintRes::Unparsable,
        Some(SequencerBlueprint {
            blueprint,
            signature,
        }) => match check_unsigned_blueprint_chunk(blueprint, next_blueprint_number) {
            SequencerBlueprintRes::SequencerBlueprint(unsigned_seq_blueprint) => {
                let bytes = unsigned_seq_blueprint.rlp_bytes().to_vec();

                let correctly_signed = sequencer
                    .verify_signature(&signature.clone().into(), &bytes)
                    .unwrap_or(false);

                if correctly_signed {
                    SequencerBlueprintRes::SequencerBlueprint(unsigned_seq_blueprint)
                } else {
                    SequencerBlueprintRes::InvalidSignature
                }
            }
            res @ (SequencerBlueprintRes::InvalidNumberOfChunks
            | SequencerBlueprintRes::InvalidSignature
            | SequencerBlueprintRes::InvalidNumber
            | SequencerBlueprintRes::Unparsable) => res,
        },
    }
}

impl SequencerInput {
    fn parse_sequencer_blueprint_input(
        bytes: &[u8],
        context: &mut SequencerParsingContext,
    ) -> InputResult<Self> {
        // Inputs are 4096 bytes longs at most, and even in the future they
        // should be limited by the size of native words of the VM which is
        // 32bits.
        context.allocated_ticks = context
            .allocated_ticks
            .saturating_sub(TICKS_FOR_BLUEPRINT_CHUNK_SIGNATURE);

        let res = parse_blueprint_chunk(
            bytes,
            &context.sequencer,
            &context.next_blueprint_number,
        );
        InputResult::Input(Input::ModeSpecific(Self::SequencerBlueprint(res)))
    }

    pub fn parse_dal_slot_import_signal<Host: Runtime>(
        host: &mut Host,
        bytes: &[u8],
        context: &mut SequencerParsingContext,
    ) -> InputResult<Self> {
        // Inputs are 4096 bytes longs at most, and even in the future they
        // should be limited by the size of native words of the VM which is
        // 32bits.
        // TODO: Define the tick model as this is temporary.
        // https://gitlab.com/tezos/tezos/-/issues/7455
        context.allocated_ticks = context
            .allocated_ticks
            .saturating_sub(TICKS_FOR_BLUEPRINT_CHUNK_SIGNATURE);

        // If legacy DAL signals are disabled, ignore this message.
        // The kernel would then rely entirely on DalAttestedSlots internal
        // messages from the protocol.
        if context.legacy_dal_signals_disabled {
            log!(
                host,
                Error,
                "Legacy DAL slot import signal ignored (disable_legacy_dal_signals is enabled)"
            );
            return InputResult::Unparsable;
        }

        let Some(dal) = &context.dal_configuration else {
            return InputResult::Unparsable;
        };

        // Parse the signals
        let signed_signals: DalSlotImportSignals =
            parsable!(FromRlpBytes::from_rlp_bytes(bytes).ok());

        // Check if all slot indices are valid
        for unsigned_signal in &signed_signals.signals.0 {
            for slot_index in &unsigned_signal.slot_indices.0 {
                if !dal.slot_indices.contains(slot_index) {
                    return InputResult::Unparsable;
                }
            }
        }

        // Encode the entire list of unsigned signals
        let bytes = signed_signals.signals.rlp_bytes().to_vec();

        // Verify the signature against the entire encoded list
        let correctly_signed = context
            .sequencer
            .verify_signature(&signed_signals.signature.clone().into(), &bytes)
            .unwrap_or(false);

        if correctly_signed {
            InputResult::Input(Input::ModeSpecific(Self::DalSlotImportSignals(
                signed_signals,
            )))
        } else {
            InputResult::Unparsable
        }
    }
}

mod delayed_chunked_transaction {

    // This module implements the fairly simple logic of messaging protocol
    // for delayed transactions that does not fit in a single inbox message.
    //
    // The protocol is the following:
    //
    // [NEW_CHUNK_TAG; <len>] => Message announcing the number of chunks. The
    // <len> next messages will be the chunks.
    // [CHUNK_TAG; <payload>] => Message containing one chunk.
    //
    // We consider that all messages are transmitted within a single
    // L1 operation, but in several inbox messages.

    use crate::parsing::BufferTransactionChunks;
    use rlp::DecoderError;
    use sha3::{Digest, Keccak256};
    use tezos_data_encoding::nom::NomReader;
    use tezos_ethereum::{
        transaction::TransactionHash, tx_common::EthereumTransactionCommon,
    };
    use tezos_tezlink::operation::Operation;
    use tezosx_interfaces::RuntimeId;

    pub const NEW_CHUNK_TAG: u8 = 0x0;
    pub const CHUNK_TAG: u8 = 0x1;

    #[allow(clippy::large_enum_variant)]
    pub enum Transaction {
        Ethereum(EthereumTransactionCommon),
        Tezos(Operation),
    }

    impl Transaction {
        pub fn from_bytes(bytes: &[u8]) -> Result<Self, DecoderError> {
            if bytes.is_empty() {
                return Err(DecoderError::Custom("Empty transaction bytes"));
            }

            let (tag, remaining) = bytes
                .split_first()
                .ok_or(DecoderError::Custom("No tag for transaction"))?;
            match RuntimeId::try_from(*tag).map_err(DecoderError::Custom)? {
                RuntimeId::Ethereum => {
                    let tx: EthereumTransactionCommon =
                        EthereumTransactionCommon::from_bytes(remaining)?;
                    Ok(Transaction::Ethereum(tx))
                }
                RuntimeId::Tezos => {
                    let tx: Operation =
                        Operation::nom_read_exact(remaining).map_err(|_| {
                            DecoderError::Custom("Failed to parse Tezos operation")
                        })?;
                    Ok(Transaction::Tezos(tx))
                }
            }
        }
    }

    pub fn parse_new_chunk(
        bytes: &[u8],
        buffer_transaction_chunks: &mut Option<BufferTransactionChunks>,
    ) {
        if let [len] = bytes {
            // Overwrites any existing transaction chunks buffer. It's the
            // responsibility of the contract to send correct values.
            *buffer_transaction_chunks = Some(BufferTransactionChunks {
                total: *len,
                accumulated: 0,
                chunks: vec![],
            })
        }
    }

    pub fn parse_multi_runtime_chunk(
        bytes: &[u8],
        buffer_transaction_chunks_opt: &mut Option<BufferTransactionChunks>,
    ) -> Option<(Transaction, TransactionHash)> {
        let is_complete = match buffer_transaction_chunks_opt.as_mut() {
            None => {
                // Again, it's the responsibility of the contract to respect
                // the message protocol.
                return None;
            }
            Some(buffer_transaction_chunks) => {
                buffer_transaction_chunks.chunks.extend(bytes);
                buffer_transaction_chunks.accumulated += 1;
                buffer_transaction_chunks.total == buffer_transaction_chunks.accumulated
            }
        };

        if !is_complete {
            return None;
        }

        // Transaction is complete.
        //
        // Drop the mutable borrow before clearing the buffer.
        let buffer_transaction_chunks = buffer_transaction_chunks_opt.take()?;
        let chunks = buffer_transaction_chunks.chunks;

        let transaction = Transaction::from_bytes(&chunks).ok()?;
        let tx_hash: TransactionHash = Keccak256::digest(&chunks).into();
        Some((transaction, tx_hash))
    }

    pub fn parse_chunk(
        bytes: &[u8],
        buffer_transaction_chunks_opt: &mut Option<BufferTransactionChunks>,
    ) -> Option<(EthereumTransactionCommon, TransactionHash)> {
        match buffer_transaction_chunks_opt {
            None => {
                // Again, it's the responsibility of the contract to respect
                // the message protocol.
                None
            }
            Some(buffer_transaction_chunks) => {
                buffer_transaction_chunks.chunks.extend(bytes);
                buffer_transaction_chunks.accumulated += 1;

                if buffer_transaction_chunks.total
                    == buffer_transaction_chunks.accumulated
                {
                    // Transaction is complete
                    let res = match EthereumTransactionCommon::from_bytes(
                        &buffer_transaction_chunks.chunks,
                    ) {
                        Ok(transaction) => {
                            let tx_hash: TransactionHash =
                                Keccak256::digest(&buffer_transaction_chunks.chunks)
                                    .into();
                            Some((transaction, tx_hash))
                        }
                        Err(_) => None,
                    };
                    *buffer_transaction_chunks_opt = None;
                    res
                } else {
                    None
                }
            }
        }
    }
}

impl Parsable for SequencerInput {
    type Context = SequencerParsingContext;

    fn parse_external<Host: Runtime>(
        host: &mut Host,
        tag: &u8,
        input: &[u8],
        context: &mut Self::Context,
    ) -> InputResult<Self> {
        // External transactions are only allowed in proxy mode
        match *tag {
            SEQUENCER_BLUEPRINT_TAG => {
                Self::parse_sequencer_blueprint_input(input, context)
            }
            DAL_SLOT_IMPORT_SIGNAL_TAG => {
                Self::parse_dal_slot_import_signal(host, input, context)
            }
            _ => InputResult::Unparsable,
        }
    }

    /// Parses transactions that come from the delayed inbox.
    fn parse_internal_bytes(
        source: ContractKt1Hash,
        bytes: &[u8],
        context: &mut Self::Context,
    ) -> InputResult<Self> {
        context.allocated_ticks = context
            .allocated_ticks
            .saturating_sub(TICKS_FOR_DELAYED_MESSAGES);

        if context.delayed_bridge.as_ref() != source.as_ref() {
            return InputResult::Unparsable;
        };

        let (tag, remaining) = parsable!(bytes.split_first());

        if context.experimental_features.is_tezos_runtime_enabled() {
            let (tx, tx_hash) = parsable!(match *tag {
                delayed_chunked_transaction::NEW_CHUNK_TAG => {
                    delayed_chunked_transaction::parse_new_chunk(
                        remaining,
                        &mut context.buffer_transaction_chunks,
                    );
                    None
                }
                delayed_chunked_transaction::CHUNK_TAG =>
                    delayed_chunked_transaction::parse_multi_runtime_chunk(
                        remaining,
                        &mut context.buffer_transaction_chunks,
                    ),
                _ => None,
            });

            InputResult::Input(Input::ModeSpecific(Self::DelayedInput(Box::new(
                Transaction {
                    tx_hash,
                    content: match tx {
                        delayed_chunked_transaction::Transaction::Ethereum(tx) => {
                            TransactionContent::EthereumDelayed(tx)
                        }
                        delayed_chunked_transaction::Transaction::Tezos(op) => {
                            TransactionContent::TezosDelayed(op)
                        }
                    },
                },
            ))))
        } else {
            let (tx, tx_hash) = parsable!(match *tag {
                delayed_chunked_transaction::NEW_CHUNK_TAG => {
                    delayed_chunked_transaction::parse_new_chunk(
                        remaining,
                        &mut context.buffer_transaction_chunks,
                    );
                    None
                }
                delayed_chunked_transaction::CHUNK_TAG =>
                    delayed_chunked_transaction::parse_chunk(
                        remaining,
                        &mut context.buffer_transaction_chunks,
                    ),
                _ => None,
            });

            InputResult::Input(Input::ModeSpecific(Self::DelayedInput(Box::new(
                Transaction {
                    tx_hash,
                    content: TransactionContent::EthereumDelayed(tx),
                },
            ))))
        }
    }

    fn on_deposit(context: &mut Self::Context) {
        context.allocated_ticks = context
            .allocated_ticks
            .saturating_sub(TICKS_PER_DEPOSIT_PARSING);
    }

    fn on_fa_deposit(context: &mut Self::Context) {
        context.allocated_ticks = context
            .allocated_ticks
            .saturating_sub(TICKS_PER_FA_DEPOSIT_PARSING);
    }

    fn dal_publishers_whitelist(
        context: &Self::Context,
    ) -> &[tezos_smart_rollup_encoding::public_key_hash::PublicKeyHash] {
        &context.dal_publishers_whitelist
    }
}

impl<Mode: Parsable> InputResult<Mode> {
    fn parse_kernel_upgrade(bytes: &[u8]) -> Self {
        let kernel_upgrade = parsable!(KernelUpgrade::from_rlp_bytes(bytes).ok());
        Self::Input(Input::Upgrade(kernel_upgrade))
    }

    fn parse_sequencer_update(bytes: &[u8]) -> Self {
        if bytes.is_empty() {
            Self::Input(Input::RemoveSequencer)
        } else {
            let sequencer_upgrade =
                parsable!(SequencerUpgrade::from_rlp_bytes(bytes).ok());
            Self::Input(Input::SequencerUpgrade(sequencer_upgrade))
        }
    }

    /// Parses an external message
    ///
    // External message structure :
    // FRAMING_PROTOCOL_TARGETTED 21B / MESSAGE_TAG 1B / DATA
    pub fn parse_external<Host: Runtime>(
        host: &mut Host,
        input: &[u8],
        smart_rollup_address: &[u8],
        context: &mut Mode::Context,
    ) -> Self {
        // Compatibility with framing protocol for external messages
        let remaining = match ExternalMessageFrame::parse(input) {
            Ok(ExternalMessageFrame::Targetted { address, contents })
                if address.hash().as_ref() == smart_rollup_address =>
            {
                contents
            }
            _ => return InputResult::Unparsable,
        };

        let (transaction_tag, remaining) = parsable!(remaining.split_first());
        // External transactions are only allowed in proxy mode
        match *transaction_tag {
            FORCE_KERNEL_UPGRADE_TAG => Self::Input(Input::ForceKernelUpgrade),
            _ => Mode::parse_external(host, transaction_tag, remaining, context),
        }
    }

    fn parse_simulation(input: &[u8]) -> Self {
        if input.is_empty() {
            InputResult::Simulation
        } else {
            InputResult::Unparsable
        }
    }

    fn parse_fa_deposit<Host: Runtime>(
        host: &mut Host,
        ticket: FA2_1Ticket,
        routing_info: MichelsonBytes,
        inbox_level: u32,
        inbox_msg_id: u32,
        context: &mut Mode::Context,
    ) -> Self {
        // Account for tick at the beginning of the deposit, in case it fails
        // directly. We prefer to overapproximate rather than under approximate.
        Mode::on_fa_deposit(context);
        match FaDeposit::try_parse(ticket, routing_info, inbox_level, inbox_msg_id) {
            Ok((fa_deposit, chain_id)) => {
                log!(host, Debug, "Parsed from input: {}", fa_deposit.display());
                InputResult::Input(Input::FaDeposit((fa_deposit, chain_id)))
            }
            Err(err) => {
                log!(
                    host,
                    Debug,
                    "FA deposit ignored because of parsing errors: {}",
                    err
                );
                InputResult::Unparsable
            }
        }
    }

    fn parse_deposit<Host: Runtime>(
        host: &mut Host,
        ticket: FA2_1Ticket,
        receiver: MichelsonBytes,
        inbox_level: u32,
        inbox_msg_id: u32,
        context: &mut Mode::Context,
    ) -> Self {
        // Account for tick at the beginning of the deposit, in case it fails
        // directly. We prefer to overapproximate rather than under approximate.
        Mode::on_deposit(context);
        match Deposit::try_parse(ticket, receiver, inbox_level, inbox_msg_id) {
            Ok((deposit, chain_id)) => {
                log!(host, Info, "Parsed from input: {}", deposit.display());
                Self::Input(Input::Deposit((deposit, chain_id)))
            }
            Err(err) => {
                log!(
                    host,
                    Info,
                    "Deposit ignored because of parsing errors: {}",
                    err
                );
                Self::Unparsable
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn parse_internal_transfer<Host: Runtime>(
        host: &mut Host,
        transfer: Transfer<RollupType>,
        smart_rollup_address: &[u8],
        tezos_contracts: &TezosContracts,
        context: &mut Mode::Context,
        inbox_level: u32,
        inbox_msg_id: u32,
        enable_fa_deposits: bool,
    ) -> Self {
        if transfer.destination.hash().as_ref() != smart_rollup_address {
            log!(
                host,
                Info,
                "Deposit ignored because of different smart rollup address"
            );
            return InputResult::Unparsable;
        }

        let source = transfer.sender;

        match transfer.payload {
            MichelsonOr::Left(left) => match left {
                MichelsonOr::Left(MichelsonPair(receiver, ticket)) => {
                    match &ticket.creator().0 {
                        Contract::Originated(kt1) => {
                            if Some(kt1) == tezos_contracts.ticketer.as_ref() {
                                Self::parse_deposit(
                                    host,
                                    ticket,
                                    receiver,
                                    inbox_level,
                                    inbox_msg_id,
                                    context,
                                )
                            } else if enable_fa_deposits {
                                Self::parse_fa_deposit(
                                    host,
                                    ticket,
                                    receiver,
                                    inbox_level,
                                    inbox_msg_id,
                                    context,
                                )
                            } else {
                                log!(
                                    host,
                                    Info,
                                    "FA deposit ignored because the feature is disabled"
                                );
                                InputResult::Unparsable
                            }
                        }
                        _ => {
                            log!(
                                host,
                                Info,
                                "Deposit ignored because of invalid ticketer"
                            );
                            InputResult::Unparsable
                        }
                    }
                }
                MichelsonOr::Right(MichelsonBytes(bytes)) => {
                    Mode::parse_internal_bytes(source, &bytes, context)
                }
            },
            MichelsonOr::Right(MichelsonBytes(bytes)) => {
                if tezos_contracts.is_admin(&source) {
                    match Self::parse_kernel_upgrade(&bytes) {
                        InputResult::Unparsable => Self::parse_sequencer_update(&bytes),
                        input => input,
                    }
                } else if tezos_contracts.is_kernel_governance(&source)
                    || tezos_contracts.is_kernel_security_governance(&source)
                {
                    Self::parse_kernel_upgrade(&bytes)
                } else if tezos_contracts.is_sequencer_governance(&source) {
                    Self::parse_sequencer_update(&bytes)
                } else {
                    Self::Unparsable
                }
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn parse_internal<Host: Runtime>(
        host: &mut Host,
        message: InternalInboxMessage<RollupType>,
        smart_rollup_address: &[u8],
        tezos_contracts: &TezosContracts,
        context: &mut Mode::Context,
        level: u32,
        msg_id: u32,
        enable_fa_deposits: bool,
    ) -> Self {
        match message {
            InternalInboxMessage::InfoPerLevel(info) => {
                InputResult::Input(Input::Info(LevelWithInfo { level, info }))
            }
            InternalInboxMessage::Transfer(transfer) => Self::parse_internal_transfer(
                host,
                transfer,
                smart_rollup_address,
                tezos_contracts,
                context,
                level,
                msg_id,
                enable_fa_deposits,
            ),
            InternalInboxMessage::EndOfLevel => InputResult::NoInput,
            InternalInboxMessage::DalAttestedSlots(dal_attested) => {
                // Extract slot indices only from whitelisted publishers.
                // Each PublisherSlots contains a Zarith bitset where bit i is
                // set if slot index i is attested.
                let whitelist = Mode::dal_publishers_whitelist(context);

                // Count total published slots before filtering
                let total_published_slots: usize = dal_attested
                    .slots_by_publisher
                    .iter()
                    .map(|publisher_slots| {
                        (0..dal_attested.number_of_slots)
                            .filter(|&i| publisher_slots.slots_bitset.0.bit(i as u64))
                            .count()
                    })
                    .sum();

                let mut rejected_publishers = Vec::new();
                let slot_indices: Vec<u8> = dal_attested
                    .slots_by_publisher
                    .iter()
                    .filter(|publisher_slots| {
                        let is_whitelisted =
                            whitelist.contains(&publisher_slots.publisher);
                        if !is_whitelisted {
                            rejected_publishers
                                .push(publisher_slots.publisher.to_b58check());
                        }
                        is_whitelisted
                    })
                    .flat_map(|publisher_slots| {
                        (0..dal_attested.number_of_slots)
                            .filter(|&i| publisher_slots.slots_bitset.0.bit(i as u64))
                            .map(|i| i as u8)
                    })
                    .collect();

                // Log summary
                if rejected_publishers.is_empty() {
                    log!(
                        host,
                        Debug,
                        "For published level {}, accepted {}/{} DAL slots from {} publishers",
                        dal_attested.published_level,
                        slot_indices.len(),
                        total_published_slots,
                        dal_attested.slots_by_publisher.len()
                    );
                } else {
                    log!(
                        host,
                        Debug,
                        "For published level {}, accepted {}/{} DAL slots; not whitelisted: {}",
                        dal_attested.published_level,
                        slot_indices.len(),
                        total_published_slots,
                        rejected_publishers.join(", ")
                    );
                }

                InputResult::Input(Input::DalAttestedSlots {
                    published_level: dal_attested.published_level,
                    slot_size: dal_attested.slot_size as u64,
                    page_size: dal_attested.page_size as u64,
                    slot_indices,
                })
            }
            _ => InputResult::Unparsable,
        }
    }

    pub fn parse<Host: Runtime>(
        host: &mut Host,
        input: Message,
        smart_rollup_address: [u8; 20],
        tezos_contracts: &TezosContracts,
        context: &mut Mode::Context,
        enable_fa_deposits: bool,
    ) -> Self {
        let bytes = Message::as_ref(&input);
        let (input_tag, remaining) = parsable!(bytes.split_first());
        if *input_tag == SIMULATION_TAG {
            return Self::parse_simulation(remaining);
        };

        match InboxMessage::<RollupType>::parse(bytes) {
            Ok((_remaing, message)) => match message {
                InboxMessage::External(message) => {
                    Self::parse_external(host, message, &smart_rollup_address, context)
                }
                InboxMessage::Internal(message) => Self::parse_internal(
                    host,
                    message,
                    &smart_rollup_address,
                    tezos_contracts,
                    context,
                    input.level,
                    input.id,
                    enable_fa_deposits,
                ),
            },
            Err(_) => InputResult::Unparsable,
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use tezos_crypto_rs::hash::SecretKeyEd25519;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_host::input::Message;

    const ZERO_SMART_ROLLUP_ADDRESS: [u8; 20] = [0; 20];

    #[test]
    fn parse_unparsable_transaction() {
        let mut host = MockKernelHost::default();

        let message = Message::new(0, 0, vec![1, 9, 32, 58, 59, 30]);
        assert_eq!(
            InputResult::<ProxyInput>::parse(
                &mut host,
                message,
                ZERO_SMART_ROLLUP_ADDRESS,
                &TezosContracts {
                    ticketer: None,
                    admin: None,
                    sequencer_governance: None,
                    kernel_governance: None,
                    kernel_security_governance: None,
                },
                &mut (),
                false,
            ),
            InputResult::Unparsable
        )
    }

    pub fn sequencer_signed_blueprint_chunk_bytes(
        unsigned_blueprint: &UnsignedSequencerBlueprint,
        sk: SecretKeyEd25519,
    ) -> Vec<u8> {
        let unsigned_blueprint_bytes = unsigned_blueprint.rlp_bytes();
        let blueprint_signature = sk.sign(unsigned_blueprint_bytes).unwrap();
        let blueprint = crate::sequencer_blueprint::SequencerBlueprint {
            blueprint: unsigned_blueprint.clone(),
            signature: blueprint_signature.into(),
        };
        blueprint.rlp_bytes().into()
    }

    #[test]
    fn test_parsing_blueprint_chunk() {
        let pk = PublicKey::from_b58check(
            "edpkv4NmL2YPe8eiVGXUDXmPQybD725ofKirTzGRxs1X9UmaG3voKw",
        )
        .unwrap();
        let sk = SecretKeyEd25519::from_base58_check(
            "edsk422LGdmDnai4Cya6csM6oFmgHpDQKUhatTURJRAY4h7NHNz9sz",
        )
        .unwrap();
        let next_blueprint_number: U256 = 7.into();

        let blueprint = UnsignedSequencerBlueprint {
            chunk: vec![],
            number: 7.into(),
            nb_chunks: 3,
            chunk_index: 0,
            chain_id: None,
        };

        // Blueprint chunk for level 7 is ok.
        let res = parse_blueprint_chunk(
            &sequencer_signed_blueprint_chunk_bytes(&blueprint, sk.clone()),
            &pk,
            &next_blueprint_number,
        );
        assert!(matches!(res, SequencerBlueprintRes::SequencerBlueprint(_)));

        let invalid_sk = SecretKeyEd25519::from_base58_check(
            "edsk37VEgDUMt7wje8vxfao55y7JhiamjTbVM1xABSCamFgtcUqhdT",
        )
        .unwrap();
        let res = parse_blueprint_chunk(
            &sequencer_signed_blueprint_chunk_bytes(&blueprint, invalid_sk),
            &pk,
            &next_blueprint_number,
        );
        assert!(matches!(res, SequencerBlueprintRes::InvalidSignature));

        let res = parse_blueprint_chunk(
            &sequencer_signed_blueprint_chunk_bytes(
                &UnsignedSequencerBlueprint {
                    number: 5.into(),
                    ..blueprint.clone()
                },
                sk.clone(),
            ),
            &pk,
            &next_blueprint_number,
        );
        assert!(matches!(res, SequencerBlueprintRes::InvalidNumber));

        let res = parse_blueprint_chunk(
            &sequencer_signed_blueprint_chunk_bytes(
                &UnsignedSequencerBlueprint {
                    number: 6.into(),
                    ..blueprint
                },
                sk.clone(),
            ),
            &pk,
            &next_blueprint_number,
        );
        assert!(matches!(res, SequencerBlueprintRes::InvalidNumber));
    }
}

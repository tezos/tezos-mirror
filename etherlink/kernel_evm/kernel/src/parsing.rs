// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    inbox::{Deposit, Transaction, TransactionContent},
    sequencer_blueprint::{SequencerBlueprint, UnsignedSequencerBlueprint},
    upgrade::KernelUpgrade,
};
use primitive_types::{H160, U256};
use rlp::Encodable;
use sha3::{Digest, Keccak256};
use tezos_crypto_rs::{hash::ContractKt1Hash, PublicKeySignatureVerifier};
use tezos_ethereum::{
    rlp_helpers::FromRlpBytes,
    transaction::{TransactionHash, TRANSACTION_HASH_SIZE},
    tx_common::EthereumTransactionCommon,
    wei::eth_from_mutez,
};
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_encoding::{
    contract::Contract,
    inbox::{
        ExternalMessageFrame, InboxMessage, InfoPerLevel, InternalInboxMessage, Transfer,
    },
    michelson::{ticket::FA2_1Ticket, MichelsonBytes, MichelsonOr, MichelsonPair},
    public_key::PublicKey,
};
use tezos_smart_rollup_host::input::Message;
use tezos_smart_rollup_host::runtime::Runtime;

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

pub const MAX_SIZE_PER_CHUNK: usize = 4095 // Max input size minus external tag
            - 1 // ExternalMessageFrame tag
            - 20 // Smart rollup address size (ExternalMessageFrame::Targetted)
            - 1  // Transaction chunk tag
            - 2  // Number of chunks (u16)
            - 32 // Transaction hash size
            - 32; // Chunk hash size

#[derive(Debug, PartialEq, Clone)]
pub enum Input {
    SimpleTransaction(Box<Transaction>),
    Deposit(Deposit),
    Upgrade(KernelUpgrade),
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
    Simulation,
    Info(InfoPerLevel),
    SequencerBlueprint(SequencerBlueprint),
}

#[derive(Debug, PartialEq, Default)]
pub enum InputResult {
    /// No further inputs
    NoInput,
    /// Some decoded input
    Input(Input),
    #[default]
    /// Unparsable input, to be ignored
    Unparsable,
}

pub type RollupType = MichelsonOr<
    MichelsonOr<MichelsonPair<MichelsonBytes, FA2_1Ticket>, MichelsonBytes>,
    MichelsonBytes,
>;

impl InputResult {
    fn parse_simple_transaction(bytes: &[u8]) -> Self {
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
        InputResult::Input(Input::SimpleTransaction(Box::new(Transaction {
            tx_hash,
            content: TransactionContent::Ethereum(tx),
        })))
    }

    fn parse_new_chunked_transaction(bytes: &[u8]) -> Self {
        // Next 32 bytes is the transaction hash.
        let (tx_hash, remaining) = parsable!(split_at(bytes, TRANSACTION_HASH_SIZE));
        let tx_hash: TransactionHash = parsable!(tx_hash.try_into().ok());
        // Next 2 bytes is the number of chunks.
        let (num_chunks, remaining) = parsable!(split_at(remaining, 2));
        let num_chunks = u16::from_le_bytes(num_chunks.try_into().unwrap());
        if remaining.len() != (TRANSACTION_HASH_SIZE * usize::from(num_chunks)) {
            return Self::Unparsable;
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
        Self::Input(Input::NewChunkedTransaction {
            tx_hash,
            num_chunks,
            chunk_hashes,
        })
    }

    fn parse_transaction_chunk(bytes: &[u8]) -> Self {
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
            return Self::Unparsable;
        }
        Self::Input(Input::TransactionChunk {
            tx_hash,
            i,
            chunk_hash,
            data: remaining.to_vec(),
        })
    }

    fn parse_kernel_upgrade(
        source: ContractKt1Hash,
        admin: &Option<ContractKt1Hash>,
        bytes: &[u8],
    ) -> Self {
        // Consider only upgrades from the bridge contract.
        if admin.is_none() || &source != admin.as_ref().unwrap() {
            return Self::Unparsable;
        }

        let kernel_upgrade = parsable!(KernelUpgrade::from_rlp_bytes(bytes).ok());
        Self::Input(Input::Upgrade(kernel_upgrade))
    }

    fn parse_sequencer_blueprint_input(sequencer: &PublicKey, bytes: &[u8]) -> Self {
        // Parse the sequencer blueprint
        let seq_blueprint: SequencerBlueprint =
            parsable!(FromRlpBytes::from_rlp_bytes(bytes).ok());

        // Creates and encodes the unsigned blueprint:
        let unsigned_seq_blueprint: UnsignedSequencerBlueprint = (&seq_blueprint).into();
        let bytes = unsigned_seq_blueprint.rlp_bytes().to_vec();
        // The sequencer signs the hash of the blueprint.
        let msg = tezos_crypto_rs::blake2b::digest_256(&bytes).unwrap();

        let correctly_signed = sequencer
            .verify_signature(&seq_blueprint.signature, &msg)
            .unwrap_or(false);

        if correctly_signed {
            InputResult::Input(Input::SequencerBlueprint(seq_blueprint))
        } else {
            InputResult::Unparsable
        }
    }

    /// Parses transactions that come from the delayed inbox.
    fn parse_transaction_from_delayed_inbox(
        source: ContractKt1Hash,
        delayed_bridge: &Option<ContractKt1Hash>,
        bytes: &[u8],
    ) -> Self {
        match delayed_bridge {
            Some(delayed_bridge) if delayed_bridge.as_ref() == source.as_ref() => (),
            _ => {
                return InputResult::Unparsable;
            }
        };
        let tx = parsable!(EthereumTransactionCommon::from_bytes(bytes).ok());
        let tx_hash: TransactionHash = Keccak256::digest(bytes).into();

        Self::Input(Input::SimpleTransaction(Box::new(Transaction {
            tx_hash,
            content: TransactionContent::Ethereum(tx),
        })))
    }

    /// Parses an external message
    ///
    // External message structure :
    // EXTERNAL_TAG 1B / FRAMING_PROTOCOL_TARGETTED 21B / MESSAGE_TAG 1B / DATA
    fn parse_external(
        input: &[u8],
        smart_rollup_address: &[u8],
        sequencer: &Option<PublicKey>,
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
        match *transaction_tag {
            SIMPLE_TRANSACTION_TAG => Self::parse_simple_transaction(remaining),
            NEW_CHUNKED_TRANSACTION_TAG => Self::parse_new_chunked_transaction(remaining),
            TRANSACTION_CHUNK_TAG => Self::parse_transaction_chunk(remaining),
            SEQUENCER_BLUEPRINT_TAG if sequencer.is_some() => {
                Self::parse_sequencer_blueprint_input(
                    sequencer.as_ref().unwrap(),
                    remaining,
                )
            }
            _ => InputResult::Unparsable,
        }
    }

    fn parse_simulation(input: &[u8]) -> Self {
        if input.is_empty() {
            InputResult::Input(Input::Simulation)
        } else {
            InputResult::Unparsable
        }
    }

    fn parse_deposit<Host: Runtime>(
        host: &mut Host,
        ticket: FA2_1Ticket,
        receiver: MichelsonBytes,
        ticketer: &Option<ContractKt1Hash>,
    ) -> Self {
        match &ticket.creator().0 {
            Contract::Originated(kt1) if Some(kt1) == ticketer.as_ref() => (),
            _ => {
                log!(host, Info, "Deposit ignored because of different ticketer");
                return InputResult::Unparsable;
            }
        };

        // Amount
        let (_sign, amount_bytes) = ticket.amount().to_bytes_le();
        // We use the `U256::from_little_endian` as it takes arbitrary long
        // bytes. Afterward it's transform to `u64` to use `eth_from_mutez`, it's
        // obviously safe as we deposit CTEZ and the amount is limited by
        // the XTZ quantity.
        let amount: u64 = U256::from_little_endian(&amount_bytes).as_u64();
        let amount: U256 = eth_from_mutez(amount);

        // EVM address
        let receiver_bytes = receiver.0;
        if receiver_bytes.len() != std::mem::size_of::<H160>() {
            log!(
                host,
                Info,
                "Deposit ignored because of invalid receiver address"
            );
            return InputResult::Unparsable;
        }
        let receiver = H160::from_slice(&receiver_bytes);

        let content = Deposit { amount, receiver };
        log!(host, Info, "Deposit of {} to {}.", amount, receiver);
        Self::Input(Input::Deposit(content))
    }

    fn parse_internal_transfer<Host: Runtime>(
        host: &mut Host,
        transfer: Transfer<RollupType>,
        smart_rollup_address: &[u8],
        ticketer: &Option<ContractKt1Hash>,
        admin: &Option<ContractKt1Hash>,
        delayed_bridge: &Option<ContractKt1Hash>,
    ) -> Self {
        if transfer.destination.hash().0 != smart_rollup_address {
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
                    Self::parse_deposit(host, ticket, receiver, ticketer)
                }
                MichelsonOr::Right(MichelsonBytes(bytes)) => {
                    Self::parse_transaction_from_delayed_inbox(
                        source,
                        delayed_bridge,
                        &bytes,
                    )
                }
            },
            MichelsonOr::Right(MichelsonBytes(upgrade)) => {
                Self::parse_kernel_upgrade(source, admin, &upgrade)
            }
        }
    }

    fn parse_internal<Host: Runtime>(
        host: &mut Host,
        message: InternalInboxMessage<RollupType>,
        smart_rollup_address: &[u8],
        ticketer: &Option<ContractKt1Hash>,
        admin: &Option<ContractKt1Hash>,
        delayed_bridge: &Option<ContractKt1Hash>,
    ) -> Self {
        match message {
            InternalInboxMessage::InfoPerLevel(info) => {
                InputResult::Input(Input::Info(info))
            }
            InternalInboxMessage::Transfer(transfer) => Self::parse_internal_transfer(
                host,
                transfer,
                smart_rollup_address,
                ticketer,
                admin,
                delayed_bridge,
            ),
            _ => InputResult::Unparsable,
        }
    }

    pub fn parse<Host: Runtime>(
        host: &mut Host,
        input: Message,
        smart_rollup_address: [u8; 20],
        ticketer: &Option<ContractKt1Hash>,
        admin: &Option<ContractKt1Hash>,
        delayed_bridge: &Option<ContractKt1Hash>,
        sequencer: &Option<PublicKey>,
    ) -> Self {
        let bytes = Message::as_ref(&input);
        let (input_tag, remaining) = parsable!(bytes.split_first());
        if *input_tag == SIMULATION_TAG {
            return Self::parse_simulation(remaining);
        };

        match InboxMessage::<RollupType>::parse(bytes) {
            Ok((_remaing, message)) => match message {
                InboxMessage::External(message) => {
                    Self::parse_external(message, &smart_rollup_address, sequencer)
                }
                InboxMessage::Internal(message) => Self::parse_internal(
                    host,
                    message,
                    &smart_rollup_address,
                    ticketer,
                    admin,
                    delayed_bridge,
                ),
            },
            Err(_) => InputResult::Unparsable,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_smart_rollup_host::input::Message;
    use tezos_smart_rollup_mock::MockHost;

    const ZERO_SMART_ROLLUP_ADDRESS: [u8; 20] = [0; 20];

    #[test]
    fn parse_unparsable_transaction() {
        let mut host = MockHost::default();

        let message = Message::new(0, 0, vec![1, 9, 32, 58, 59, 30]);
        assert_eq!(
            InputResult::parse(
                &mut host,
                message,
                ZERO_SMART_ROLLUP_ADDRESS,
                &None,
                &None,
                &None,
                &None
            ),
            InputResult::Unparsable
        )
    }
}

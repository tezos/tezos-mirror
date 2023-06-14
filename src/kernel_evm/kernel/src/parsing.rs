// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::inbox::{Deposit, KernelUpgrade, Transaction, TransactionContent};
use primitive_types::{H160, U256};
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_ethereum::{
    signatures::EthereumTransactionCommon, transaction::TransactionHash,
    wei::eth_from_mutez,
};
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_debug::{debug_msg, Runtime};
use tezos_smart_rollup_encoding::{
    contract::Contract,
    inbox::{InboxMessage, InfoPerLevel, InternalInboxMessage, Transfer},
    michelson::{ticket::UnitTicket, MichelsonBytes, MichelsonInt, MichelsonPair},
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

const KERNEL_UPGRADE_TAG: u8 = 3;

pub const MAX_SIZE_PER_CHUNK: usize = 4095 // Max input size minus external tag
            - 20 // Smart rollup address size
            - 1  // Transaction chunk tag
            - 2  // Number of chunks (u16)
            - 32; // Transaction hash size

pub const SIGNATURE_HASH_SIZE: usize = 64;

pub const UPGRADE_NONCE_SIZE: usize = 2;

#[derive(Debug, PartialEq)]
pub enum Input {
    SimpleTransaction(Box<Transaction>),
    Deposit(Deposit),
    Upgrade(KernelUpgrade),
    NewChunkedTransaction {
        tx_hash: TransactionHash,
        num_chunks: u16,
    },
    TransactionChunk {
        tx_hash: TransactionHash,
        i: u16,
        data: Vec<u8>,
    },
    Simulation,
    Info(InfoPerLevel),
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

type RollupType = MichelsonPair<MichelsonPair<MichelsonBytes, UnitTicket>, MichelsonInt>;

impl InputResult {
    fn parse_simple_transaction(bytes: &[u8]) -> Self {
        // Next 32 bytes is the transaction hash.
        let (tx_hash, remaining) = parsable!(split_at(bytes, 32));
        let tx_hash: TransactionHash = parsable!(tx_hash.try_into().ok()); // Remaining bytes is the rlp encoded transaction.
        let tx: EthereumTransactionCommon = parsable!(remaining.try_into().ok());
        InputResult::Input(Input::SimpleTransaction(Box::new(Transaction {
            tx_hash,
            content: TransactionContent::Ethereum(tx),
        })))
    }

    fn parse_new_chunked_transaction(bytes: &[u8]) -> Self {
        // Next 32 bytes is the transaction hash.
        let (tx_hash, remaining) = parsable!(split_at(bytes, 32));
        let tx_hash: TransactionHash = parsable!(tx_hash.try_into().ok());
        // Next 2 bytes is the number of chunks.
        let (num_chunks, remaining) = parsable!(split_at(remaining, 2));
        let num_chunks = u16::from_le_bytes(num_chunks.try_into().unwrap());
        if remaining.is_empty() {
            Self::Input(Input::NewChunkedTransaction {
                tx_hash,
                num_chunks,
            })
        } else {
            Self::Unparsable
        }
    }

    fn parse_transaction_chunk(bytes: &[u8]) -> Self {
        // Next 32 bytes is the transaction hash.
        let (tx_hash, remaining) = parsable!(split_at(bytes, 32));
        let tx_hash: TransactionHash = parsable!(tx_hash.try_into().ok());
        // Next 2 bytes is the index.
        let (i, remaining) = parsable!(split_at(remaining, 2));
        let i = u16::from_le_bytes(i.try_into().unwrap());
        Self::Input(Input::TransactionChunk {
            tx_hash,
            i,
            data: remaining.to_vec(),
        })
    }

    fn parse_kernel_upgrade(bytes: &[u8]) -> Self {
        // Next UPGRADE_NONCE_SIZE bytes is the incoming kernel upgrade nonce
        let (nonce, remaining) = parsable!(split_at(bytes, UPGRADE_NONCE_SIZE));
        let nonce: [u8; UPGRADE_NONCE_SIZE] = parsable!(nonce.try_into().ok());
        // Next PREIMAGE_HASH_SIZE bytes is the preimage hash
        let (preimage_hash, remaining) =
            parsable!(split_at(remaining, PREIMAGE_HASH_SIZE));
        let preimage_hash: [u8; PREIMAGE_HASH_SIZE] =
            parsable!(preimage_hash.try_into().ok());
        // Next SIGNATURE_HASH_SIZE bytes is the preimage hash
        let (signature, remaining) = parsable!(split_at(remaining, SIGNATURE_HASH_SIZE));
        let signature: [u8; SIGNATURE_HASH_SIZE] = parsable!(signature.try_into().ok());
        if remaining.is_empty() {
            Self::Input(Input::Upgrade(KernelUpgrade {
                nonce,
                preimage_hash,
                signature,
            }))
        } else {
            Self::Unparsable
        }
    }

    // External message structure :
    // EXTERNAL_TAG 1B / ROLLUP_ADDRESS 20B / MESSAGE_TAG 1B / DATA
    fn parse_external(input: &[u8], smart_rollup_address: &[u8]) -> Self {
        // Next 20 bytes is the targeted smart rollup address.
        let remaining = {
            let (target_smart_rollup_address, remaining) = parsable!(split_at(input, 20));

            if target_smart_rollup_address == smart_rollup_address {
                remaining
            } else {
                return InputResult::Unparsable;
            }
        };
        let (transaction_tag, remaining) = parsable!(remaining.split_first());
        match *transaction_tag {
            SIMPLE_TRANSACTION_TAG => Self::parse_simple_transaction(remaining),
            NEW_CHUNKED_TRANSACTION_TAG => Self::parse_new_chunked_transaction(remaining),
            TRANSACTION_CHUNK_TAG => Self::parse_transaction_chunk(remaining),
            KERNEL_UPGRADE_TAG => Self::parse_kernel_upgrade(remaining),
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
        transfer: Transfer<RollupType>,
        smart_rollup_address: &[u8],
        ticketer: &ContractKt1Hash,
    ) -> Self {
        if transfer.destination.hash().0 != smart_rollup_address {
            debug_msg!(
                host,
                "Deposit ignored because of different smart rollup address\n"
            );
            return InputResult::Unparsable;
        }

        let ticket = transfer.payload.0 .1;

        match &ticket.creator().0 {
            Contract::Originated(kt1) if kt1 == ticketer => (),
            _ => {
                debug_msg!(host, "Deposit ignored because of different ticketer\n");
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

        // Amount for gas
        let gas_price: MichelsonInt = transfer.payload.1;
        let (_sign, gas_price_bytes) = gas_price.0 .0.to_bytes_le();
        let gas_price: U256 = U256::from_little_endian(&gas_price_bytes);

        // EVM address
        let receiver_bytes: Vec<u8> = transfer.payload.0 .0 .0;
        if receiver_bytes.len() != std::mem::size_of::<H160>() {
            debug_msg!(
                host,
                "Deposit ignored because of invalid receiver address\n"
            );
            return InputResult::Unparsable;
        }
        let receiver = H160::from_slice(&receiver_bytes);

        let content = Deposit {
            amount,
            gas_price,
            receiver,
        };
        debug_msg!(
            host,
            "Deposit of {} to {} with gas price {}\n",
            amount,
            receiver,
            gas_price
        );
        Self::Input(Input::Deposit(content))
    }

    pub fn parse<Host: Runtime>(
        host: &mut Host,
        input: Message,
        smart_rollup_address: [u8; 20],
        l1_bridge: &Option<ContractKt1Hash>,
    ) -> Self {
        let bytes = Message::as_ref(&input);
        let (input_tag, remaining) = parsable!(bytes.split_first());
        if *input_tag == SIMULATION_TAG {
            return Self::parse_simulation(remaining);
        };

        match InboxMessage::<RollupType>::parse(bytes) {
            Ok((_remaing, message)) => match message {
                InboxMessage::External(message) => {
                    Self::parse_external(message, &smart_rollup_address)
                }
                InboxMessage::Internal(InternalInboxMessage::InfoPerLevel(info)) => {
                    InputResult::Input(Input::Info(info))
                }
                InboxMessage::Internal(InternalInboxMessage::Transfer(transfer))
                    if l1_bridge.is_some() =>
                {
                    Self::parse_deposit(
                        host,
                        transfer,
                        &smart_rollup_address,
                        l1_bridge.as_ref().unwrap(),
                    )
                }
                InboxMessage::Internal(_) => InputResult::Unparsable,
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
            InputResult::parse(&mut host, message, ZERO_SMART_ROLLUP_ADDRESS, &None),
            InputResult::Unparsable
        )
    }
}

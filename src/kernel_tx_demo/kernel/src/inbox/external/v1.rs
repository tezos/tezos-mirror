// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! V1 of external inbox message parsed into a kernel-specific data structure.
//!
//! Specifically, v1 allows a user to [transfer] & [withdraw].
//!
//! [transfer]: OperationTransfer
//! [withdraw]: OperationWithdraw

use self::verifiable::TransactionError;

use super::Signer;
use crypto::hash::TryFromPKError;
use crypto::hash::{ContractKt1Hash, ContractTz1Hash};
use crypto::CryptoError;
use nom::combinator::map;
use nom::multi::many0;
use tezos_crypto_rs::blake2b::Blake2bError;
use tezos_data_encoding::enc::{BinError, BinWriter};
use tezos_data_encoding::encoding::Encoding;
use tezos_data_encoding::encoding::HasEncoding;
use tezos_data_encoding::nom::{dynamic, NomReader};
use tezos_smart_rollup_encoding::entrypoint::Entrypoint;
use tezos_smart_rollup_encoding::michelson::ticket::{StringTicket, TicketHash};
use thiserror::Error;
use verifiable::VerifiableOperation;

pub mod sendable;
#[cfg(feature = "testing")]
pub mod testing;
pub mod verifiable;

/// Withdraws the ticket from the signer's account.
///
/// Ticket itself contains both the `creator`, `content` & `quantity`.
/// The withdrawal may be of quantity up to or equal the quantity of the
/// ticket that exists within the account.
///
/// The ticket may then be withdrawable on layer-1 by the account given by
/// `destination`.
#[derive(Debug, PartialEq, Eq, HasEncoding, NomReader, BinWriter)]
pub struct OperationWithdraw {
    destination: ContractKt1Hash,
    ticket: StringTicket,
    entrypoint: Entrypoint,
}

/// Transfers the ticket from the signer's account, to the layer-2 `destination`.
///
/// Ticket is a hash of the 'contents & creator'.
///
/// The transfer may be of quantity up to or equal the quantity of the
/// ticket that exists within the account.
#[derive(Debug, PartialEq, Eq, HasEncoding, NomReader, BinWriter)]
pub struct OperationTransfer {
    destination: ContractTz1Hash,
    ticket: TicketHash,
    amount: TicketAmount,
}

/// Transfers the ticket from the signer's account, to the layer-2 `destination`.
///
/// Ticket is an index corresponding to the hash of the 'contents & creator'.
///
/// The transfer may be of quantity up to or equal the quantity of the
/// ticket that exists within the account.
#[derive(Debug, PartialEq, Eq, HasEncoding, NomReader, BinWriter)]
pub struct OperationTransferCompressed {
    destination: ContractTz1Hash,
    ticket: TicketIndex,
    amount: TicketAmount,
}

/// A serialised data for ticket amount
#[derive(Debug, PartialEq, Eq)]
pub struct TicketAmount {
    amount: u64,
}

/// A serialised data for ticket amount
#[derive(Debug, PartialEq, Eq)]
pub struct TicketIndex {
    index: u64,
}

impl HasEncoding for TicketAmount {
    fn encoding() -> Encoding {
        Encoding::Custom
    }
}

impl NomReader for TicketAmount {
    fn nom_read(input: &[u8]) -> tezos_data_encoding::nom::NomResult<Self> {
        nom::combinator::map(nom::number::complete::le_u64, |amount| Self { amount })(input)
    }
}

impl BinWriter for TicketAmount {
    fn bin_write(&self, output: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        output.extend_from_slice(&self.amount.to_le_bytes());
        Ok(())
    }
}

impl HasEncoding for TicketIndex {
    fn encoding() -> Encoding {
        Encoding::Custom
    }
}

impl NomReader for TicketIndex {
    fn nom_read(input: &[u8]) -> tezos_data_encoding::nom::NomResult<Self> {
        nom::combinator::map(nom::number::complete::le_u64, |index| Self { index })(input)
    }
}

impl BinWriter for TicketIndex {
    fn bin_write(&self, output: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        output.extend_from_slice(&self.index.to_le_bytes());
        Ok(())
    }
}

/// An operation either to withdraw or transfer a ticket.
#[derive(Debug, PartialEq, Eq, HasEncoding, NomReader, BinWriter)]
pub enum OperationContent {
    /// Withdraw a ticket from the signer account.
    Withdraw(OperationWithdraw),
    /// Transfer a ticket from the signer account to another Layer-2 account.
    Transfer(OperationTransfer),
    /// Compressed transfer
    CTransfer(OperationTransferCompressed),
}

impl OperationContent {
    /// Create a new withdrawal operation.
    pub fn withdrawal(
        destination: ContractKt1Hash,
        ticket: impl Into<StringTicket>,
        entrypoint: Entrypoint,
    ) -> OperationContent {
        OperationContent::Withdraw(OperationWithdraw {
            destination,
            entrypoint,
            ticket: ticket.into(),
        })
    }

    /// Create a new transfer operation.
    pub fn transfer(
        destination: ContractTz1Hash,
        ticket: TicketHash,
        amount: u64,
    ) -> Result<OperationContent, TransactionError> {
        if amount > 0 {
            Ok(OperationContent::Transfer(OperationTransfer {
                destination,
                ticket,
                amount: TicketAmount { amount },
            }))
        } else {
            Err(TransactionError::ZeroTicket)
        }
    }

    /// Create a new transfer operation.
    pub fn compressed_transfer(
        destination: ContractTz1Hash,
        ticket: u64,
        amount: u64,
    ) -> Result<OperationContent, TransactionError> {
        if amount > 0 {
            Ok(OperationContent::CTransfer(OperationTransferCompressed {
                destination,
                ticket: TicketIndex { index: ticket },
                amount: TicketAmount { amount },
            }))
        } else {
            Err(TransactionError::ZeroTicket)
        }
    }
}

/// A group of operations, operating on the account given by `signer`.
#[derive(Debug, PartialEq, Eq, HasEncoding, NomReader, BinWriter)]
pub struct Operation {
    /// The signer of this operation, who's account this will act against.
    pub signer: Signer,
    /// The counter of this operation - must match the signer's current counter.
    pub counter: i64,
    /// The individual actions contained within this operation.
    pub contents: OperationContent,
}

/// Errors occurring when signing or serializing operations, transactions and batches.
#[derive(Debug, Error)]
pub enum ToBytesError {
    /// Bls signing/aggregation error
    #[error("Bls error in signatures {0}")]
    Bls(#[from] CryptoError),
    /// Unable to convert pk to hash.
    #[error("Failed to convert pk to hash: {0}")]
    FromPk(#[from] TryFromPKError),
    /// Serialization error
    #[error("Unable to serialize operation: {0}")]
    Binary(#[from] BinError),
    /// Digest error
    #[error("Unable to digest operation: {0}")]
    Digest(#[from] Blake2bError),
    /// Incorrect signing key
    #[error("BlsKey did not match Signer")]
    IncorrectKey,
}

/// A batch of operations, associated with an aggregated signature.
#[derive(Debug, PartialEq, Eq)]
pub struct ParsedBatch<'a> {
    /// List of transactions, to be applied in order.
    pub operations: Vec<VerifiableOperation<'a>>,
}

impl<'a> ParsedBatch<'a> {
    /// Parse a batch, where each transaction is *verifiable*.
    pub fn parse(input: &'a [u8]) -> tezos_data_encoding::nom::NomResult<Self> {
        map(dynamic(many0(VerifiableOperation::parse)), |operations| {
            ParsedBatch { operations }
        })(input)
    }
}

#[cfg(test)]
mod test {
    use proptest::prelude::*;
    use tezos_data_encoding::enc::BinWriter;
    use tezos_data_encoding::nom::NomReader;

    use super::OperationContent;

    proptest! {
        #[test]
        fn operation_withdraw_encode_decode(
            operation in OperationContent::arb_withdrawal(),
            remaining_input in any::<Vec<u8>>(),
        ) {
            let mut encoded = Vec::new();
            operation
                .bin_write(&mut encoded)
                .expect("encoding withdraw operation should work");
            encoded.extend_from_slice(remaining_input.as_slice());

            let (remaining, decoded) = OperationContent::nom_read(encoded.as_slice())
                .expect("decoding withdraw operation should work");

            assert_eq!(remaining, remaining_input.as_slice());

            assert_eq!(operation, decoded);
        }

        #[test]
        fn operation_transfer_encode_decode(
            operation in OperationContent::arb_transfer(),
            remaining_input in any::<Vec<u8>>(),
        ) {
            let mut encoded = Vec::new();
            operation
                .bin_write(&mut encoded)
                .expect("encoding transfer operation should work");
            encoded.extend_from_slice(remaining_input.as_slice());

            let (remaining, decoded) = OperationContent::nom_read(encoded.as_slice())
                .expect("decoding transfer operation should work");

            assert_eq!(remaining, remaining_input.as_slice());

            assert_eq!(operation, decoded);
        }
    }
}

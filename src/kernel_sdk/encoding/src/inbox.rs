// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Types & encodings for the *inbox-half* of the *L1/L2 communication protocol*
//!
//! In *general*, this module is a re-implementation of the tezos-protocol
//! [inbox message repr].
//!
//! [inbox message repr]: <https://gitlab.com/tezos/tezos/-/blob/9028b797894a5d9db38bc61a20abb793c3778316/src/proto_alpha/lib_protocol/sc_rollup_inbox_message_repr.mli>

use crate::michelson::Michelson;
use crate::public_key_hash::PublicKeyHash;
use crate::smart_rollup::SmartRollupAddress;
use crate::timestamp::Timestamp;
use crypto::hash::{BlockHash, ContractKt1Hash};
use nom::combinator::{map, rest};
use std::fmt::Display;
use tezos_data_encoding::enc::BinWriter;
use tezos_data_encoding::encoding::HasEncoding;
use tezos_data_encoding::nom::NomReader;

#[derive(Debug, PartialEq, Eq, NomReader, HasEncoding, BinWriter)]
enum InboxMessageRepr<Expr: Michelson> {
    #[encoding(tag = 0)]
    Internal(InternalInboxMessage<Expr>),
    #[encoding(tag = 1)]
    External,
}

/// Inbox message, received by the kernel as tezos-encoded bytes.
#[derive(Debug, PartialEq, Eq)]
pub enum InboxMessage<'a, Expr: Michelson> {
    /// Message sent from an L1 smart-contract.
    Internal(InternalInboxMessage<Expr>),
    /// Message of arbitrary bytes, in a format specific to the kernel.
    ///
    /// The containing operation will be sent by an implicit account - but will
    /// deserialize to a structure representing *transactions* & *withdrawals* between
    /// and from **Layer2Tz4* addresses respectively.
    External(&'a [u8]),
}

impl<'a, Expr: Michelson> InboxMessage<'a, Expr> {
    /// Replacement for `nom_read` for [InboxMessage].
    ///
    /// [NomReader] trait unfortunately does not propagate lifetime of the input bytes,
    /// meaning that it is impossible to use it with a type that refers to a section of
    /// the input.
    ///
    /// In our case, we want to avoid copies if possible - which require additional ticks.
    pub fn parse(input: &'a [u8]) -> tezos_data_encoding::nom::NomResult<Self> {
        let (remaining, repr): (&'a [u8], _) = InboxMessageRepr::nom_read(input)?;

        match repr {
            InboxMessageRepr::Internal(i) => Ok((remaining, InboxMessage::Internal(i))),
            InboxMessageRepr::External => map(rest, InboxMessage::External)(remaining),
        }
    }

    /// Replacement for `bin_write` for [InboxMessage].
    ///
    /// [BinWriter] does not allow consumption of the input.
    pub fn serialize(self, output: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        match self {
            InboxMessage::Internal(m) => {
                InboxMessageRepr::Internal(m).bin_write(output)?;
            }
            InboxMessage::External(m) => {
                InboxMessageRepr::<Expr>::External.bin_write(output)?;
                output.extend_from_slice(m);
            }
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, NomReader, HasEncoding, BinWriter)]
/// Transfer sent by an L1 smart-contract.
pub struct Transfer<Expr: Michelson> {
    /// Micheline-encoded payload, sent by the calling contract.
    pub payload: Expr,
    /// The calling smart-contract.
    pub sender: ContractKt1Hash,
    /// The originator of the transaction.
    pub source: PublicKeyHash,
    /// The destination of the message.
    pub destination: SmartRollupAddress,
}

impl<E: Michelson> Display for Transfer<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Transfer {{sender: {}, source: {}, dest: {}}}",
            self.sender, self.source, self.destination
        )
    }
}

/// Metainformation per inbox level, usually goes after StartOfLevel message
#[derive(Debug, PartialEq, Eq, NomReader, HasEncoding, BinWriter, Clone)]
pub struct InfoPerLevel {
    /// Timestamp of predecessor block
    pub predecessor_timestamp: Timestamp,
    /// Hash of predecessor block
    pub predecessor: BlockHash,
}

impl Display for InfoPerLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "InfoPerLevel {{predecessor_timestamp: {}, predecessor: {}}}",
            self.predecessor_timestamp, self.predecessor
        )
    }
}

/// Internal inbox message - known to be sent by the protocol
#[derive(Debug, PartialEq, Eq, NomReader, HasEncoding, BinWriter)]
pub enum InternalInboxMessage<Expr: Michelson> {
    /// Transfer message
    Transfer(Transfer<Expr>),
    /// Start of level message, pushed at the beginning of an inbox level.
    StartOfLevel,
    /// End of level message, pushed at the end of an inbox level.
    EndOfLevel,
    /// Info per level, goes after StartOfLevel
    InfoPerLevel(InfoPerLevel),
}

impl<Expr: Michelson> Display for InternalInboxMessage<Expr> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Transfer(tr) => write!(f, "{}", tr),
            Self::StartOfLevel => write!(f, "StartOfLevel"),
            Self::EndOfLevel => write!(f, "EndOfLevel"),
            Self::InfoPerLevel(ipl) => write!(f, "{}", ipl),
        }
    }
}

#[cfg(test)]
mod test {
    use super::InboxMessage;
    use super::InternalInboxMessage;
    use crate::michelson::Michelson;
    use crate::michelson::MichelsonUnit;

    #[test]
    fn test_encode_decode_sol() {
        // binary encoding produced by lightly-modified (to print encoded data) protocol test
        let expected_bytes = vec![
            // Inbox message start
            0, // Internal tag
            1, // Start of level tag
        ];

        let inbox_message = InboxMessage::Internal(InternalInboxMessage::StartOfLevel);

        test_encode_decode::<MichelsonUnit>(expected_bytes, inbox_message)
    }

    #[test]
    fn test_encode_decode_eol() {
        // binary encoding produced by lightly-modified (to print encoded data) protocol test
        let expected_bytes = vec![
            // Inbox message start
            0, // Internal tag
            2, // End of level tag
        ];

        let inbox_message = InboxMessage::Internal(InternalInboxMessage::EndOfLevel);

        test_encode_decode::<MichelsonUnit>(expected_bytes, inbox_message)
    }

    #[test]
    fn test_encode_decode_external_inbox_message() {
        let assert_enc = |message: Vec<u8>| {
            let inbox_message =
                InboxMessage::<MichelsonUnit>::External(message.as_slice());

            assert_encode_decode_inbox_message(inbox_message);
        };

        assert_enc(vec![]);
        assert_enc(vec![b'A']);
        assert_enc("0123456789".as_bytes().to_vec());
        assert_enc(vec![b'B'; 256]);
        assert_enc(vec![b'\n'; 1234567]);
        assert_enc(vec![5; 1234567]);
    }

    fn assert_encode_decode_inbox_message<Expr: Michelson>(message: InboxMessage<Expr>) {
        let mut encoded = Vec::new();
        message
            .serialize(&mut encoded)
            .expect("encoding should work");

        let decoded = InboxMessage::<Expr>::parse(encoded.as_slice())
            .expect("Deserialization failed")
            .1;

        let mut encoded_twice = Vec::new();
        decoded
            .serialize(&mut encoded_twice)
            .expect("encoding should work");

        assert_eq!(encoded, encoded_twice);
    }

    fn test_encode_decode<Expr: Michelson>(
        expected_bytes: Vec<u8>,
        inbox_message: InboxMessage<Expr>,
    ) {
        // Encoding
        let mut bin = Vec::new();
        inbox_message
            .serialize(&mut bin)
            .expect("encoding should work");

        assert_eq!(expected_bytes, bin, "error in serialization");

        // Decoding
        let (input_remaining, _parsed_message) =
            InboxMessage::<Expr>::parse(bin.as_slice())
                .expect("deserialization should work");

        assert!(input_remaining.is_empty());
    }
}

// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2022-2023, 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
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
use nom::bytes::complete::tag;
use nom::combinator::{map, rest};
use nom::sequence::pair;
use nom::sequence::preceded;
use nom::Finish;
use std::fmt::Display;
use tezos_data_encoding::enc;
use tezos_data_encoding::enc::BinWriter;
use tezos_data_encoding::encoding::HasEncoding;
use tezos_data_encoding::nom::NomReader;
#[cfg(feature = "proto-alpha")]
use tezos_data_encoding::types::Zarith;

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
    pub fn parse(input: &'a [u8]) -> tezos_data_encoding::nom::NomResult<'a, Self> {
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

/// A publisher and their attested slots as a bitset.
#[cfg(feature = "proto-alpha")]
#[derive(Debug, PartialEq, Eq, Clone, NomReader, HasEncoding, BinWriter)]
pub struct PublisherSlots {
    /// The publisher's public key hash.
    pub publisher: PublicKeyHash,
    /// Bitset where bit i is set if slot index i is attested by this publisher.
    pub slots_bitset: Zarith,
}

/// DAL attested slots message - contains information about which slots were
/// attested for a given published level, grouped by publisher.
///
/// The slots for each publisher are encoded as a bitset (Z.t in OCaml),
/// where bit i is set if slot index i is attested.
///
/// This type is currently only available with the proto-alpha feature.
#[cfg(feature = "proto-alpha")]
#[derive(Debug, PartialEq, Eq, Clone, NomReader, HasEncoding, BinWriter)]
pub struct DalAttestedSlots {
    /// The level at which the slots were published.
    pub published_level: i32,
    /// The number of DAL slots (used for interpreting the bitset).
    pub number_of_slots: u16,
    /// The size of each DAL slot in bytes (encoded as int31 in OCaml, but int32 compatible).
    pub slot_size: i32,
    /// The size of each DAL page in bytes.
    pub page_size: u16,
    /// List of publishers and their attested slots.
    /// Max ~184 entries (bounded by number of DAL slots).
    #[encoding(dynamic)]
    pub slots_by_publisher: Vec<PublisherSlots>,
}

#[cfg(feature = "proto-alpha")]
impl Display for DalAttestedSlots {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "DalAttestedSlots {{published_level: {}, number_of_slots: {}, slot_size: {}, page_size: {}, slots_by_publisher: {} entries}}",
            self.published_level,
            self.number_of_slots,
            self.slot_size,
            self.page_size,
            self.slots_by_publisher.len()
        )
    }
}

/// Internal inbox message - known to be sent by the protocol.
#[derive(Debug, PartialEq, Eq, NomReader, HasEncoding, BinWriter)]
pub enum InternalInboxMessage<Expr: Michelson> {
    /// Transfer message
    #[encoding(tag = 0)]
    Transfer(Transfer<Expr>),
    /// Start of level message, pushed at the beginning of an inbox level.
    #[encoding(tag = 1)]
    StartOfLevel,
    /// End of level message, pushed at the end of an inbox level.
    #[encoding(tag = 2)]
    EndOfLevel,
    /// Info per level, goes after StartOfLevel
    #[encoding(tag = 3)]
    InfoPerLevel(InfoPerLevel),
    /// Protocol migration message, pushed after each protocol migration, at the
    /// beginning of the level of the first block of the new protocol, after
    /// [InfoPerLevel]. It contains the new protocol name.
    #[encoding(tag = 4)]
    ProtocolMigration(String),
    /// DAL attested slots message, pushed at each level
    #[cfg(feature = "proto-alpha")]
    #[encoding(tag = 5)]
    DalAttestedSlots(DalAttestedSlots),
}

impl<Expr: Michelson> Display for InternalInboxMessage<Expr> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Transfer(tr) => write!(f, "{}", tr),
            Self::StartOfLevel => write!(f, "StartOfLevel"),
            Self::EndOfLevel => write!(f, "EndOfLevel"),
            Self::InfoPerLevel(ipl) => write!(f, "{}", ipl),
            Self::ProtocolMigration(proto) => {
                write!(f, "ProtocolMigration {{protocol: {}}}", proto)
            }
            #[cfg(feature = "proto-alpha")]
            Self::DalAttestedSlots(dal) => write!(f, "{}", dal),
        }
    }
}

/// External message framing protocol.
///
/// The rollup inbox is global in nature. Therefore a rollup will recieve
/// messages destined for both itself, and other rollups.
///
/// For [`InternalInboxMessage::Transfer`]s, the rollup address is included
/// directly. External messages, however, are purely byte sequences.
///
/// Where a rollup wishes to distinguish which external messages are meant for
/// itself, a framing protocol is suggested.
#[derive(Debug, Eq)]
pub enum ExternalMessageFrame<T: AsRef<[u8]>> {
    /// A message targetted at a single, specific, rollup.
    Targetted {
        /// The address of the targetted rollup.
        address: SmartRollupAddress,
        /// The remaining contents of the message.
        contents: T,
    },
}

impl<T: AsRef<[u8]>> ExternalMessageFrame<T> {
    const TARGETTED_TAG: u8 = 0;
}

impl<'a> ExternalMessageFrame<&'a [u8]> {
    /// Replacement for `nom_read` for [ExternalMessageFrame].
    ///
    /// [NomReader] trait unfortunately does not propagate lifetime of the input bytes,
    /// meaning that it is impossible to use it with a type that refers to a section of
    /// the input.
    ///
    /// In our case, we want to avoid copies if possible - which require additional ticks.
    pub fn parse(
        input: &'a [u8],
    ) -> Result<Self, tezos_data_encoding::nom::NomError<'a>> {
        let (_remaining, message) = map(
            preceded(
                tag([Self::TARGETTED_TAG]),
                pair(SmartRollupAddress::nom_read, rest),
            ),
            |(address, contents)| Self::Targetted { address, contents },
        )(input)
        .finish()?;

        Ok(message)
    }
}

impl<T: AsRef<[u8]>, U: AsRef<[u8]>> core::cmp::PartialEq<ExternalMessageFrame<U>>
    for ExternalMessageFrame<T>
{
    fn eq(&self, other: &ExternalMessageFrame<U>) -> bool {
        match (self, other) {
            (
                Self::Targetted {
                    address: a1,
                    contents: c1,
                },
                ExternalMessageFrame::Targetted {
                    address: a2,
                    contents: c2,
                },
            ) => a1 == a2 && c1.as_ref() == c2.as_ref(),
        }
    }
}

impl<T: AsRef<[u8]>> BinWriter for ExternalMessageFrame<T> {
    fn bin_write(&self, output: &mut Vec<u8>) -> enc::BinResult {
        match self {
            Self::Targetted { address, contents } => {
                enc::put_byte(&Self::TARGETTED_TAG, output);
                address.bin_write(output)?;
                enc::put_bytes(contents.as_ref(), output);
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::ExternalMessageFrame;
    use super::InboxMessage;
    use super::InternalInboxMessage;
    #[cfg(feature = "proto-alpha")]
    use super::{DalAttestedSlots, PublisherSlots};
    use crate::michelson::Michelson;
    use crate::michelson::MichelsonUnit;
    #[cfg(feature = "proto-alpha")]
    use crate::public_key_hash::PublicKeyHash;
    use crate::smart_rollup::SmartRollupAddress;
    #[cfg(feature = "proto-alpha")]
    use num_bigint::BigInt;
    use tezos_data_encoding::enc::BinWriter;
    #[cfg(feature = "proto-alpha")]
    use tezos_data_encoding::nom::NomReader;
    #[cfg(feature = "proto-alpha")]
    use tezos_data_encoding::types::Zarith;

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
    fn test_encode_decode_protocol_migration() {
        let inbox_message: InboxMessage<MichelsonUnit> = InboxMessage::Internal(
            InternalInboxMessage::ProtocolMigration("PtAlphaProtocol".to_string()),
        );

        assert_encode_decode_inbox_message(inbox_message);
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

    #[test]
    fn test_encode_decode_external_framing_targetted() {
        let contents = "Hello, world! (But only in one rollup)";
        let address =
            SmartRollupAddress::from_b58check("sr163Lv22CdE8QagCwf48PWDTquk6isQwv57")
                .unwrap();

        let framed = ExternalMessageFrame::Targetted { address, contents };

        let mut output = Vec::new();
        framed.bin_write(&mut output).unwrap();

        let parsed = ExternalMessageFrame::parse(&output).unwrap();

        assert_eq!(framed, parsed);
    }

    /// Helper to create a bitset from a list of slot indices.
    /// Sets bit i for each index i in the list.
    #[cfg(feature = "proto-alpha")]
    fn make_bitset(indices: &[u32]) -> Zarith {
        let mut value = BigInt::from(0);
        for &idx in indices {
            value |= BigInt::from(1) << idx;
        }
        Zarith(value)
    }

    #[cfg(feature = "proto-alpha")]
    #[test]
    fn test_encode_decode_dal_attested_slots_empty() {
        let dal_attested = DalAttestedSlots {
            published_level: 100,
            number_of_slots: 16,
            slot_size: 126944,
            page_size: 4096,
            slots_by_publisher: Vec::new(),
        };

        let inbox_message: InboxMessage<MichelsonUnit> =
            InboxMessage::Internal(InternalInboxMessage::DalAttestedSlots(dal_attested));

        assert_encode_decode_inbox_message(inbox_message);
    }

    #[cfg(feature = "proto-alpha")]
    #[test]
    fn test_encode_decode_dal_attested_slots_with_slots_by_publisher() {
        let pkh = PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
            .expect("valid pkh");

        // Slots 0, 1, 2 attested -> bitset = 0b111 = 7
        let slots_by_publisher = vec![PublisherSlots {
            publisher: pkh,
            slots_bitset: make_bitset(&[0, 1, 2]),
        }];

        let dal_attested = DalAttestedSlots {
            published_level: 42,
            number_of_slots: 16,
            slot_size: 126944,
            page_size: 4096,
            slots_by_publisher,
        };

        let inbox_message: InboxMessage<MichelsonUnit> =
            InboxMessage::Internal(InternalInboxMessage::DalAttestedSlots(dal_attested));

        assert_encode_decode_inbox_message(inbox_message);
    }

    #[cfg(feature = "proto-alpha")]
    #[test]
    fn test_dal_attested_slots_roundtrip() {
        let pkh1 = PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
            .expect("valid pkh");
        let pkh2 = PublicKeyHash::from_b58check("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN")
            .expect("valid pkh");

        let slots_by_publisher = vec![
            PublisherSlots {
                publisher: pkh1,
                slots_bitset: make_bitset(&[0, 5, 10]),
            },
            PublisherSlots {
                publisher: pkh2,
                slots_bitset: make_bitset(&[1, 2]),
            },
        ];

        let original = DalAttestedSlots {
            published_level: 12345,
            number_of_slots: 16,
            slot_size: 126944,
            page_size: 4096,
            slots_by_publisher,
        };

        let mut encoded = Vec::new();
        original
            .bin_write(&mut encoded)
            .expect("encoding should work");

        let (remaining, decoded) =
            DalAttestedSlots::nom_read(&encoded).expect("decoding should work");

        assert!(remaining.is_empty(), "all bytes should be consumed");
        assert_eq!(original, decoded);
    }

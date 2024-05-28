// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Types & encodings for the *outbox-half* of the *L1/L2 communication protocol*
//!
//! In *general*, this module is a re-implementation of the tezos-protocol
//! [outbox message repr].
//!
//! We have, however, a *specialised* [OutboxMessageTransaction::parameters]. The
//! communication protocol allows payload to be any michelson `script_expr`.
//!
//! [outbox message repr]: <https://gitlab.com/tezos/tezos/-/blob/80b2cccb9c663dde2d86a6c94806fc149b7d1ef3/src/proto_alpha/lib_protocol/sc_rollup_outbox_message_repr.ml>

use tezos_data_encoding::enc::BinWriter;
use tezos_data_encoding::encoding::HasEncoding;
use tezos_data_encoding::nom::NomReader;

use crate::contract::Contract;
use crate::entrypoint::Entrypoint;
use crate::michelson::Michelson;
#[cfg(feature = "proto-alpha")]
use crate::public_key_hash::PublicKeyHash;

/// Outbox message, sent by the kernel as tezos-encoded bytes.
///
/// Encoded as a dynamic list of [OutboxMessageTransaction], with **no** case tag.
#[derive(Debug, PartialEq, Eq, HasEncoding, NomReader, BinWriter)]
pub enum OutboxMessage<Expr: Michelson> {
    /// List of outbox transactions that must succeed together.
    #[encoding(tag = 0)]
    AtomicTransactionBatch(OutboxMessageTransactionBatch<Expr>),
    /// Only keys in the whitelist are allowed to stake and publish a commitment.
    #[cfg(feature = "proto-alpha")]
    #[encoding(tag = 2)]
    WhitelistUpdate(OutboxMessageWhitelistUpdate),
}

/// A batch of [`OutboxMessageTransaction`].
#[derive(Debug, PartialEq, Eq, HasEncoding, BinWriter, NomReader)]
pub struct OutboxMessageTransactionBatch<Expr: Michelson> {
    #[encoding(dynamic, list)]
    batch: Vec<OutboxMessageTransaction<Expr>>,
}

impl<Expr: Michelson> OutboxMessageTransactionBatch<Expr> {
    /// Returns the number of transactions in the batch.
    pub fn len(&self) -> usize {
        self.batch.len()
    }

    /// Returns whether the batch is empty.
    pub fn is_empty(&self) -> bool {
        self.batch.is_empty()
    }
}

impl<Expr: Michelson> core::ops::Index<usize> for OutboxMessageTransactionBatch<Expr> {
    type Output = OutboxMessageTransaction<Expr>;

    fn index(&self, index: usize) -> &Self::Output {
        self.batch.index(index)
    }
}

impl<Expr: Michelson> From<Vec<OutboxMessageTransaction<Expr>>>
    for OutboxMessageTransactionBatch<Expr>
{
    fn from(batch: Vec<OutboxMessageTransaction<Expr>>) -> Self {
        Self { batch }
    }
}

impl<Expr: Michelson> From<OutboxMessageTransaction<Expr>> for OutboxMessage<Expr> {
    fn from(transaction: OutboxMessageTransaction<Expr>) -> Self {
        Self::AtomicTransactionBatch(vec![transaction].into())
    }
}

impl<Expr: Michelson> From<Vec<OutboxMessageTransaction<Expr>>> for OutboxMessage<Expr> {
    fn from(batch: Vec<OutboxMessageTransaction<Expr>>) -> Self {
        Self::AtomicTransactionBatch(batch.into())
    }
}

/// Outbox message transaction, part of the outbox message.
///
/// Encoded as:
/// ```ocaml
/// (obj3
///   (req "parameters" Script_repr.expr_encoding)
///   (req "destination" Contract_repr.originated_encoding)
///   (req "entrypoint" Entrypoint_repr.simple_encoding))
/// ```
#[derive(Debug, PartialEq, Eq, HasEncoding, BinWriter, NomReader)]
pub struct OutboxMessageTransaction<Expr: Michelson> {
    /// Micheline-encoded payload, sent to the destination contract.
    pub parameters: Expr,
    /// The destination smart-contract.
    ///
    /// Protocol side this is a `Contract_hash` (aka `ContractKT1Hash`), but encoded as
    /// `Contract.originated_encoding`.
    pub destination: Contract,
    /// The entrypoint of the destination that will be called.
    pub entrypoint: Entrypoint,
}

/// Whitelist update, part of the outbox message.
/// The keys in the whitelist are allowed to stake and publish a commitment.
/// The whitelist is either Some (non empty list), or None (remove the whitelist,
/// allowing anyone to stake/publish commitments).
///
/// Encoded as:
/// `(opt "whitelist" Sc_rollup_whitelist_repr.encoding)`
/// where
/// `encoding = Data_encoding.(list Signature.Public_key_hash.encoding)`
#[cfg(feature = "proto-alpha")]
#[derive(Debug, PartialEq, Eq, HasEncoding, BinWriter, NomReader)]
pub struct OutboxMessageWhitelistUpdate {
    /// The new contents of the whitelist
    #[encoding(dynamic, list)]
    pub whitelist: Option<Vec<PublicKeyHash>>,
}

/// Kinds of invalid whitelists
#[cfg(feature = "proto-alpha")]
#[derive(Debug, PartialEq, Eq)]
pub enum InvalidWhitelist {
    /// If the provided whitelist is the empty list
    EmptyWhitelist,
    /// Duplicated keys
    DuplicatedKeys,
}

#[cfg(feature = "proto-alpha")]
fn has_unique_elements<T>(iter: T) -> bool
where
    T: IntoIterator,
    T::Item: Eq + Ord,
{
    let mut uniq = std::collections::BTreeSet::new();
    iter.into_iter().all(move |x| uniq.insert(x))
}

/// Returns `Err` on empty list, to not accidentally set the whitelist to
/// None, thereby making the rollup public.
#[cfg(feature = "proto-alpha")]
impl TryFrom<Option<Vec<PublicKeyHash>>> for OutboxMessageWhitelistUpdate {
    type Error = InvalidWhitelist;

    fn try_from(whitelist: Option<Vec<PublicKeyHash>>) -> Result<Self, Self::Error> {
        match whitelist {
            Some(mut list) => {
                if list.is_empty() {
                    return Err(InvalidWhitelist::EmptyWhitelist);
                };
                if !has_unique_elements(&mut list) {
                    Err(InvalidWhitelist::DuplicatedKeys)
                } else {
                    Ok(Self {
                        whitelist: Some(list),
                    })
                }
            }
            None => Ok(Self { whitelist: None }),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::michelson::ticket::StringTicket;

    use super::*;

    // first byte is union tag (currently always `0`, next four are list size of batch)
    const ENCODED_OUTBOX_MESSAGE_PREFIX: [u8; 5] = [0, 0, 0, 0, 152];

    // first byte is union tag (currently always `2`)
    #[cfg(feature = "proto-alpha")]
    const ENCODED_OUTBOX_MESSAGE_WHITELIST_PREFIX: [u8; 1] = [2];

    const ENCODED_TRANSACTION_ONE: [u8; 74] = [
        7, 7, // Prim pair tags
        // Contract KT1 hash
        b'\n', 0, 0, 0, 22, 1, 209, 163, b'|', 8, 138, 18, b'!', 182, b'6', 187, b'_',
        204, 179, b'^', 5, 24, 16, b'8', 186, b'|', 0, // Padding
        7, 7, // Prim pair tags,
        // String
        1, // tag
        0, 0, 0, 3, // size,
        b'r', b'e', b'd', // contents,
        0, 1, // nat size + value
        // Destination
        1, // originated tag
        36, 102, 103, 169, 49, 254, 11, 210, 251, 28, 182, 4, 247, 20, 96, 30, 136, 40,
        69, 80, // end originated
        0,  // padding
        //  entrypoint
        0, 0, 0, 7, b'd', b'e', b'f', b'a', b'u', b'l', b't',
    ];

    const ENCODED_TRANSACTION_TWO: [u8; 78] = [
        // String Ticket
        7, 7, // prim pair tags
        // Contract KT1 hash
        b'\n', 0, 0, 0, 22, 1, b'$', b'f', b'g', 169, b'1', 254, 11, 210, 251, 28, 182, 4,
        247, 20, b'`', 30, 136, b'(', b'E', b'P', 0, // padding
        7, 7, // prim pair tags
        // String contents
        1, 0, 0, 0, 6, b'y', b'e', b'l', b'l', b'o', b'w', // end contents
        0,    // Nat tag
        137, 5, // Z(329)
        // Destination
        1, 21, 237, 173, b'\'', 159, b'U', 226, 254, b'@', 17, 222, b'm', b',', b'$', 253,
        245, 27, 242, b'%', 197, 0, // Entrypoint
        0, 0, 0, 7, // Entrypoint size
        b'a', b'n', b'o', b't', b'h', b'e', b'r', // Entrypoint name
    ];

    // To display the encoding from OCaml:
    // Format.asprintf "%a"
    // Binary_schema.pp
    // (Binary.describe (list Tezos_crypto.Signature.Public_key_hash.encoding))
    #[cfg(feature = "proto-alpha")]
    const ENCODED_WHITELIST_UPDATE: [u8; 47] = [
        0xff, // provide whitelist (0x0 for none)
        0x0, 0x0, 0x0, 0x2a, // # bytes in next field
        // sequence of public_key_hash (21 bytes, 8-bit tag)
        // tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx
        0x0, // Ed25519 (tag 0)
        0x2, 0x29, 0x8c, 0x3, 0xed, 0x7d, 0x45, 0x4a, 0x10, 0x1e, 0xb7, 0x2, 0x2b, 0xc9,
        0x5f, 0x7e, 0x5f, 0x41, 0xac, 0x78,
        // tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN
        0x0, // Ed25519 (tag 0)
        0xe7, 0x67, 0xf, 0x32, 0x3, 0x81, 0x7, 0xa5, 0x9a, 0x2b, 0x9c, 0xfe, 0xfa, 0xe3,
        0x6e, 0xa2, 0x1f, 0x5a, 0xa6, 0x3c,
    ];

    #[test]
    fn encode_transaction() {
        let mut bin = vec![];
        transaction_one().bin_write(&mut bin).unwrap();
        assert_eq!(&ENCODED_TRANSACTION_ONE, bin.as_slice());
    }

    #[test]
    #[cfg(feature = "proto-alpha")]
    fn encode_whitelist_update() {
        let mut bin = vec![];
        whitelist().bin_write(&mut bin).unwrap();
        assert_eq!(&ENCODED_WHITELIST_UPDATE, bin.as_slice());
    }

    #[test]
    fn decode_transaction() {
        let (remaining, decoded) =
            OutboxMessageTransaction::nom_read(ENCODED_TRANSACTION_TWO.as_slice())
                .unwrap();

        assert!(remaining.is_empty());
        assert_eq!(transaction_two(), decoded);
    }

    #[test]
    #[cfg(feature = "proto-alpha")]
    fn decode_whitelist_update() {
        let (remaining, decoded) =
            OutboxMessageWhitelistUpdate::nom_read(ENCODED_WHITELIST_UPDATE.as_slice())
                .unwrap();
        assert!(remaining.is_empty());
        assert_eq!(whitelist(), decoded);
    }

    #[test]
    fn encode_outbox_message() {
        let mut expected = ENCODED_OUTBOX_MESSAGE_PREFIX.to_vec();
        expected.extend_from_slice(ENCODED_TRANSACTION_ONE.as_slice());
        expected.extend_from_slice(ENCODED_TRANSACTION_TWO.as_slice());

        let message = OutboxMessage::AtomicTransactionBatch(
            vec![transaction_one(), transaction_two()].into(),
        );

        let mut bin = vec![];
        message.bin_write(&mut bin).unwrap();

        assert_eq!(expected, bin);
    }

    #[test]
    #[cfg(feature = "proto-alpha")]
    fn encode_outbox_message_whitelist() {
        let mut expected = ENCODED_OUTBOX_MESSAGE_WHITELIST_PREFIX.to_vec();
        expected.extend_from_slice(ENCODED_WHITELIST_UPDATE.as_slice());

        let message: OutboxMessage<StringTicket> =
            OutboxMessage::WhitelistUpdate(whitelist());

        let mut bin = vec![];
        message.bin_write(&mut bin).unwrap();

        assert_eq!(expected, bin);
    }

    #[test]
    fn decode_outbox_message() {
        let mut bytes = ENCODED_OUTBOX_MESSAGE_PREFIX.to_vec();
        bytes.extend_from_slice(ENCODED_TRANSACTION_TWO.as_slice());
        bytes.extend_from_slice(ENCODED_TRANSACTION_ONE.as_slice());

        let expected = OutboxMessage::AtomicTransactionBatch(
            vec![transaction_two(), transaction_one()].into(),
        );

        let (remaining, message) = OutboxMessage::nom_read(bytes.as_slice()).unwrap();

        assert!(remaining.is_empty());
        assert_eq!(expected, message);
    }

    #[test]
    #[cfg(feature = "proto-alpha")]
    fn decode_outbox_message_whitelist() {
        let mut bytes = ENCODED_OUTBOX_MESSAGE_WHITELIST_PREFIX.to_vec();
        bytes.extend_from_slice(ENCODED_WHITELIST_UPDATE.as_slice());

        let expected: OutboxMessage<StringTicket> =
            OutboxMessage::WhitelistUpdate(whitelist());

        let (remaining, message) = OutboxMessage::nom_read(bytes.as_slice()).unwrap();

        assert!(remaining.is_empty());
        assert_eq!(expected, message);
    }

    #[test]
    fn decode_outbox_message_err_on_invalid_prefix() {
        let mut bytes = ENCODED_OUTBOX_MESSAGE_PREFIX.to_vec();
        // prefix too long, missing message
        bytes.extend_from_slice(ENCODED_TRANSACTION_ONE.as_slice());
        // garbage (to be ignored) tail
        bytes.extend_from_slice([10; 1000].as_slice());

        assert!(OutboxMessage::<StringTicket>::nom_read(bytes.as_slice()).is_err());
    }

    #[test]
    #[cfg(feature = "proto-alpha")]
    fn decode_outbox_message_whitelist_err_on_invalid_prefix() {
        let mut bytes = ENCODED_OUTBOX_MESSAGE_PREFIX.to_vec();
        // prefix too long, missing message
        bytes.extend_from_slice(ENCODED_WHITELIST_UPDATE.as_slice());
        // garbage (to be ignored) tail
        bytes.extend_from_slice([10; 1000].as_slice());

        assert!(OutboxMessage::<StringTicket>::nom_read(bytes.as_slice()).is_err());
    }

    fn transaction_one() -> OutboxMessageTransaction<StringTicket> {
        let ticket = StringTicket::new(
            Contract::from_b58check("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq").unwrap(),
            "red".to_string(),
            1_u64,
        )
        .unwrap();
        make_transaction(ticket, "KT1BuEZtb68c1Q4yjtckcNjGELqWt56Xyesc", "default")
    }

    fn transaction_two() -> OutboxMessageTransaction<StringTicket> {
        let ticket = StringTicket::new(
            Contract::from_b58check("KT1BuEZtb68c1Q4yjtckcNjGELqWt56Xyesc").unwrap(),
            "yellow".to_string(),
            329_u64,
        )
        .unwrap();
        make_transaction(ticket, "KT1AaiUqbT3NmQts2w7ofY4vJviVchztiW4y", "another")
    }

    fn make_transaction(
        ticket: StringTicket,
        destination: &str,
        entrypoint: &str,
    ) -> OutboxMessageTransaction<StringTicket> {
        let parameters = ticket;
        let destination = Contract::from_b58check(destination).unwrap();
        let entrypoint = Entrypoint::try_from(entrypoint.to_string()).unwrap();

        OutboxMessageTransaction {
            parameters,
            destination,
            entrypoint,
        }
    }

    #[cfg(feature = "proto-alpha")]
    fn whitelist() -> OutboxMessageWhitelistUpdate {
        let whitelist = Some(vec![
            PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap(),
            PublicKeyHash::from_b58check("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN").unwrap(),
        ]);
        OutboxMessageWhitelistUpdate { whitelist }
    }

    #[test]
    #[cfg(feature = "proto-alpha")]
    fn tryfrom_whitelist() {
        let addr =
            PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap();
        let l1 = Some(vec![addr.clone(), addr.clone()]);
        let w1: Result<OutboxMessageWhitelistUpdate, _> = l1.try_into();
        assert_eq!(
            w1.expect_err("Expected Err(InvalidWhitelist::DuplicatedKeys)"),
            InvalidWhitelist::DuplicatedKeys
        );
        let l2 = Some(vec![]);
        let w2: Result<OutboxMessageWhitelistUpdate, _> = l2.try_into();
        assert_eq!(
            w2.expect_err("Expected Err(InvalidWhitelist::EmptyWhitelist)"),
            InvalidWhitelist::EmptyWhitelist
        );
        let l3 = None;
        let w3: Result<OutboxMessageWhitelistUpdate, _> = l3.try_into();
        assert_eq!(
            w3.expect("Expected Ok(message)"),
            (OutboxMessageWhitelistUpdate { whitelist: None })
        );
        let l4 = Some(vec![addr]);
        let w4: Result<OutboxMessageWhitelistUpdate, _> = l4.clone().try_into();
        assert_eq!(
            w4.expect("Expected Ok(message)"),
            (OutboxMessageWhitelistUpdate { whitelist: l4 })
        );
    }
}

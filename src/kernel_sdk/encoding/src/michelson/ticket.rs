// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023-2024 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

//! Michelson-ticket encoding.

use super::{
    micheline::{annots::Annotations, Node},
    MichelsonTicketContent, TICKET_TAG,
};
use crate::michelson::{
    Michelson, MichelsonBytes, MichelsonContract, MichelsonInt, MichelsonNat,
    MichelsonOption, MichelsonPair, MichelsonString, MichelsonUnit,
};
use core::{
    cmp::Ordering,
    fmt::{Display, Formatter, Result as FmtResult},
};
use crypto::blake2b::{digest_256, Blake2bError};
use hex::FromHexError;
use nom::{
    combinator::map,
    error::{ErrorKind, ParseError},
};
use num_bigint::BigInt;
use num_traits::Signed;
use std::fmt::Debug;
use tezos_data_encoding::{
    enc::{BinError, BinResult, BinWriter},
    encoding::{Encoding, HasEncoding},
    nom::{error::DecodeError, NomReader, NomResult},
    types::{SizedBytes, Zarith},
};
use tezos_protocol::contract::Contract;
use thiserror::Error;

#[cfg(feature = "testing")]
pub mod testing;

/// The length of a Tezos ticket ID
pub const TICKET_HASH_SIZE: usize = 32;

/// The hash of a string ticket - identifying a ticket by creator and contents.
#[derive(Clone, PartialEq, Eq, NomReader, BinWriter, HasEncoding)]
pub struct TicketHash {
    inner: SizedBytes<TICKET_HASH_SIZE>,
}

impl PartialOrd for TicketHash {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TicketHash {
    fn cmp(&self, other: &Self) -> Ordering {
        self.inner.as_ref().cmp(other.inner.as_ref())
    }
}

impl Debug for TicketHash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TicketId(")?;
        for &byte in self.inner.as_ref() {
            write!(f, "{byte:02x?}")?;
        }
        write!(f, ")")
    }
}

impl Display for TicketHash {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", hex::encode(&self.inner))
    }
}

#[allow(clippy::from_over_into)]
impl Into<String> for TicketHash {
    fn into(self) -> String {
        hex::encode(self.inner)
    }
}

impl TryFrom<String> for TicketHash {
    type Error = FromHexError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        let mut result = Self {
            inner: SizedBytes([0; TICKET_HASH_SIZE]),
        };
        hex::decode_to_slice(value, result.inner.as_mut())?;
        Ok(result)
    }
}

/// Errors occurring when identifying tickets.
#[derive(Error, Debug)]
pub enum TicketHashError {
    /// Unable to serialize ticket creator and contents.
    #[error("Unable to serialize ticket for hashing: {0}")]
    Serialization(#[from] BinError),
    /// Unable to hash serialized ticket.
    #[error("Unable to hash ticket bytes: {0}")]
    Hashing(#[from] Blake2bError),
}

/// Errors occurring when identifying tickets.
#[derive(Error, Debug, Clone)]
pub enum TicketError {
    /// Invalid amount in ticket repr.
    #[error("ticket amount out of range")]
    InvalidAmount(BigInt),
}

// TODO: <https://gitlab.com/tezos/tezos/-/issues/6991>
// Switch from `MichelsonInt` to `MichelsonNat` for the amount field
// for consistency with the OCaml part.
// This will be a breaking change.
/// Michelson *ticket* encoding.
#[derive(Debug, PartialEq, Eq)]
struct TypedTicket<Contents>(pub MichelsonContract, pub Contents, pub MichelsonInt)
where
    Contents: Debug + PartialEq + Eq;

impl<Arg> HasEncoding for TypedTicket<Arg>
where
    Arg: Debug + PartialEq + Eq,
{
    fn encoding() -> Encoding {
        Encoding::Custom
    }
}

/// Extract the four fields of a ticket from the given input
/// and return them as `Node`
fn get_ticket_arguments(input: &[u8]) -> NomResult<[Node; 4]> {
    let (fst, node) = Node::nom_read(input)?;
    let Node::Prim {
        prim_tag,
        args,
        annots,
    } = node
    else {
        return Err(nom::Err::Error(DecodeError::from_error_kind(
            input,
            ErrorKind::MapRes,
        )));
    };

    if prim_tag != TICKET_TAG || !annots.is_empty() || args.len() != 4 {
        return Err(nom::Err::Error(DecodeError::from_error_kind(
            input,
            ErrorKind::MapRes,
        )));
    };
    let [arg0, arg1, arg2, arg3]: [_; 4] = args.try_into().unwrap();
    Ok((fst, ([arg0, arg1, arg2, arg3])))
}

impl<Contents: MichelsonTicketContent> NomReader<'_> for TypedTicket<Contents> {
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        // 1st: extract each field of the input
        let (fst, [arg0, arg1, arg2, arg3]) = get_ticket_arguments(input)?;

        // Ensure the `arg1` has the correct type
        if !Contents::typecheck_node(&arg1) {
            return Err(nom::Err::Error(DecodeError::from_error_kind(
                input,
                ErrorKind::MapRes,
            )));
        };
        let Node::Bytes(ticketer) = arg0 else {
            return Err(nom::Err::Error(DecodeError::from_error_kind(
                input,
                ErrorKind::MapRes,
            )));
        };

        let (rest, contract) = Contract::nom_read(&ticketer).unwrap();
        assert!(rest.is_empty());
        let Node::Int(amount) = arg3 else {
            return Err(nom::Err::Error(DecodeError::from_error_kind(
                input,
                ErrorKind::MapRes,
            )));
        };

        // Extract MichelsonUnit, which is the contents of the ticket
        let contents = Contents::of_node(arg2).unwrap();

        // Data retrieved from input is correct, build the corresponding MichelsonTicket
        let ticket =
            TypedTicket(MichelsonContract(contract), contents, MichelsonInt(amount));
        Ok((fst, ticket))
    }
}

impl<Contents: MichelsonTicketContent> BinWriter for TypedTicket<Contents> {
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        let TypedTicket(ticketer, contents, amount) = self;

        let mut ticketer_bytes = Vec::new();
        ticketer.0.bin_write(&mut ticketer_bytes).unwrap();

        let ticketer = Node::Bytes(ticketer_bytes);
        let amount = Node::Int(amount.0.clone());

        let contents = contents.to_node();
        let contents_type = Contents::node_of_type();

        let args = vec![ticketer, contents_type, contents, amount];

        let node = Node::Prim {
            prim_tag: TICKET_TAG,
            args,
            annots: Annotations(vec![]),
        };
        node.bin_write(output)
    }
}

// Expr is guarantee by construction to implement `Michelson` even though
// rust does not enforce it in type aliases `type TicketRepr<Expr: Michelson>`.
type LegacyTicketRepr<Expr> =
    MichelsonPair<MichelsonContract, MichelsonPair<Expr, MichelsonInt>>;

enum EncodedTicketRepr<Expr>
where
    Expr: super::MichelsonTicketContent + Debug + Eq,
{
    Legacy(LegacyTicketRepr<Expr>),
    Typed(TypedTicket<Expr>),
}

impl<Expr: MichelsonTicketContent + Debug + Eq> From<EncodedTicketRepr<Expr>>
    for LegacyTicketRepr<Expr>
{
    fn from(s: EncodedTicketRepr<Expr>) -> Self {
        match s {
            EncodedTicketRepr::Legacy(l) => l,
            EncodedTicketRepr::Typed(TypedTicket(contract, contents, amount)) => {
                MichelsonPair(contract, MichelsonPair(contents, amount))
            }
        }
    }
}

/// Michelson ticket representative.
#[derive(Debug, PartialEq, Eq)]
pub struct Ticket<Expr: MichelsonTicketContent>(pub(crate) LegacyTicketRepr<Expr>);

impl<Expr> Michelson for Ticket<Expr> where Expr: MichelsonTicketContent {}

impl<Expr> NomReader<'_> for Ticket<Expr>
where
    Expr: MichelsonTicketContent,
{
    fn nom_read(bytes: &[u8]) -> NomResult<Self> {
        use nom::branch::alt;

        map(
            alt((
                map(
                    <LegacyTicketRepr<Expr>>::nom_read,
                    EncodedTicketRepr::Legacy,
                ),
                map(TypedTicket::<Expr>::nom_read, EncodedTicketRepr::Typed),
            )),
            |repr| Ticket(repr.into()),
        )(bytes)
    }
}

impl<Expr: MichelsonTicketContent> BinWriter for Ticket<Expr> {
    // TODO: <https://gitlab.com/tezos/tezos/-/issues/6992>
    // Switch the output to the new Ticket constructor.
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        self.0.bin_write(output)
    }
}

impl<Expr: MichelsonTicketContent> HasEncoding for Ticket<Expr> {
    fn encoding() -> tezos_data_encoding::encoding::Encoding {
        <LegacyTicketRepr<Expr>>::encoding()
    }
}

impl<Expr: MichelsonTicketContent> Ticket<Expr> {
    /// creates a new ticket with `creator`, `contents` and `amount`.
    pub fn new<Val: Into<Expr>, Amount: Into<BigInt>>(
        creator: Contract,
        contents: Val,
        amount: Amount,
    ) -> Result<Self, TicketError> {
        let amount: BigInt = amount.into();
        if amount.is_positive() {
            Ok(Ticket(MichelsonPair(
                MichelsonContract(creator),
                MichelsonPair(contents.into(), MichelsonInt(Zarith(amount))),
            )))
        } else {
            Err(TicketError::InvalidAmount(amount))
        }
    }

    /// Return an identifying hash of the ticket creator and contents.
    ///
    /// Calculated as the `blake2b` hash of a tezos-encoded `obj2`:
    /// - creator contract
    /// - string contents
    pub fn hash(&self) -> Result<TicketHash, TicketHashError> {
        let mut bytes = Vec::new();
        self.creator().bin_write(&mut bytes)?;
        self.contents().bin_write(&mut bytes)?;

        let digest = digest_256(bytes.as_slice());
        let digest: [u8; TICKET_HASH_SIZE] = digest.try_into().unwrap();

        Ok(TicketHash {
            inner: SizedBytes(digest),
        })
    }

    /// The L1 ticketer's address.
    pub fn creator(&self) -> &MichelsonContract {
        &self.0 .0
    }
    /// The ticket's content
    pub fn contents(&self) -> &Expr {
        &self.0 .1 .0
    }
    /// The ticket's amount
    pub fn amount(&self) -> &BigInt {
        &self.0 .1 .1 .0 .0
    }

    /// same as `amount()` but returns it as a `T`
    pub fn amount_as<T: TryFrom<BigInt, Error = E>, E>(&self) -> Result<T, E> {
        self.amount().to_owned().try_into()
    }
}

/// Specialized version of ticket where the content must be an int
pub type IntTicket = Ticket<MichelsonInt>;

/// Specialized version of ticket where the content must be a string
pub type StringTicket = Ticket<MichelsonString>;

impl Ticket<MichelsonString> {
    /// clone used in testing
    #[cfg(feature = "testing")]
    pub fn testing_clone(&self) -> Self {
        Ticket(MichelsonPair(
            MichelsonContract(self.creator().0.clone()),
            MichelsonPair(
                MichelsonString(self.contents().0.clone()),
                MichelsonInt(Zarith(self.amount().clone())),
            ),
        ))
    }
}

/// Specialized version of ticket where the content must be byte
pub type BytesTicket = Ticket<MichelsonBytes>;

/// Specialized version of ticket where the content must be unit
pub type UnitTicket = Ticket<MichelsonUnit>;

/// Specialized version of ticket for FA2.1 tokens
pub type FA2_1Ticket =
    Ticket<MichelsonPair<MichelsonNat, MichelsonOption<MichelsonBytes>>>;

#[cfg(test)]
mod test {
    use crate::michelson::MichelsonOr;

    use super::*;
    use tezos_data_encoding::enc::BinWriter;
    use tezos_data_encoding::nom::NomReader;

    #[test]
    fn content_bytes() {
        let ticket = BytesTicket::new(
            Contract::from_b58check("KT1NgXQ6Mwu3XKFDcKdYFS6dkkY3iNKdBKEc").unwrap(),
            MichelsonBytes(vec![1, 2, 3, 4, 5]),
            500,
        )
        .unwrap();

        assert_encode_decode(ticket);
    }

    #[test]
    fn content_string() {
        let ticket = StringTicket::new(
            Contract::from_b58check("KT1NgXQ6Mwu3XKFDcKdYFS6dkkY3iNKdBKEc").unwrap(),
            MichelsonString("Hello, Ticket".to_string()),
            900,
        )
        .unwrap();

        assert_encode_decode(ticket);
    }

    #[test]
    fn content_unit() {
        let ticket = UnitTicket::new(
            Contract::from_b58check("KT1NgXQ6Mwu3XKFDcKdYFS6dkkY3iNKdBKEc").unwrap(),
            MichelsonUnit,
            900,
        )
        .unwrap();

        assert_encode_decode(ticket);
    }

    #[test]
    fn content_int() {
        let ticket = IntTicket::new::<i32, i32>(
            Contract::from_b58check("KT1NgXQ6Mwu3XKFDcKdYFS6dkkY3iNKdBKEc").unwrap(),
            -25,
            900,
        )
        .unwrap();

        assert_encode_decode(ticket);
    }

    #[test]
    fn content_pair() {
        type NestedPair = MichelsonPair<
            MichelsonUnit,
            MichelsonPair<MichelsonPair<MichelsonString, MichelsonBytes>, MichelsonInt>,
        >;
        let ticket: Ticket<NestedPair> = Ticket::new::<_, i32>(
            Contract::from_b58check("KT1NgXQ6Mwu3XKFDcKdYFS6dkkY3iNKdBKEc").unwrap(),
            MichelsonPair(
                MichelsonUnit,
                MichelsonPair(
                    MichelsonPair(
                        MichelsonString("hello".to_string()),
                        MichelsonBytes(b"a series of bytes".to_vec()),
                    ),
                    MichelsonInt::from(19),
                ),
            ),
            17,
        )
        .unwrap();

        assert_encode_decode(ticket);
    }

    fn assert_encode_decode<T: MichelsonTicketContent>(ticket: Ticket<T>) {
        let mut bin = Vec::new();
        ticket.bin_write(&mut bin).unwrap();

        let (remaining, parsed) = Ticket::nom_read(&bin).unwrap();

        assert_eq!(ticket, parsed);
        assert!(remaining.is_empty());
    }

    #[test]
    fn basic_encode_decode_on_unit_ticket() {
        let expected_ticket = TypedTicket(
            MichelsonContract(
                Contract::from_b58check("KT1FHqsvc7vRS3u54L66DdMX4gb6QKqxJ1JW").unwrap(),
            ),
            MichelsonUnit,
            11_i32.into(),
        );
        let mut output = Vec::new();
        let result = expected_ticket.bin_write(&mut output);
        assert!(result.is_ok());
        let result = TypedTicket::<MichelsonUnit>::nom_read(output.as_slice());
        assert!(result.is_ok());
        let (remaining, ticket) = result.unwrap();
        assert_eq!(expected_ticket, ticket);
        assert!(remaining.is_empty());
    }

    #[test]
    fn basic_encode_decode_on_int_ticket() {
        let expected_ticket: TypedTicket<MichelsonInt> = TypedTicket(
            MichelsonContract(
                Contract::from_b58check("KT1FHqsvc7vRS3u54L66DdMX4gb6QKqxJ1JW").unwrap(),
            ),
            17_i32.into(),
            11_i32.into(),
        );
        let mut output = Vec::new();
        let result = expected_ticket.bin_write(&mut output);
        assert!(result.is_ok());
        let result = TypedTicket::<MichelsonInt>::nom_read(output.as_slice());
        assert!(result.is_ok());
        let (remaining, ticket) = result.unwrap();
        assert_eq!(expected_ticket, ticket);
        assert!(remaining.is_empty());
    }

    #[test]
    fn basic_encode_decode_on_nat_ticket() {
        let expected_ticket: TypedTicket<MichelsonNat> = TypedTicket(
            MichelsonContract(
                Contract::from_b58check("KT1FHqsvc7vRS3u54L66DdMX4gb6QKqxJ1JW").unwrap(),
            ),
            17_u32.into(),
            11_i32.into(),
        );
        let mut output = Vec::new();
        let result = expected_ticket.bin_write(&mut output);
        assert!(result.is_ok());
        let result = TypedTicket::<MichelsonNat>::nom_read(output.as_slice());
        assert!(result.is_ok());
        let (remaining, ticket) = result.unwrap();
        assert_eq!(expected_ticket, ticket);
        assert!(remaining.is_empty());
    }

    #[test]
    fn basic_encode_decode_on_string_ticket() {
        let expected_ticket = TypedTicket(
            MichelsonContract(
                Contract::from_b58check("KT1FHqsvc7vRS3u54L66DdMX4gb6QKqxJ1JW").unwrap(),
            ),
            MichelsonString("string".into()),
            11_i32.into(),
        );
        let mut output = Vec::new();
        let result = expected_ticket.bin_write(&mut output);
        assert!(result.is_ok());
        let result = TypedTicket::<MichelsonString>::nom_read(output.as_slice());
        assert!(result.is_ok());
        let (remaining, ticket) = result.unwrap();
        assert_eq!(expected_ticket, ticket);
        assert!(remaining.is_empty());
    }

    #[test]
    fn basic_encode_decode_on_bytes_ticket() {
        let expected_ticket = TypedTicket(
            MichelsonContract(
                Contract::from_b58check("KT1FHqsvc7vRS3u54L66DdMX4gb6QKqxJ1JW").unwrap(),
            ),
            MichelsonBytes(vec![62, 79, 74, 65, 73]),
            11_i32.into(),
        );
        let mut output = Vec::new();
        let result = expected_ticket.bin_write(&mut output);
        assert!(result.is_ok());
        let result = TypedTicket::<MichelsonBytes>::nom_read(output.as_slice());
        assert!(result.is_ok());
        let (remaining, ticket) = result.unwrap();
        assert_eq!(expected_ticket, ticket);
        assert!(remaining.is_empty());
    }

    #[test]
    fn basic_encode_decode_on_option_ticket() {
        // Some
        let expected_ticket = TypedTicket(
            MichelsonContract(
                Contract::from_b58check("KT1FHqsvc7vRS3u54L66DdMX4gb6QKqxJ1JW").unwrap(),
            ),
            MichelsonOption(Some(MichelsonBytes(vec![62, 79, 74, 65, 73]))),
            11_i32.into(),
        );
        let mut output = Vec::new();
        let result = expected_ticket.bin_write(&mut output);
        assert!(result.is_ok());
        let result = TypedTicket::nom_read(output.as_slice());
        assert!(result.is_ok());
        let (remaining, ticket) = result.unwrap();
        assert_eq!(expected_ticket, ticket);
        assert!(remaining.is_empty());

        // None
        let expected_ticket = TypedTicket::<MichelsonOption<MichelsonBytes>>(
            MichelsonContract(
                Contract::from_b58check("KT1FHqsvc7vRS3u54L66DdMX4gb6QKqxJ1JW").unwrap(),
            ),
            MichelsonOption(None),
            11_i32.into(),
        );
        let mut output = Vec::new();
        let result = expected_ticket.bin_write(&mut output);
        assert!(result.is_ok());
        let result = TypedTicket::nom_read(output.as_slice());
        assert!(result.is_ok());
        let (remaining, ticket) = result.unwrap();
        assert_eq!(expected_ticket, ticket);
        assert!(remaining.is_empty());
    }

    #[test]
    fn basic_encode_decode_on_or_ticket() {
        // Left
        let left: MichelsonOr<MichelsonBytes, MichelsonBytes> =
            MichelsonOr::Left(MichelsonBytes(vec![62, 79, 74, 65, 73]));
        let expected_ticket = TypedTicket(
            MichelsonContract(
                Contract::from_b58check("KT1FHqsvc7vRS3u54L66DdMX4gb6QKqxJ1JW").unwrap(),
            ),
            left,
            11_i32.into(),
        );
        let mut output = Vec::new();
        let result = expected_ticket.bin_write(&mut output);
        assert!(result.is_ok());
        let result = TypedTicket::nom_read(output.as_slice());
        assert!(result.is_ok());
        let (remaining, ticket) = result.unwrap();
        assert_eq!(expected_ticket, ticket);
        assert!(remaining.is_empty());

        // Right
        let right: MichelsonOr<MichelsonBytes, MichelsonBytes> =
            MichelsonOr::Right(MichelsonBytes(vec![62, 79, 74, 65, 73]));
        let expected_ticket = TypedTicket(
            MichelsonContract(
                Contract::from_b58check("KT1FHqsvc7vRS3u54L66DdMX4gb6QKqxJ1JW").unwrap(),
            ),
            right,
            11_i32.into(),
        );
        let mut output = Vec::new();
        let result = expected_ticket.bin_write(&mut output);
        assert!(result.is_ok());
        let result = TypedTicket::nom_read(output.as_slice());
        assert!(result.is_ok());
        let (remaining, ticket) = result.unwrap();
        assert_eq!(expected_ticket, ticket);
        assert!(remaining.is_empty());
    }

    #[test]
    fn basic_encode_decode_on_pair_ticket() {
        let expected_ticket = TypedTicket(
            MichelsonContract(
                Contract::from_b58check("KT1FHqsvc7vRS3u54L66DdMX4gb6QKqxJ1JW").unwrap(),
            ),
            MichelsonPair(MichelsonInt::from(5_i32), MichelsonNat::from(11_u32)),
            11_i32.into(),
        );
        let mut output = Vec::new();
        let result = expected_ticket.bin_write(&mut output);
        assert!(result.is_ok());
        let result = TypedTicket::nom_read(output.as_slice());
        assert!(result.is_ok());
        let (remaining, ticket) = result.unwrap();
        assert_eq!(expected_ticket, ticket);
        assert!(remaining.is_empty());
    }
}

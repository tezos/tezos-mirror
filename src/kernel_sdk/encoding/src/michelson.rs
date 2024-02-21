// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

//! Definitions & tezos-encodings for *michelson* data.
use micheline::annots::Annotations;
use nom::branch::alt;
use nom::combinator::map;
use prim::*;
use std::fmt::Debug;
use tezos_data_encoding::enc::{self, BinResult, BinWriter};
use tezos_data_encoding::encoding::{Encoding, HasEncoding};
use tezos_data_encoding::nom::{self as nom_read, NomReader, NomResult};
use tezos_data_encoding::types::Zarith;

mod micheline;
#[cfg(feature = "alloc")]
pub mod ticket;

use self::micheline::Node;
use super::contract::Contract;
use micheline::{
    bin_write_micheline_bytes, bin_write_micheline_int, bin_write_micheline_string,
    bin_write_prim_1_arg_no_annots, bin_write_prim_2_args_no_annots,
    bin_write_prim_no_args_no_annots, nom_read_micheline_bytes, nom_read_micheline_int,
    nom_read_micheline_string, MichelinePrim1ArgNoAnnots, MichelinePrim2ArgsNoAnnots,
    MichelinePrimNoArgsNoAnnots,
};
use v1_primitives as prim;

pub mod v1_primitives {
    //! Encoding of [michelson_v1_primitives].
    //!
    //! [michelson_v1_primitives]: <https://gitlab.com/tezos/tezos/-/blob/9028b797894a5d9db38bc61a20abb793c3778316/src/proto_alpha/lib_protocol/michelson_v1_primitives.ml>

    /// `("Left", D_Left)` case tag.
    pub const LEFT_TAG: u8 = 5;

    /// `("None", D_None)` case tag.
    pub const NONE_TAG: u8 = 6;

    /// `("Pair", D_PAIR)` case tag.
    pub const PAIR_TAG: u8 = 7;

    /// `("Right", D_Right)` case tag.
    pub const RIGHT_TAG: u8 = 8;

    /// `("Some", D_Some)` case tag.
    pub const SOME_TAG: u8 = 9;

    /// unit encoding case tag.
    pub const UNIT_TAG: u8 = 11;

    /// int type tag
    pub const INT_TYPE_TAG: u8 = 91;

    /// nat type tag
    pub const NAT_TYPE_TAG: u8 = 98;

    /// option type tag
    pub const OPTION_TYPE_TAG: u8 = 99;

    /// or type tag
    pub const OR_TYPE_TAG: u8 = 100;

    /// pair type tag
    pub const PAIR_TYPE_TAG: u8 = 101;

    /// string type tag
    pub const STRING_TYPE_TAG: u8 = 104;

    /// bytes type tag
    pub const BYTES_TYPE_TAG: u8 = 105;

    /// unit type tag
    pub const UNIT_TYPE_TAG: u8 = 108;

    /// ticket encoding case tag.
    pub const TICKET_TAG: u8 = 157;
}

// TODO: <https://gitlab.com/tezos/tezos/-/issues/6990>
// combine MichelsonTicketContent and Michelson traits
/// marker trait for michelson encoding
pub trait Michelson:
    HasEncoding + BinWriter + NomReader + Debug + PartialEq + Eq
{
}

impl Michelson for MichelsonUnit {}
impl Michelson for MichelsonContract {}
impl Michelson for MichelsonInt {}
impl Michelson for MichelsonNat {}
impl Michelson for MichelsonString {}
impl Michelson for MichelsonBytes {}
impl<Arg0, Arg1> Michelson for MichelsonPair<Arg0, Arg1>
where
    Arg0: Michelson,
    Arg1: Michelson,
{
}
impl<Arg0, Arg1> Michelson for MichelsonOr<Arg0, Arg1>
where
    Arg0: Michelson,
    Arg1: Michelson,
{
}
impl<Arg> Michelson for MichelsonOption<Arg> where Arg: Michelson {}

/// Michelson *unit* encoding.
#[derive(Debug, PartialEq, Eq)]
pub struct MichelsonUnit;

/// Michelson *contract* encoding.
///
#[derive(Debug, PartialEq, Eq)]
pub struct MichelsonContract(pub Contract);

/// Michelson *pair* encoding.
#[derive(Debug, PartialEq, Eq)]
pub struct MichelsonPair<Arg0, Arg1>(pub Arg0, pub Arg1)
where
    Arg0: Debug + PartialEq + Eq,
    Arg1: Debug + PartialEq + Eq;

/// Michelson *or* encoding.
#[derive(Debug, PartialEq, Eq)]
pub enum MichelsonOr<Arg0, Arg1>
where
    Arg0: Debug + PartialEq + Eq,
    Arg1: Debug + PartialEq + Eq,
{
    /// The *Left* case
    Left(Arg0),
    /// The *Right* case
    Right(Arg1),
}

/// Michelson *option* encoding.  #[derive(Debug, PartialEq, Eq)] pub
#[derive(Debug, PartialEq, Eq)]
pub struct MichelsonOption<Arg>(pub Option<Arg>)
where
    Arg: Debug + PartialEq + Eq;

/// Michelson String encoding.
#[derive(Debug, PartialEq, Eq)]
pub struct MichelsonString(pub String);

/// Michelson Bytes encoding.
#[derive(Debug, PartialEq, Eq)]
pub struct MichelsonBytes(pub Vec<u8>);

/// Michelson Int encoding.
#[derive(Debug, PartialEq, Eq)]
pub struct MichelsonInt(pub Zarith);

/// Michelson Nat encoding.
#[derive(Debug, PartialEq, Eq)]
pub struct MichelsonNat(Zarith);
impl MichelsonNat {
    /// Create a new nat, returns none if z is negative
    pub fn new(z: Zarith) -> Option<Self> {
        if z.0 >= 0.into() {
            Some(Self(z))
        } else {
            None
        }
    }
}

impl AsRef<Zarith> for MichelsonNat {
    fn as_ref(&self) -> &Zarith {
        &self.0
    }
}

// ----------
// CONVERSION
// ----------
impl From<String> for MichelsonString {
    fn from(str: String) -> MichelsonString {
        MichelsonString(str)
    }
}

impl From<Vec<u8>> for MichelsonBytes {
    fn from(b: Vec<u8>) -> MichelsonBytes {
        MichelsonBytes(b)
    }
}

impl From<Zarith> for MichelsonInt {
    fn from(value: Zarith) -> MichelsonInt {
        MichelsonInt(value)
    }
}

impl From<i32> for MichelsonInt {
    fn from(value: i32) -> MichelsonInt {
        MichelsonInt(Zarith(value.into()))
    }
}
impl From<u32> for MichelsonNat {
    fn from(value: u32) -> MichelsonNat {
        MichelsonNat(Zarith(value.into()))
    }
}

// --------
// ENCODING
// --------
impl HasEncoding for MichelsonContract {
    fn encoding() -> Encoding {
        Encoding::Custom
    }
}

impl HasEncoding for MichelsonUnit {
    fn encoding() -> Encoding {
        Encoding::Custom
    }
}

impl<Arg0, Arg1> HasEncoding for MichelsonPair<Arg0, Arg1>
where
    Arg0: Debug + PartialEq + Eq,
    Arg1: Debug + PartialEq + Eq,
{
    fn encoding() -> Encoding {
        Encoding::Custom
    }
}

impl<Arg0, Arg1> HasEncoding for MichelsonOr<Arg0, Arg1>
where
    Arg0: Debug + PartialEq + Eq,
    Arg1: Debug + PartialEq + Eq,
{
    fn encoding() -> Encoding {
        Encoding::Custom
    }
}

impl<Arg> HasEncoding for MichelsonOption<Arg>
where
    Arg: Debug + PartialEq + Eq,
{
    fn encoding() -> Encoding {
        Encoding::Custom
    }
}

impl HasEncoding for MichelsonString {
    fn encoding() -> Encoding {
        Encoding::Custom
    }
}

impl HasEncoding for MichelsonBytes {
    fn encoding() -> Encoding {
        Encoding::Custom
    }
}

impl HasEncoding for MichelsonInt {
    fn encoding() -> Encoding {
        Encoding::Custom
    }
}
impl HasEncoding for MichelsonNat {
    fn encoding() -> Encoding {
        Encoding::Custom
    }
}

// --------
// DECODING
// --------
impl NomReader for MichelsonContract {
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        map(
            nom_read_micheline_bytes(Contract::nom_read),
            MichelsonContract,
        )(input)
    }
}

impl NomReader for MichelsonUnit {
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        map(
            MichelinePrimNoArgsNoAnnots::<{ prim::UNIT_TAG }>::nom_read,
            |_prim| MichelsonUnit,
        )(input)
    }
}

impl<Arg0, Arg1> NomReader for MichelsonPair<Arg0, Arg1>
where
    Arg0: NomReader + Debug + PartialEq + Eq,
    Arg1: NomReader + Debug + PartialEq + Eq,
{
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        map(
            MichelinePrim2ArgsNoAnnots::<_, _, { prim::PAIR_TAG }>::nom_read,
            Into::into,
        )(input)
    }
}
/// Functions needed to ensure a safe conversion to/from a valid MichelsonTicket and to/from a Node.
#[doc(hidden)]
pub trait MichelsonTicketContent: Michelson {
    fn typecheck_node(node: &Node) -> bool;
    fn node_of_type() -> Node;
    fn of_node(node: Node) -> Option<Self>;
    fn to_node(&self) -> Node;
}

macro_rules! typed_prim {
    ($ty:ty, $tag:expr) => {
    fn typecheck_node(node: &Node) -> bool {
        matches!(node, Node::Prim {
                prim_tag,
                args,
                annots,
            } if *prim_tag == $tag && args.is_empty() && annots.is_empty())
    }
    fn node_of_type() -> Node {
        Node::Prim {
            prim_tag: $tag,
            args: vec![],
            annots: Annotations(vec![]),
        }
    }
    }
}

impl MichelsonTicketContent for MichelsonUnit {
    typed_prim!(MichelsonUnit, UNIT_TYPE_TAG);

    fn of_node(node: Node) -> Option<Self> {
        match node {
            Node::Prim {
                prim_tag,
                args,
                annots,
            } if prim_tag == UNIT_TAG && args.is_empty() && annots.is_empty() => {
                Some(MichelsonUnit)
            }
            _ => None,
        }
    }

    fn to_node(&self) -> Node {
        Node::Prim {
            prim_tag: UNIT_TAG,
            args: vec![],
            annots: Annotations(vec![]),
        }
    }
}

impl MichelsonTicketContent for MichelsonNat {
    typed_prim!(MichelsonNat, NAT_TYPE_TAG);

    fn of_node(value: Node) -> Option<Self> {
        match value {
            Node::Int(value) if value.0 >= 0.into() => Some(MichelsonNat(value)),
            _ => None,
        }
    }

    fn to_node(&self) -> Node {
        Node::Int(self.0.clone())
    }
}

impl MichelsonTicketContent for MichelsonInt {
    typed_prim!(MichelsonInt, INT_TYPE_TAG);

    fn of_node(value: Node) -> Option<Self> {
        match value {
            Node::Int(value) => Some(MichelsonInt(value)),
            _ => None,
        }
    }

    fn to_node(&self) -> Node {
        Node::Int(self.0.clone())
    }
}

impl MichelsonTicketContent for MichelsonString {
    typed_prim!(MichelsonString, STRING_TYPE_TAG);

    fn of_node(value: Node) -> Option<Self> {
        match value {
            Node::String(value) => Some(MichelsonString(value)),
            _ => None,
        }
    }

    fn to_node(&self) -> Node {
        Node::String(self.0.clone())
    }
}

impl MichelsonTicketContent for MichelsonBytes {
    typed_prim!(MichelsonBytes, BYTES_TYPE_TAG);

    fn of_node(value: Node) -> Option<Self> {
        match value {
            Node::Bytes(value) => Some(MichelsonBytes(value)),
            _ => None,
        }
    }

    fn to_node(&self) -> Node {
        Node::Bytes(self.0.clone())
    }
}

impl<Expr> MichelsonTicketContent for MichelsonOption<Expr>
where
    Expr: MichelsonTicketContent,
{
    fn typecheck_node(node: &Node) -> bool {
        let args = match node {
            Node::Prim {
                prim_tag,
                args,
                annots,
            } if *prim_tag == OPTION_TYPE_TAG && annots.is_empty() => args,
            _ => return false,
        };

        match args.as_slice() {
            [arg] => Expr::typecheck_node(arg),
            _ => false,
        }
    }

    fn node_of_type() -> Node {
        Node::Prim {
            prim_tag: OPTION_TYPE_TAG,
            args: vec![Expr::node_of_type()],
            annots: Annotations(vec![]),
        }
    }

    fn to_node(&self) -> Node {
        match &self.0 {
            Some(v) => Node::Prim {
                prim_tag: SOME_TAG,
                args: vec![Expr::to_node(v)],
                annots: Annotations(vec![]),
            },
            None => Node::Prim {
                prim_tag: NONE_TAG,
                args: vec![],
                annots: Annotations(vec![]),
            },
        }
    }
    fn of_node(value: Node) -> Option<Self> {
        match value {
            Node::Prim {
                prim_tag,
                args,
                annots,
            } if prim_tag == NONE_TAG && args.is_empty() && annots.is_empty() => {
                Some(MichelsonOption(None))
            }
            Node::Prim {
                prim_tag,
                args,
                annots,
            } if prim_tag == SOME_TAG && annots.is_empty() && args.len() == 1 => {
                let [v]: [Node; 1] = args.try_into().unwrap();
                let v = Expr::of_node(v)?;
                Some(MichelsonOption(Some(v)))
            }
            _ => None,
        }
    }
}

impl<Arg0, Arg1> MichelsonTicketContent for MichelsonPair<Arg0, Arg1>
where
    Arg0: MichelsonTicketContent,
    Arg1: MichelsonTicketContent,
{
    fn typecheck_node(node: &Node) -> bool {
        let args = match node {
            Node::Prim {
                prim_tag,
                args,
                annots,
            } if *prim_tag == PAIR_TYPE_TAG && annots.is_empty() => args,
            _ => return false,
        };

        match args.as_slice() {
            [arg0, arg1] => Arg0::typecheck_node(arg0) && Arg1::typecheck_node(arg1),
            _ => false,
        }
    }

    fn node_of_type() -> Node {
        Node::Prim {
            prim_tag: PAIR_TYPE_TAG,
            args: vec![Arg0::node_of_type(), Arg1::node_of_type()],
            annots: Annotations(vec![]),
        }
    }

    fn of_node(value: Node) -> Option<Self> {
        match value {
            Node::Prim {
                prim_tag,
                args,
                annots,
            } if prim_tag == PAIR_TAG && annots.is_empty() && args.len() == 2 => {
                let [v0, v1]: [Node; 2] = args.try_into().unwrap();
                let v0 = Arg0::of_node(v0)?;
                let v1 = Arg1::of_node(v1)?;
                Some(MichelsonPair(v0, v1))
            }
            _ => None,
        }
    }

    fn to_node(&self) -> Node {
        let MichelsonPair(v0, v1) = self;
        Node::Prim {
            prim_tag: PAIR_TAG,
            args: vec![Arg0::to_node(v0), Arg1::to_node(v1)],
            annots: Annotations(vec![]),
        }
    }
}

impl<Arg0, Arg1> MichelsonTicketContent for MichelsonOr<Arg0, Arg1>
where
    Arg0: MichelsonTicketContent,
    Arg1: MichelsonTicketContent,
{
    fn typecheck_node(node: &Node) -> bool {
        let args = match node {
            Node::Prim {
                prim_tag,
                args,
                annots,
            } if *prim_tag == OR_TYPE_TAG && annots.is_empty() => args,
            _ => return false,
        };

        match args.as_slice() {
            [left, right] => Arg0::typecheck_node(left) && Arg1::typecheck_node(right),
            _ => false,
        }
    }

    fn node_of_type() -> Node {
        Node::Prim {
            prim_tag: OR_TYPE_TAG,
            args: vec![Arg0::node_of_type(), Arg1::node_of_type()],
            annots: Annotations(vec![]),
        }
    }

    fn to_node(&self) -> Node {
        match &self {
            MichelsonOr::Left(v) => Node::Prim {
                prim_tag: LEFT_TAG,
                args: vec![Arg0::to_node(v)],
                annots: Annotations(vec![]),
            },
            MichelsonOr::Right(v) => Node::Prim {
                prim_tag: RIGHT_TAG,
                args: vec![Arg1::to_node(v)],
                annots: Annotations(vec![]),
            },
        }
    }
    fn of_node(value: Node) -> Option<Self> {
        match value {
            Node::Prim {
                prim_tag,
                args,
                annots,
            } if prim_tag == LEFT_TAG && annots.is_empty() && args.len() == 1 => {
                let [v]: [Node; 1] = args.try_into().unwrap();
                let v = Arg0::of_node(v)?;
                Some(MichelsonOr::Left(v))
            }
            Node::Prim {
                prim_tag,
                args,
                annots,
            } if prim_tag == RIGHT_TAG && annots.is_empty() && args.len() == 1 => {
                let [v]: [Node; 1] = args.try_into().unwrap();
                let v = Arg1::of_node(v)?;
                Some(MichelsonOr::Right(v))
            }
            _ => None,
        }
    }
}

impl<Arg0, Arg1> NomReader for MichelsonOr<Arg0, Arg1>
where
    Arg0: NomReader + Debug + PartialEq + Eq,
    Arg1: NomReader + Debug + PartialEq + Eq,
{
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        alt((
            map(
                MichelinePrim1ArgNoAnnots::<_, { prim::LEFT_TAG }>::nom_read,
                |MichelinePrim1ArgNoAnnots { arg }| Self::Left(arg),
            ),
            map(
                MichelinePrim1ArgNoAnnots::<_, { prim::RIGHT_TAG }>::nom_read,
                |MichelinePrim1ArgNoAnnots { arg }| Self::Right(arg),
            ),
        ))(input)
    }
}

impl<Arg> NomReader for MichelsonOption<Arg>
where
    Arg: NomReader + Debug + PartialEq + Eq,
{
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        alt((
            map(
                MichelinePrimNoArgsNoAnnots::<{ prim::NONE_TAG }>::nom_read,
                |_prim| Self(None),
            ),
            map(
                MichelinePrim1ArgNoAnnots::<_, { prim::SOME_TAG }>::nom_read,
                |MichelinePrim1ArgNoAnnots { arg }| Self(Some(arg)),
            ),
        ))(input)
    }
}

impl NomReader for MichelsonString {
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        map(nom_read_micheline_string, MichelsonString)(input)
    }
}

impl NomReader for MichelsonBytes {
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        map(nom_read_micheline_bytes(nom_read::bytes), MichelsonBytes)(input)
    }
}

impl NomReader for MichelsonInt {
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        map(nom_read_micheline_int, MichelsonInt)(input)
    }
}

impl NomReader for MichelsonNat {
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        use nom::error::{ErrorKind, ParseError};
        use tezos_data_encoding::nom::error::*;

        let (rest, i) = nom_read_micheline_int(input)?;
        if i.0 >= 0.into() {
            Ok((rest, MichelsonNat(i)))
        } else {
            Err(nom::Err::Error(DecodeError::from_error_kind(
                input,
                ErrorKind::MapRes,
            )))
        }
    }
}

// --------
// ENCODING
// --------
impl BinWriter for MichelsonContract {
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        bin_write_micheline_bytes(Contract::bin_write)(&self.0, output)
    }
}

impl BinWriter for MichelsonUnit {
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        bin_write_prim_no_args_no_annots(prim::UNIT_TAG, output)
    }
}

impl<Arg0, Arg1> BinWriter for MichelsonPair<Arg0, Arg1>
where
    Arg0: BinWriter + Debug + PartialEq + Eq,
    Arg1: BinWriter + Debug + PartialEq + Eq,
{
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        bin_write_prim_2_args_no_annots(prim::PAIR_TAG, &self.0, &self.1, output)
    }
}

impl<Arg0, Arg1> From<MichelinePrim2ArgsNoAnnots<Arg0, Arg1, { prim::PAIR_TAG }>>
    for MichelsonPair<Arg0, Arg1>
where
    Arg0: Debug + PartialEq + Eq,
    Arg1: Debug + PartialEq + Eq,
{
    fn from(
        micheline: MichelinePrim2ArgsNoAnnots<Arg0, Arg1, { prim::PAIR_TAG }>,
    ) -> Self {
        Self(micheline.arg1, micheline.arg2)
    }
}

impl<Arg0, Arg1> From<MichelsonPair<Arg0, Arg1>>
    for MichelinePrim2ArgsNoAnnots<Arg0, Arg1, { prim::PAIR_TAG }>
where
    Arg0: Debug + PartialEq + Eq,
    Arg1: Debug + PartialEq + Eq,
{
    fn from(michelson: MichelsonPair<Arg0, Arg1>) -> Self {
        Self {
            arg1: michelson.0,
            arg2: michelson.1,
        }
    }
}

impl<Arg0, Arg1> BinWriter for MichelsonOr<Arg0, Arg1>
where
    Arg0: BinWriter + Debug + PartialEq + Eq,
    Arg1: BinWriter + Debug + PartialEq + Eq,
{
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        match self {
            MichelsonOr::Left(left) => {
                bin_write_prim_1_arg_no_annots(prim::LEFT_TAG, left, output)
            }
            MichelsonOr::Right(right) => {
                bin_write_prim_1_arg_no_annots(prim::RIGHT_TAG, right, output)
            }
        }
    }
}

impl<Arg0, Arg1> From<MichelinePrim1ArgNoAnnots<Arg0, { prim::LEFT_TAG }>>
    for MichelsonOr<Arg0, Arg1>
where
    Arg0: Debug + PartialEq + Eq,
    Arg1: Debug + PartialEq + Eq,
{
    fn from(micheline: MichelinePrim1ArgNoAnnots<Arg0, { prim::LEFT_TAG }>) -> Self {
        Self::Left(micheline.arg)
    }
}

impl<Arg0, Arg1> From<MichelinePrim1ArgNoAnnots<Arg1, { prim::RIGHT_TAG }>>
    for MichelsonOr<Arg0, Arg1>
where
    Arg0: Debug + PartialEq + Eq,
    Arg1: Debug + PartialEq + Eq,
{
    fn from(micheline: MichelinePrim1ArgNoAnnots<Arg1, { prim::RIGHT_TAG }>) -> Self {
        Self::Right(micheline.arg)
    }
}

impl<Arg> BinWriter for MichelsonOption<Arg>
where
    Arg: BinWriter + Debug + PartialEq + Eq,
{
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        match self {
            MichelsonOption(None) => {
                bin_write_prim_no_args_no_annots(prim::NONE_TAG, output)
            }
            MichelsonOption(Some(arg)) => {
                bin_write_prim_1_arg_no_annots(prim::SOME_TAG, arg, output)
            }
        }
    }
}

impl BinWriter for MichelsonString {
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        bin_write_micheline_string(&self.0, output)
    }
}

impl BinWriter for MichelsonBytes {
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        bin_write_micheline_bytes(enc::bytes)(self.0.as_slice(), output)
    }
}

impl BinWriter for MichelsonInt {
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        bin_write_micheline_int(&self.0, output)
    }
}
impl BinWriter for MichelsonNat {
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        bin_write_micheline_int(&self.0, output)
    }
}

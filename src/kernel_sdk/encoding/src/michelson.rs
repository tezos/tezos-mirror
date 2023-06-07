// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-License-Identifier: MIT

//! Definitions & tezos-encodings for *michelson* data.
use nom::combinator::map;
use std::fmt::Debug;
use tezos_data_encoding::enc::{self, BinResult, BinWriter};
use tezos_data_encoding::encoding::{Encoding, HasEncoding};
use tezos_data_encoding::nom::{self as nom_read, NomReader, NomResult};
use tezos_data_encoding::types::Zarith;

mod micheline;
#[cfg(feature = "alloc")]
pub mod ticket;

use super::contract::Contract;
use micheline::{
    bin_write_micheline_bytes, bin_write_micheline_int, bin_write_micheline_string,
    bin_write_prim_2_args_no_annots, bin_write_prim_no_args_no_annots,
    nom_read_micheline_bytes, nom_read_micheline_int, nom_read_micheline_string,
    MichelinePrim2ArgsNoAnnots, MichelinePrimNoArgsNoAnnots,
};
use v1_primitives as prim;

pub mod v1_primitives {
    //! Encoding of [michelson_v1_primitives].
    //!
    //! [michelson_v1_primitives]: <https://gitlab.com/tezos/tezos/-/blob/9028b797894a5d9db38bc61a20abb793c3778316/src/proto_alpha/lib_protocol/michelson_v1_primitives.ml>

    /// `("Pair", D_PAIR)` case tag.
    pub const PAIR_TAG: u8 = 7;

    /// unit encoding case tag.
    pub const UNIT_TAG: u8 = 11;
}

/// marker trait for michelson encoding
pub trait Michelson:
    HasEncoding + BinWriter + NomReader + Debug + PartialEq + Eq
{
}

impl Michelson for MichelsonUnit {}
impl Michelson for MichelsonContract {}
impl Michelson for MichelsonInt {}
impl Michelson for MichelsonString {}
impl Michelson for MichelsonBytes {}
impl<Arg0, Arg1> Michelson for MichelsonPair<Arg0, Arg1>
where
    Arg0: Michelson,
    Arg1: Michelson,
{
}

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

/// Michelson String encoding.
#[derive(Debug, PartialEq, Eq)]
pub struct MichelsonString(pub String);

/// Michelson Bytes encoding.
#[derive(Debug, PartialEq, Eq)]
pub struct MichelsonBytes(pub Vec<u8>);

/// Michelson Int encoding.
#[derive(Debug, PartialEq, Eq)]
pub struct MichelsonInt(pub Zarith);

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
        bin_write_prim_no_args_no_annots::<{ prim::UNIT_TAG }>(output)
    }
}

impl<Arg0, Arg1> BinWriter for MichelsonPair<Arg0, Arg1>
where
    Arg0: BinWriter + Debug + PartialEq + Eq,
    Arg1: BinWriter + Debug + PartialEq + Eq,
{
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        bin_write_prim_2_args_no_annots::<_, _, { prim::PAIR_TAG }>(
            &self.0, &self.1, output,
        )
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

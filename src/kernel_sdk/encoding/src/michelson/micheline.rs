// SPDX-FileCopyrightText: 2022 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Definitions & tezos-encodings for *micheline* structures.
//!
//! Encodings & constants taken from [lib_micheline].
//!
//! [lib_micheline]: <https://gitlab.com/tezos/tezos/-/blob/9028b797894a5d9db38bc61a20abb793c3778316/src/lib_micheline/micheline_encoding.ml>
use nom::bytes::complete::tag;
use nom::combinator::map;
use nom::sequence::{pair, preceded};
use tezos_data_encoding::enc::{self, BinResult, BinSerializer, BinWriter};
use tezos_data_encoding::encoding::{Encoding, HasEncoding};
use tezos_data_encoding::has_encoding;
use tezos_data_encoding::nom::{self as nom_read, NomInput, NomReader, NomResult};
use tezos_data_encoding::types::Zarith;

use std::fmt::Debug;

/// Int encoding case tag.
pub const MICHELINE_INT_TAG: u8 = 0;
/// String encoding case tag.
pub const MICHELINE_STRING_TAG: u8 = 1;
/// no-argument primitive (without annotations) encoding case tag.
pub const MICHELINE_PRIM_NO_ARGS_NO_ANNOTS_TAG: u8 = 3;
/// no-argument primitive (with annotations) encoding case tag.
pub const MICHELINE_PRIM_NO_ARGS_SOME_ANNOTS_TAG: u8 = 4;
/// 1-argument primitive (without annotations) encoding case tag.
pub const MICHELINE_PRIM_1_ARG_NO_ANNOTS_TAG: u8 = 5;
/// 1-argument primitive (with annotations) encoding case tag.
pub const MICHELINE_PRIM_1_ARG_SOME_ANNOTS_TAG: u8 = 6;
/// 2-argument primitive (without annotations) encoding case tag.
pub const MICHELINE_PRIM_2_ARGS_NO_ANNOTS_TAG: u8 = 7;
/// 2-argument primitive (with annotations) encoding case tag.
pub const MICHELINE_PRIM_2_ARGS_SOME_ANNOTS_TAG: u8 = 8;
/// Bytes encoding case tag.
pub const MICHELINE_BYTES_TAG: u8 = 10;

// -----
// TYPES
// -----

/// lib_micheline *int* encoding.
///
/// Encoded as [Zarith] prefixed by [MICHELINE_INT_TAG].
#[derive(Debug, PartialEq, Eq)]
pub struct MichelineInt(pub Zarith);

/// lib_micheline *string* encoding.
///
/// Encoded as dynamically-sized string prefixed by [MICHELINE_STRING_TAG].
#[derive(Debug, PartialEq, Eq)]
pub struct MichelineString(pub String);

/// lib_micheline *bytes* encoding.
///
/// Encoded as dynamically-sized bytes prefixed by [MICHELINE_BYTES_TAG].
#[derive(Debug, PartialEq, Eq)]
pub struct MichelineBytes(pub Vec<u8>);

/// lib_micheline *prim-no args no annotations* encoding.
///
/// Encoded as an `obj1`, prefixed by [MICHELINE_PRIM_NO_ARGS_NO_ANNOTS_TAG], with field:
/// - `prim` - the `PRIM_TAG`
#[derive(Debug, PartialEq, Eq)]
pub struct MichelinePrimNoArgsNoAnnots<const PRIM_TAG: u8>;

/// lib_micheline *prim-no args some annotations* encoding.
///
/// Encoded as an `obj2`, prefixed by [MICHELINE_PRIM_NO_ARGS_SOME_ANNOTS_TAG], with field:
/// - `prim` - the `PRIM_TAG`
/// - `annots` - the annotations
#[derive(Debug, PartialEq, Eq)]
pub struct MichelinePrimNoArgsSomeAnnots<const PRIM_TAG: u8> {
    pub(crate) annots: String,
}

/// lib_micheline *prim-1 no annotations* encoding.
///
/// Encoded as an `obj2`, prefixed by [MICHELINE_PRIM_1_ARG_NO_ANNOTS_TAG], with fields:
/// - `prim` - the `PRIM_TAG`
/// - `arg` - the argument
#[derive(Debug, PartialEq, Eq)]
pub struct MichelinePrim1ArgNoAnnots<Arg, const PRIM_TAG: u8>
where
    Arg: Debug + PartialEq + Eq,
{
    pub(crate) arg: Arg,
}

/// lib_micheline *prim-1 some annotations* encoding.
///
/// Encoded as an `obj3`, prefixed by [MICHELINE_PRIM_1_ARG_SOME_ANNOTS_TAG], with fields:
/// - `prim` - the `PRIM_TAG`
/// - `arg` - the argument
/// - `annots` - the annotations
#[derive(Debug, PartialEq, Eq)]
pub struct MichelinePrim1ArgSomeAnnots<Arg, const PRIM_TAG: u8>
where
    Arg: Debug + PartialEq + Eq,
{
    pub(crate) arg: Arg,
    pub(crate) annots: String,
}

/// lib_micheline *prim-2 no annotations* encoding.
///
/// Encoded as an `obj3`, prefixed by [MICHELINE_PRIM_2_ARGS_NO_ANNOTS_TAG], with fields:
/// - `prim` - the `PRIM_TAG`
/// - `arg1` - the first argument
/// - `arg2` - the second argument
#[derive(Debug, PartialEq, Eq)]
pub struct MichelinePrim2ArgsNoAnnots<Arg1, Arg2, const PRIM_TAG: u8>
where
    Arg1: Debug + PartialEq + Eq,
    Arg2: Debug + PartialEq + Eq,
{
    pub(crate) arg1: Arg1,
    pub(crate) arg2: Arg2,
}

/// lib_micheline *prim-2 some annotations* encoding.
///
/// Encoded as an `obj4`, prefixed by [MICHELINE_PRIM_2_ARGS_SOME_ANNOTS_TAG], with fields:
/// - `prim` - the `PRIM_TAG`
/// - `arg1` - the first argument
/// - `arg2` - the second argument
/// - `annots` - the annotations
#[derive(Debug, PartialEq, Eq)]
pub struct MichelinePrim2ArgsSomeAnnots<Arg1, Arg2, const PRIM_TAG: u8>
where
    Arg1: Debug + PartialEq + Eq,
    Arg2: Debug + PartialEq + Eq,
{
    pub(crate) arg1: Arg1,
    pub(crate) arg2: Arg2,
    pub(crate) annots: String,
}

// ----------
// CONVERSION
// ----------
impl From<i32> for MichelineInt {
    fn from(int: i32) -> Self {
        MichelineInt(Zarith(int.into()))
    }
}

// --------
// ENCODING
// --------
has_encoding!(MichelineInt, MICHELINE_INT_ENCODING, { Encoding::Custom });
has_encoding!(MichelineString, MICHELINE_STRING_ENCODING, {
    Encoding::Custom
});
has_encoding!(MichelineBytes, MICHELINE_BYTES_ENCODING, {
    Encoding::Custom
});

impl<const PRIM_TAG: u8> HasEncoding for MichelinePrimNoArgsNoAnnots<PRIM_TAG> {
    fn encoding() -> Encoding {
        Encoding::Custom
    }
}

impl<const PRIM_TAG: u8> HasEncoding for MichelinePrimNoArgsSomeAnnots<PRIM_TAG> {
    fn encoding() -> Encoding {
        Encoding::Custom
    }
}

impl<Arg, const PRIM_TAG: u8> HasEncoding for MichelinePrim1ArgNoAnnots<Arg, PRIM_TAG>
where
    Arg: Debug + PartialEq + Eq,
{
    fn encoding() -> Encoding {
        Encoding::Custom
    }
}

impl<Arg, const PRIM_TAG: u8> HasEncoding for MichelinePrim1ArgSomeAnnots<Arg, PRIM_TAG>
where
    Arg: Debug + PartialEq + Eq,
{
    fn encoding() -> Encoding {
        Encoding::Custom
    }
}

impl<Arg1, Arg2, const PRIM_TAG: u8> HasEncoding
    for MichelinePrim2ArgsNoAnnots<Arg1, Arg2, PRIM_TAG>
where
    Arg1: Debug + PartialEq + Eq,
    Arg2: Debug + PartialEq + Eq,
{
    fn encoding() -> Encoding {
        Encoding::Custom
    }
}

impl<Arg1, Arg2, const PRIM_TAG: u8> HasEncoding
    for MichelinePrim2ArgsSomeAnnots<Arg1, Arg2, PRIM_TAG>
where
    Arg1: Debug + PartialEq + Eq,
    Arg2: Debug + PartialEq + Eq,
{
    fn encoding() -> Encoding {
        Encoding::Custom
    }
}

// ----------
// NOM_READER
// ----------
impl NomReader for MichelineInt {
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        map(nom_read_micheline_int, MichelineInt)(input)
    }
}

impl NomReader for MichelineString {
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        map(nom_read_micheline_string, MichelineString)(input)
    }
}

impl NomReader for MichelineBytes {
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        map(nom_read_micheline_bytes(nom_read::bytes), MichelineBytes)(input)
    }
}

impl<const PRIM_TAG: u8> NomReader for MichelinePrimNoArgsNoAnnots<PRIM_TAG> {
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        map(
            tag([MICHELINE_PRIM_NO_ARGS_NO_ANNOTS_TAG, PRIM_TAG]),
            |_prim| MichelinePrimNoArgsNoAnnots {},
        )(input)
    }
}

impl<const PRIM_TAG: u8> NomReader for MichelinePrimNoArgsSomeAnnots<PRIM_TAG> {
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        let parse = preceded(
            tag([MICHELINE_PRIM_NO_ARGS_SOME_ANNOTS_TAG, PRIM_TAG]),
            nom_read::string,
        );

        map(parse, |annots| MichelinePrimNoArgsSomeAnnots { annots })(input)
    }
}

impl<Arg, const PRIM_TAG: u8> NomReader for MichelinePrim1ArgNoAnnots<Arg, PRIM_TAG>
where
    Arg: NomReader + Debug + PartialEq + Eq,
{
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        let parse = preceded(
            tag([MICHELINE_PRIM_1_ARG_NO_ANNOTS_TAG, PRIM_TAG]),
            Arg::nom_read,
        );

        map(parse, |arg| MichelinePrim1ArgNoAnnots { arg })(input)
    }
}

impl<Arg, const PRIM_TAG: u8> NomReader for MichelinePrim1ArgSomeAnnots<Arg, PRIM_TAG>
where
    Arg: NomReader + Debug + PartialEq + Eq,
{
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        let parse = preceded(
            tag([MICHELINE_PRIM_1_ARG_SOME_ANNOTS_TAG, PRIM_TAG]),
            pair(Arg::nom_read, nom_read::string),
        );

        map(parse, |(arg, annots)| MichelinePrim1ArgSomeAnnots {
            arg,
            annots,
        })(input)
    }
}

impl<Arg1, Arg2, const PRIM_TAG: u8> NomReader
    for MichelinePrim2ArgsNoAnnots<Arg1, Arg2, PRIM_TAG>
where
    Arg1: NomReader + Debug + PartialEq + Eq,
    Arg2: NomReader + Debug + PartialEq + Eq,
{
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        let parse = preceded(
            tag([MICHELINE_PRIM_2_ARGS_NO_ANNOTS_TAG, PRIM_TAG]),
            pair(Arg1::nom_read, Arg2::nom_read),
        );

        map(parse, |(arg1, arg2)| MichelinePrim2ArgsNoAnnots {
            arg1,
            arg2,
        })(input)
    }
}

impl<Arg1, Arg2, const PRIM_TAG: u8> NomReader
    for MichelinePrim2ArgsSomeAnnots<Arg1, Arg2, PRIM_TAG>
where
    Arg1: NomReader + Debug + PartialEq + Eq,
    Arg2: NomReader + Debug + PartialEq + Eq,
{
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        let parse = preceded(
            tag([MICHELINE_PRIM_2_ARGS_SOME_ANNOTS_TAG, PRIM_TAG]),
            pair(Arg1::nom_read, pair(Arg2::nom_read, nom_read::string)),
        );

        map(parse, |(arg1, (arg2, annots))| {
            MichelinePrim2ArgsSomeAnnots { arg1, arg2, annots }
        })(input)
    }
}

// ----------
// BIN_WRITER
// ----------
impl BinWriter for MichelineInt {
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        enc::put_byte(&MICHELINE_INT_TAG, output);
        self.0.bin_write(output)
    }
}

impl BinWriter for MichelineString {
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        enc::put_byte(&MICHELINE_STRING_TAG, output);
        enc::string(&self.0, output)
    }
}

impl BinWriter for MichelineBytes {
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        bin_write_micheline_bytes(enc::bytes)(self.0.as_slice(), output)
    }
}

impl<const PRIM_TAG: u8> BinWriter for MichelinePrimNoArgsNoAnnots<PRIM_TAG> {
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        bin_write_prim_no_args_no_annots::<{ PRIM_TAG }>(output)
    }
}

impl<const PRIM_TAG: u8> BinWriter for MichelinePrimNoArgsSomeAnnots<PRIM_TAG> {
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        bin_write_prim_no_args_some_annots::<{ PRIM_TAG }>(&self.annots, output)
    }
}

impl<Arg, const PRIM_TAG: u8> BinWriter for MichelinePrim1ArgNoAnnots<Arg, PRIM_TAG>
where
    Arg: BinWriter + Debug + PartialEq + Eq,
{
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        bin_write_prim_1_arg_no_annots::<_, { PRIM_TAG }>(&self.arg, output)
    }
}

impl<Arg, const PRIM_TAG: u8> BinWriter for MichelinePrim1ArgSomeAnnots<Arg, PRIM_TAG>
where
    Arg: BinWriter + Debug + PartialEq + Eq,
{
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        bin_write_prim_1_arg_some_annots::<_, { PRIM_TAG }>(
            &self.arg,
            &self.annots,
            output,
        )
    }
}

impl<Arg1, Arg2, const PRIM_TAG: u8> BinWriter
    for MichelinePrim2ArgsNoAnnots<Arg1, Arg2, PRIM_TAG>
where
    Arg1: BinWriter + Debug + PartialEq + Eq,
    Arg2: BinWriter + Debug + PartialEq + Eq,
{
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        bin_write_prim_2_args_no_annots::<_, _, { PRIM_TAG }>(
            &self.arg1, &self.arg2, output,
        )
    }
}

impl<Arg1, Arg2, const PRIM_TAG: u8> BinWriter
    for MichelinePrim2ArgsSomeAnnots<Arg1, Arg2, PRIM_TAG>
where
    Arg1: BinWriter + Debug + PartialEq + Eq,
    Arg2: BinWriter + Debug + PartialEq + Eq,
{
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        bin_write_prim_2_args_some_annots::<_, _, { PRIM_TAG }>(
            &self.arg1,
            &self.arg2,
            &self.annots,
            output,
        )
    }
}

// ---------------------------
// Deserialization Combinators
// ---------------------------

/// Read element with a prefix of [TAG] into `parser`.
fn nom_read_tagged_micheline<'a, T: Clone, const TAG: u8>(
    parser: impl FnMut(NomInput<'a>) -> NomResult<'a, T>,
) -> impl FnMut(NomInput<'a>) -> NomResult<'a, T> {
    preceded(tag([TAG]), parser)
}

/// Read dynamically-sized bytes with a prefix of [MICHELINE_BYTES_TAG] into `parser`.
pub(crate) fn nom_read_micheline_bytes<'a, T: Clone>(
    parser: impl FnMut(NomInput) -> NomResult<T>,
) -> impl FnMut(NomInput<'a>) -> NomResult<'a, T> {
    nom_read_tagged_micheline::<_, { MICHELINE_BYTES_TAG }>(nom_read::dynamic(parser))
}

/// Read string with a prefix of [MICHELINE_STRING_TAG].
pub(crate) fn nom_read_micheline_string(input: NomInput) -> NomResult<String> {
    nom_read_tagged_micheline::<_, { MICHELINE_STRING_TAG }>(nom_read::string)(input)
}

/// Read int with a prefix of [MICHELINE_INT_TAG].
pub(crate) fn nom_read_micheline_int(input: NomInput) -> NomResult<Zarith> {
    nom_read_tagged_micheline::<_, { MICHELINE_INT_TAG }>(Zarith::nom_read)(input)
}

// -------------------------
// Serialization Combinators
// -------------------------
/// Write `PRIM_TAG` into an `obj1` encoding, prefixed with the
/// [MICHELINE_PRIM_NO_ARGS_NO_ANNOTS_TAG].
pub(crate) fn bin_write_prim_no_args_no_annots<const PRIM_TAG: u8>(
    output: &mut Vec<u8>,
) -> BinResult {
    enc::put_bytes(&[MICHELINE_PRIM_NO_ARGS_NO_ANNOTS_TAG, PRIM_TAG], output);
    Ok(())
}

/// Write `PRIM_TAG` & `annots` into an `obj2` encoding, prefixed with the
/// [MICHELINE_PRIM_NO_ARGS_SOME_ANNOTS_TAG].
pub(crate) fn bin_write_prim_no_args_some_annots<const PRIM_TAG: u8>(
    annots: &String,
    output: &mut Vec<u8>,
) -> BinResult {
    enc::put_bytes(&[MICHELINE_PRIM_NO_ARGS_SOME_ANNOTS_TAG, PRIM_TAG], output);

    enc::string(annots, output)?;

    Ok(())
}

/// Write `PRIM_TAG` & `arg` into an `obj2` encoding, prefixed with the
/// [MICHELINE_PRIM_1_ARG_NO_ANNOTS_TAG].
pub(crate) fn bin_write_prim_1_arg_no_annots<Arg, const PRIM_TAG: u8>(
    arg: &Arg,
    output: &mut Vec<u8>,
) -> BinResult
where
    Arg: BinWriter,
{
    enc::put_bytes(&[MICHELINE_PRIM_1_ARG_NO_ANNOTS_TAG, PRIM_TAG], output);

    arg.bin_write(output)?;

    Ok(())
}

/// Write `PRIM_TAG`, `arg` & `annots` into an `obj3` encoding, prefixed with the
/// [MICHELINE_PRIM_1_ARG_SOME_ANNOTS_TAG].
pub(crate) fn bin_write_prim_1_arg_some_annots<Arg, const PRIM_TAG: u8>(
    arg: &Arg,
    annots: &String,
    output: &mut Vec<u8>,
) -> BinResult
where
    Arg: BinWriter,
{
    enc::put_bytes(&[MICHELINE_PRIM_1_ARG_SOME_ANNOTS_TAG, PRIM_TAG], output);

    arg.bin_write(output)?;
    enc::string(annots, output)?;

    Ok(())
}

/// Write `PRIM_TAG`, `arg1` & `arg2` into an `obj3` encoding, prefixed with the
/// [MICHELINE_PRIM_2_ARGS_NO_ANNOTS_TAG].
pub(crate) fn bin_write_prim_2_args_no_annots<Arg1, Arg2, const PRIM_TAG: u8>(
    arg1: &Arg1,
    arg2: &Arg2,
    output: &mut Vec<u8>,
) -> BinResult
where
    Arg1: BinWriter,
    Arg2: BinWriter,
{
    enc::put_bytes(&[MICHELINE_PRIM_2_ARGS_NO_ANNOTS_TAG, PRIM_TAG], output);

    arg1.bin_write(output)?;
    arg2.bin_write(output)?;

    Ok(())
}

/// Write `PRIM_TAG`, `arg1`, `arg2` & `annots` into an `obj4` encoding, prefixed with the
/// [MICHELINE_PRIM_2_ARGS_SOME_ANNOTS_TAG].
pub(crate) fn bin_write_prim_2_args_some_annots<Arg1, Arg2, const PRIM_TAG: u8>(
    arg1: &Arg1,
    arg2: &Arg2,
    annots: &String,
    output: &mut Vec<u8>,
) -> BinResult
where
    Arg1: BinWriter,
    Arg2: BinWriter,
{
    enc::put_bytes(&[MICHELINE_PRIM_2_ARGS_SOME_ANNOTS_TAG, PRIM_TAG], output);

    arg1.bin_write(output)?;
    arg2.bin_write(output)?;
    enc::string(annots, output)?;

    Ok(())
}

/// Write `TAG`, then the data using `writer`.
fn bin_write_tagged_micheline<T, const TAG: u8>(
    writer: impl FnOnce(T, &mut Vec<u8>) -> BinResult,
    data: T,
    output: &mut Vec<u8>,
) -> BinResult {
    enc::put_byte(&TAG, output);
    writer(data, output)
}

/// Write the byte-slice given by `bytes(T)` into dynamically-sized bytes, prefixed by
/// [MICHELINE_BYTES_TAG].
pub(crate) fn bin_write_micheline_bytes<T>(
    bytes: impl BinSerializer<T>,
) -> impl FnOnce(T, &mut Vec<u8>) -> BinResult {
    move |data, output| {
        bin_write_tagged_micheline::<_, { MICHELINE_BYTES_TAG }>(
            enc::dynamic(bytes),
            data,
            output,
        )
    }
}

/// Write the string into dynamically-sized string, prefixed by
/// [MICHELINE_STRING_TAG].
pub(crate) fn bin_write_micheline_string(
    data: &impl AsRef<str>,
    output: &mut Vec<u8>,
) -> BinResult {
    bin_write_tagged_micheline::<_, { MICHELINE_STRING_TAG }>(enc::string, data, output)
}

/// Write the zarith into an int, prefixed by [MICHELINE_INT_TAG].
pub(crate) fn bin_write_micheline_int(data: &Zarith, output: &mut Vec<u8>) -> BinResult {
    bin_write_tagged_micheline::<_, { MICHELINE_INT_TAG }>(
        Zarith::bin_write,
        data,
        output,
    )
}

#[cfg(test)]
mod test {
    use super::*;

    // z_bignum test cases from tezedge/tezos-encoding, prefixed by
    // the micheline_int tag (0)
    const MICHELINE_INT_ENCODING: &[(&str, &str)] = &[
        ("0", "0000"),
        ("1", "0001"),
        ("7f", "00bf01"),
        ("80", "008002"),
        ("81", "008102"),
        ("ff", "00bf03"),
        ("100", "008004"),
        ("101", "008104"),
        ("7fff", "00bfff03"),
        ("8000", "00808004"),
        ("8001", "00818004"),
        ("ffff", "00bfff07"),
        ("10000", "00808008"),
        ("10001", "00818008"),
        ("9da879e", "009e9ed49d01"),
    ];

    #[test]
    fn micheline_int_encode() {
        for (hex, enc) in MICHELINE_INT_ENCODING {
            let num = hex_to_bigint(hex);
            let enc = hex::decode(enc).unwrap();

            let micheline_int = MichelineInt(Zarith(num));

            let mut bin = Vec::new();
            micheline_int
                .bin_write(&mut bin)
                .expect("serialization should work");

            assert_eq!(bin, enc);
        }
    }

    #[test]
    fn micheline_int_decode() {
        for (hex, enc) in MICHELINE_INT_ENCODING {
            let num = hex_to_bigint(hex);
            let input = hex::decode(enc).unwrap();
            let (input, dec) = MichelineInt::nom_read(&input).unwrap();

            assert!(input.is_empty());

            let micheline_int = MichelineInt(Zarith(num));

            assert_eq!(micheline_int, dec);
        }
    }

    #[test]
    fn micheline_string_encode() {
        let test = "the quick brown fox jumps over the lazy dog";
        let mut expected = vec![1, 0, 0, 0, 43];
        expected.append(&mut test.as_bytes().to_vec());

        let mut bin = Vec::new();

        MichelineString(test.into()).bin_write(&mut bin).unwrap();

        assert_eq!(expected, bin);
    }

    #[test]
    fn micheline_string_decode() {
        let expected = "Little by little, one travels far.";
        let mut test = vec![1, 0, 0, 0, 34];
        test.append(&mut expected.as_bytes().to_vec());

        let (input_remaining, value) =
            MichelineString::nom_read(test.as_slice()).unwrap();

        assert!(input_remaining.is_empty());
        assert_eq!(MichelineString(expected.into()), value);
    }

    #[test]
    fn micheline_bytes_encode() {
        let test = "hello".as_bytes().to_vec();
        let expected = vec![
            b'\n', // MICHELINE_BYTES_TAG
            0, 0, 0, 5, // SIZE
            b'h', b'e', b'l', b'l', b'o',
        ];

        let bytes = MichelineBytes(test);

        let mut bin = Vec::new();
        bytes.bin_write(&mut bin).unwrap();

        assert_eq!(expected, bin);
    }

    #[test]
    fn micheline_bytes_decode() {
        let expected = MichelineBytes("world".as_bytes().to_vec());
        let test = vec![
            b'\n', // MICHELINE_BYTES_TAG
            0, 0, 0, 5, // SIZE
            b'w', b'o', b'r', b'l', b'd',
        ];

        let (remaining_input, bytes) = MichelineBytes::nom_read(test.as_slice()).unwrap();

        assert!(remaining_input.is_empty());
        assert_eq!(expected, bytes);
    }

    #[test]
    fn micheline_unit_decode() {
        let test = vec![
            3,  // Prim_0 (no annots)
            11, // Prim tag: Unit
        ];

        let expected = MichelinePrimNoArgsNoAnnots::<11> {};

        let (remaining_input, unit) = NomReader::nom_read(test.as_slice()).unwrap();

        assert!(remaining_input.is_empty());
        assert_eq!(expected, unit);
    }

    #[test]
    fn micheline_unit_encode() {
        let expected = vec![
            3,  // Prim_0 (no annots)
            11, // Prim tag: Unit
        ];

        let test = MichelinePrimNoArgsNoAnnots::<11> {};

        let mut bin = Vec::new();
        test.bin_write(&mut bin).unwrap();

        assert_eq!(expected, bin);
    }

    #[test]
    fn micheline_nat_annot_decode() {
        // Decode `nat :foo`
        let test = vec![
            4,  // Prim_0 (some annots)
            98, // Prim tag: nat
            0, 0, 0, 4, // length of the annotation string
            b':', b'f', b'o', b'o', // annotation
        ];

        let expected = MichelinePrimNoArgsSomeAnnots::<98> {
            annots: ":foo".into(),
        };

        let (remaining_input, natfoo) = NomReader::nom_read(test.as_slice()).unwrap();

        assert!(remaining_input.is_empty());
        assert_eq!(expected, natfoo);
    }

    #[test]
    fn micheline_nat_annot_encode() {
        let expected = vec![
            4,  // Prim_0 (some annots)
            98, // Prim tag: nat
            0, 0, 0, 4, // length of the annotation string
            b':', b'f', b'o', b'o', // annotation
        ];

        let test = MichelinePrimNoArgsSomeAnnots::<98> {
            annots: ":foo".into(),
        };

        let mut bin = Vec::new();
        test.bin_write(&mut bin).unwrap();

        assert_eq!(expected, bin);
    }

    #[test]
    fn micheline_some_decode() {
        let test = vec![
            5, // Prim_1 (no annots)
            9, // Prim tag: Some
            1, // String tag
            0, 0, 0, 3, // String size
            b'r', b'e', b'd', // string contents
        ];

        let expected = MichelinePrim1ArgNoAnnots::<_, 9> {
            arg: MichelineString("red".into()),
        };

        let (remaining_input, some) = NomReader::nom_read(test.as_slice()).unwrap();

        assert!(remaining_input.is_empty());
        assert_eq!(expected, some);
    }

    #[test]
    fn micheline_some_encode() {
        let expected = vec![
            5, // Prim_1 (no annots)
            9, // Prim tag: Some
            0, // int encoding tag
            2, // amount
        ];

        let test = MichelinePrim1ArgNoAnnots::<MichelineInt, 9> { arg: 2.into() };

        let mut bin = Vec::new();
        test.bin_write(&mut bin).unwrap();

        assert_eq!(expected, bin);
    }

    #[test]
    fn micheline_option_annot_decode() {
        // Decode `option :foo nat`
        let test = vec![
            6,  // Prim_1 (some annots)
            99, // Prim tag: option
            3,  // Prim 0 (no annots)
            98, // Prim tag: nat
            0, 0, 0, 4, // length of the annotation string
            b':', b'f', b'o', b'o', // annotation
        ];

        let nat = MichelinePrimNoArgsNoAnnots::<98> {};
        let expected = MichelinePrim1ArgSomeAnnots::<_, 99> {
            arg: nat,
            annots: ":foo".into(),
        };

        let (remaining_input, optnatfoo) = NomReader::nom_read(test.as_slice()).unwrap();

        assert!(remaining_input.is_empty());
        assert_eq!(expected, optnatfoo);
    }

    #[test]
    fn micheline_option_annot_encode() {
        let expected = vec![
            6,  // Prim_1 (some annots)
            99, // Prim tag: option
            3,  // Prim 0 (no annots)
            98, // Prim tag: nat
            0, 0, 0, 4, // length of the annotation string
            b':', b'f', b'o', b'o', // annotation
        ];

        let nat = MichelinePrimNoArgsNoAnnots::<98> {};
        let test = MichelinePrim1ArgSomeAnnots::<_, 99> {
            arg: nat,
            annots: ":foo".into(),
        };

        let mut bin = Vec::new();
        test.bin_write(&mut bin).unwrap();

        assert_eq!(expected, bin);
    }

    #[test]
    fn micheline_pair_decode() {
        let test = vec![
            7, // Prim_2 (no annots)
            7, // Prim tag: Pair
            1, // String tag
            0, 0, 0, 3, // String size
            b'r', b'e', b'd', // string contents
            0,    // int encoding tag
            1,    // amount
            4, b'!', // remaining input
        ];

        let expected = MichelinePrim2ArgsNoAnnots::<_, MichelineInt, 7> {
            arg1: MichelineString("red".into()),
            arg2: 1.into(),
        };

        let (remaining_input, pair) = NomReader::nom_read(test.as_slice()).unwrap();

        assert_eq!(&[4, b'!'], remaining_input);
        assert_eq!(expected, pair);
    }

    #[test]
    fn micheline_pair_encode() {
        let expected = vec![
            7, // Prim_2 (no annots)
            7, // Prim tag: Pair
            0, // int encoding tag
            2, // amount
            1, // String tag
            0, 0, 0, 5, // String size
            b'g', b'r', b'e', b'e', b'n', // string contents
        ];

        let test = MichelinePrim2ArgsNoAnnots::<MichelineInt, _, 7> {
            arg1: 2.into(),
            arg2: MichelineString("green".into()),
        };

        let mut bin = Vec::new();
        test.bin_write(&mut bin).unwrap();

        assert_eq!(expected, bin);
    }

    #[test]
    fn micheline_or_annot_decode() {
        // Decode `or :foo nat int`
        let test = vec![
            8,   // Prim_2 (some annots)
            100, // Prim tag: or
            3,   // Prim 0 (no annots)
            98,  // Prim tag: nat
            3,   // Prim 0 (no annots)
            91,  // Prim tag: int
            0, 0, 0, 4, // length of the annotation string
            b':', b'f', b'o', b'o', // annotation
        ];

        let nat = MichelinePrimNoArgsNoAnnots::<98> {};
        let int = MichelinePrimNoArgsNoAnnots::<91> {};
        let expected = MichelinePrim2ArgsSomeAnnots::<_, _, 100> {
            arg1: nat,
            arg2: int,
            annots: ":foo".into(),
        };

        let (remaining_input, optnatfoo) = NomReader::nom_read(test.as_slice()).unwrap();

        assert!(remaining_input.is_empty());
        assert_eq!(expected, optnatfoo);
    }

    #[test]
    fn micheline_or_annot_encode() {
        let expected = vec![
            8,   // Prim_2 (some annots)
            100, // Prim tag: or
            3,   // Prim 0 (no annots)
            98,  // Prim tag: nat
            3,   // Prim 0 (no annots)
            91,  // Prim tag: int
            0, 0, 0, 4, // length of the annotation string
            b':', b'f', b'o', b'o', // annotation
        ];

        let nat = MichelinePrimNoArgsNoAnnots::<98> {};
        let int = MichelinePrimNoArgsNoAnnots::<91> {};
        let test = MichelinePrim2ArgsSomeAnnots::<_, _, 100> {
            arg1: nat,
            arg2: int,
            annots: ":foo".into(),
        };

        let mut bin = Vec::new();
        test.bin_write(&mut bin).unwrap();

        assert_eq!(expected, bin);
    }

    fn hex_to_bigint(s: &str) -> num_bigint::BigInt {
        use num_traits::FromPrimitive;
        num_bigint::BigInt::from_u64(u64::from_str_radix(s, 16).unwrap()).unwrap()
    }
}

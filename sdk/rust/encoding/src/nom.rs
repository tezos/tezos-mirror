// Copyright (c) SimpleStaking, Viable Systems, Nomadic Labs and Tezedge Contributors
// SPDX-CopyrightText: 2022-2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use bitvec::slice::BitSlice;
use bitvec::{bitvec, order::Msb0, view::BitView};
use nom::{
    branch::*, bytes::complete::*, combinator::*, error::ErrorKind, multi::*, number::complete::u8,
    sequence::*, Err, InputLength, Parser, Slice,
};
use num_bigint::{BigInt, BigUint, Sign};
pub use tezos_data_encoding_derive::NomReader;

use crate::types::{Narith, Zarith};

use self::error::{BoundedEncodingKind, DecodeError, DecodeErrorKind};

pub mod error {
    use std::{fmt::Write, str::Utf8Error};

    use nom::{
        error::{ErrorKind, FromExternalError},
        Offset,
    };

    use crate::bit_utils::BitsError;

    use super::NomInput;

    /// Decoding error
    #[derive(Debug, PartialEq)]
    pub struct DecodeError<I> {
        /// Input causing the error.
        pub(crate) input: I,
        /// Kind of the error.
        pub(crate) kind: DecodeErrorKind,
        /// Subsequent error, if any.
        pub(crate) other: Option<Box<DecodeError<I>>>,
    }

    /// Decoding error kind.
    #[derive(Debug, PartialEq, Eq)]
    pub enum DecodeErrorKind {
        /// Nom-specific error.
        Nom(ErrorKind),
        /// Error converting bytes to a UTF-8 string.
        Utf8(ErrorKind, Utf8Error),
        /// Boundary violation.
        Boundary(BoundedEncodingKind),
        /// Bits error
        Bits(BitsError),
        /// Field name
        Field(&'static str),
        /// Field name
        Variant(&'static str),
        /// Unknown/unsupported tag
        UnknownTag(String),
        /// Invalid tag
        InvalidTag(String),
    }

    /// Specific bounded encoding kind.
    #[derive(Debug, PartialEq, Eq, Clone)]
    pub enum BoundedEncodingKind {
        String,
        List,
        Dynamic,
        Bounded,
        Signature,
    }

    impl<'a> DecodeError<NomInput<'a>> {
        pub(crate) fn add_field(self, name: &'static str) -> Self {
            Self {
                input: <&[u8]>::clone(&self.input),
                kind: DecodeErrorKind::Field(name),
                other: Some(Box::new(self)),
            }
        }

        pub(crate) fn add_variant(self, name: &'static str) -> Self {
            Self {
                input: <&[u8]>::clone(&self.input),
                kind: DecodeErrorKind::Variant(name),
                other: Some(Box::new(self)),
            }
        }

        pub fn limit(input: NomInput<'a>, kind: BoundedEncodingKind) -> Self {
            Self {
                input,
                kind: DecodeErrorKind::Boundary(kind),
                other: None,
            }
        }

        pub fn unknown_tag(input: NomInput<'a>, tag: String) -> Self {
            Self {
                input,
                kind: DecodeErrorKind::UnknownTag(tag),
                other: None,
            }
        }

        pub fn invalid_tag(input: NomInput<'a>, tag: String) -> Self {
            Self {
                input,
                kind: DecodeErrorKind::InvalidTag(tag),
                other: None,
            }
        }

        pub fn get_unknown_tag(&self) -> Option<&String> {
            match self.kind {
                DecodeErrorKind::UnknownTag(ref tag) => Some(tag),
                _ => None,
            }
        }
    }

    impl<I> nom::error::ParseError<I> for DecodeError<I> {
        fn from_error_kind(input: I, kind: ErrorKind) -> Self {
            Self {
                input,
                kind: DecodeErrorKind::Nom(kind),
                other: None,
            }
        }

        fn append(input: I, kind: ErrorKind, other: Self) -> Self {
            Self {
                input,
                kind: DecodeErrorKind::Nom(kind),
                other: Some(Box::new(other)),
            }
        }
    }

    impl<I> nom::error::ParseError<(I, usize)> for DecodeError<I> {
        fn from_error_kind(input: (I, usize), kind: ErrorKind) -> Self {
            Self {
                input: input.0,
                kind: DecodeErrorKind::Nom(kind),
                other: None,
            }
        }

        fn append(input: (I, usize), kind: ErrorKind, other: Self) -> Self {
            Self {
                input: input.0,
                kind: DecodeErrorKind::Nom(kind),
                other: Some(Box::new(other)),
            }
        }
    }

    impl<I> FromExternalError<I, Utf8Error> for DecodeError<I> {
        fn from_external_error(input: I, kind: ErrorKind, e: Utf8Error) -> Self {
            Self {
                input,
                kind: DecodeErrorKind::Utf8(kind, e),
                other: None,
            }
        }
    }

    pub fn convert_error(input: NomInput, error: DecodeError<NomInput>) -> String {
        let mut res = String::new();
        let start = input.offset(error.input);
        let end = start + error.input.len();
        let _ = write!(res, "Error decoding bytes [{}..{}]", start, end);
        let _ = match error.kind {
            DecodeErrorKind::Nom(kind) => write!(res, " by nom parser `{:?}`", kind),
            DecodeErrorKind::Utf8(kind, e) => write!(res, " by nom parser `{:?}`: {}", kind, e),
            DecodeErrorKind::Boundary(kind) => {
                write!(
                    res,
                    " caused by boundary violation of encoding `{:?}`",
                    kind
                )
            }
            DecodeErrorKind::Field(name) => {
                write!(res, " while decoding field `{}`", name)
            }
            DecodeErrorKind::Variant(name) => {
                write!(res, " while decoding variant `{}`", name)
            }
            DecodeErrorKind::Bits(e) => write!(res, " while performing bits operation: {}", e),
            DecodeErrorKind::UnknownTag(tag) => write!(res, " caused by unsupported tag `{}`", tag),
            DecodeErrorKind::InvalidTag(tag) => write!(res, " caused by invalid tag `{}`", tag),
        };

        if let Some(other) = error.other {
            let _ = write!(res, "\n\nNext error:\n{}", convert_error(input, *other));
        }

        res
    }
}

/// Input for decoding.
pub type NomInput<'a> = &'a [u8];

/// Error type used to parameterize `nom`.
pub type NomError<'a> = error::DecodeError<NomInput<'a>>;

/// Nom result used in Tezedge (`&[u8]` as input, [NomError] as error type).
pub type NomResult<'a, T> = nom::IResult<NomInput<'a>, T, NomError<'a>>;

/// Traits defining message decoding using `nom` primitives.
pub trait NomReader<'a>: Sized {
    fn nom_read(input: &'a [u8]) -> NomResult<'a, Self>;
}

impl NomReader<'_> for Zarith {
    fn nom_read(bytes: &[u8]) -> NomResult<Self> {
        map(z_bignum, |big_int| big_int.into())(bytes)
    }
}

impl NomReader<'_> for Narith {
    fn nom_read(bytes: &[u8]) -> NomResult<Self> {
        map(n_bignum, |big_uint| big_uint.into())(bytes)
    }
}

pub fn unit(input: NomInput) -> NomResult<()> {
    Ok((input, ()))
}

/// Reads a boolean value.
#[inline(always)]
pub fn boolean(input: NomInput) -> NomResult<bool> {
    alt((
        map(tag(&[crate::types::BYTE_VAL_TRUE][..]), |_| true),
        map(tag(&[crate::types::BYTE_VAL_FALSE][..]), |_| false),
    ))(input)
}

/// Reads all available bytes into a [Vec]. Used in conjunction with [sized].
#[inline(always)]
pub fn bytes(input: NomInput) -> NomResult<Vec<u8>> {
    map(rest, Vec::from)(input)
}

/// Reads size encoded as 4-bytes big-endian unsigned.
#[inline(always)]
pub fn size(input: NomInput) -> NomResult<u32> {
    u32(input)
}

/// Reads size encoded as 1-byte unsigned.
#[inline(always)]
pub fn short_size(input: NomInput) -> NomResult<u8> {
    u8(input)
}

/// Reads size encoded as 4-bytes big-endian unsigned, checking that it does not exceed the `max` value.
#[inline(always)]
fn bounded_size(kind: BoundedEncodingKind, max: usize) -> impl FnMut(NomInput) -> NomResult<u32> {
    move |input| {
        let i = <&[u8]>::clone(&input);
        let (input, size) = size(input)?;
        if size as usize <= max {
            Ok((input, size))
        } else {
            Err(Err::Error(DecodeError::limit(i, kind.clone())))
        }
    }
}

/// Reads Tesoz string encoded as a 32-bit length followed by the string bytes.
#[inline(always)]
pub fn string(input: NomInput) -> NomResult<String> {
    map_res(complete(length_data(size)), |bytes| {
        std::str::from_utf8(bytes).map(str::to_string)
    })(input)
}

/// Returns parser that reads Tesoz string encoded as a 32-bit length followed by the string bytes,
/// checking that the lengh of the string does not exceed `max`.
#[inline(always)]
pub fn bounded_string<'a>(max: usize) -> impl FnMut(NomInput<'a>) -> NomResult<'a, String> {
    map_res(
        complete(length_data(bounded_size(BoundedEncodingKind::String, max))),
        |bytes| std::str::from_utf8(bytes).map(str::to_string),
    )
}

/// Parser that applies specified parser to the fixed length slice of input.
#[inline(always)]
pub fn sized<'a, O, F>(size: usize, f: F) -> impl FnMut(NomInput<'a>) -> NomResult<'a, O>
where
    F: FnMut(NomInput<'a>) -> NomResult<'a, O>,
{
    map_parser(take(size), f)
}

/// Parses optional field. Byte `0x00` indicates absence of the field,
/// byte `0xff` preceedes encoding of the existing field.
#[inline(always)]
pub fn optional_field<'a, O, F>(parser: F) -> impl FnMut(NomInput<'a>) -> NomResult<'a, Option<O>>
where
    F: FnMut(NomInput<'a>) -> NomResult<'a, O>,
    O: Clone,
{
    alt((
        preceded(tag(0x00u8.to_be_bytes()), success(None)),
        preceded(tag(0xffu8.to_be_bytes()), map(parser, Some)),
    ))
}

/// Parses input by applying parser `f` to it.
#[inline(always)]
pub fn list<'a, O, F>(f: F) -> impl FnMut(NomInput<'a>) -> NomResult<'a, Vec<O>>
where
    F: FnMut(NomInput<'a>) -> NomResult<'a, O>,
{
    many0(f)
}

/// Parses input by applying parser `f` to it no more than `max` times.
#[inline(always)]
pub fn bounded_list<'a, O, F>(
    max: usize,
    mut f: F,
) -> impl FnMut(NomInput<'a>) -> NomResult<'a, Vec<O>>
where
    F: FnMut(NomInput<'a>) -> NomResult<'a, O>,
    O: Clone,
{
    move |input| {
        let (input, list) = fold_many_m_n(
            0,
            max,
            |i| f.parse(i),
            Vec::new,
            |mut list, item| {
                list.push(item);
                list
            },
        )(input)?;
        if input.input_len() > 0 {
            Err(Err::Error(DecodeError {
                input,
                kind: DecodeErrorKind::Boundary(BoundedEncodingKind::List),
                other: None,
            }))
        } else {
            Ok((input, list))
        }
    }
}

/// Parses dynamic block by reading 4-bytes size and applying the parser `f` to the following sequence of bytes of that size.
#[inline(always)]
pub fn dynamic<'a, O, F>(f: F) -> impl FnMut(NomInput<'a>) -> NomResult<'a, O>
where
    F: FnMut(NomInput<'a>) -> NomResult<'a, O>,
{
    length_value(size, all_consuming(f))
}

/// Parses short dynamic block by reading 1-byte size and applying the parser `f` to the following sequence of bytes of that size.
#[inline(always)]
pub fn short_dynamic<'a, O, F>(f: F) -> impl FnMut(NomInput<'a>) -> NomResult<'a, O>
where
    F: FnMut(NomInput<'a>) -> NomResult<'a, O>,
    O: Clone,
{
    length_value(short_size, all_consuming(f))
}

/// Parses dynamic block by reading 4-bytes size and applying the parser `f`
/// to the following sequence of bytes of that size. It also checks that the size
/// does not exceed the `max` value.
#[inline(always)]
pub fn bounded_dynamic<'a, O, F>(max: usize, f: F) -> impl FnMut(NomInput<'a>) -> NomResult<'a, O>
where
    F: FnMut(NomInput<'a>) -> NomResult<'a, O>,
    O: Clone,
{
    length_value(
        bounded_size(BoundedEncodingKind::Dynamic, max),
        all_consuming(f),
    )
}

/// Applies the parser `f` to the input, limiting it to `max` bytes at most.
#[inline(always)]
pub fn bounded<'a, O, F>(max: usize, mut f: F) -> impl FnMut(NomInput<'a>) -> NomResult<'a, O>
where
    F: FnMut(NomInput<'a>) -> NomResult<'a, O>,
    O: Clone,
{
    move |input: NomInput| {
        let max = std::cmp::min(max, input.input_len());
        let bounded = input.slice(std::ops::RangeTo { end: max });
        match f.parse(bounded) {
            Ok((rest, parsed)) => Ok((
                input.slice(std::ops::RangeFrom {
                    start: max - rest.input_len(),
                }),
                parsed,
            )),
            Err(Err::Error(DecodeError {
                input,
                kind: error::DecodeErrorKind::Nom(ErrorKind::Eof),
                other,
            })) => Err(Err::Error(DecodeError {
                input,
                kind: error::DecodeErrorKind::Boundary(BoundedEncodingKind::Bounded),
                other,
            })),
            e => e,
        }
    }
}

/// Reserves `size` trailing bytes of the input and applies parser to the rest of the input.
#[inline(always)]
pub fn reserve<'a, O, F>(size: usize, mut parser: F) -> impl FnMut(NomInput<'a>) -> NomResult<'a, O>
where
    F: FnMut(NomInput<'a>) -> NomResult<'a, O>,
{
    move |input| {
        let input_len = input.len();
        let reserved_len = input_len - std::cmp::min(input_len, size);
        let reserved_input = &input[..reserved_len];
        let (reserved_input, out) = parser(reserved_input)?;
        Ok((&input[reserved_len - reserved_input.len()..], out))
    }
}

/// Applies the `parser` to the input, addin field context to the error.
#[inline(always)]
pub fn field<'a, O, F>(
    name: &'static str,
    mut parser: F,
) -> impl FnMut(NomInput<'a>) -> NomResult<'a, O>
where
    F: FnMut(NomInput<'a>) -> NomResult<'a, O>,
{
    move |input| parser(input).map_err(|e| e.map(|e| e.add_field(name)))
}

/// Applies the `parser` to the input, adding enum variant context to the error.
#[inline(always)]
pub fn variant<'a, O, F>(
    name: &'static str,
    mut parser: F,
) -> impl FnMut(NomInput<'a>) -> NomResult<'a, O>
where
    F: FnMut(NomInput<'a>) -> NomResult<'a, O>,
{
    move |input| parser(input).map_err(|e| e.map(|e| e.add_variant(name)))
}

pub fn z_bignum(mut input: NomInput) -> NomResult<BigInt> {
    let mut bitslice_vec: Vec<&BitSlice<u8, Msb0>> = Vec::new();
    let mut has_next = true;
    let mut missing_bits = 0;
    let mut first = true;
    let mut neg = false;
    while has_next {
        let (new_input, byte) = take(1_u8)(input)?;
        input = new_input;
        let bits = byte.view_bits();
        has_next = bits[0];
        let skip_bits = if first {
            neg = bits[1];
            2
        } else {
            1
        };
        first = false;
        bitslice_vec.push(&bits[skip_bits..]);
        missing_bits += skip_bits;
    }
    let mut bitvec = bitvec![u8, Msb0; 0; missing_bits % 8];
    for bitslice in bitslice_vec.into_iter().rev() {
        bitvec.extend_from_bitslice(bitslice);
    }
    let sign = if neg { Sign::Minus } else { Sign::Plus };
    Ok((input, BigInt::from_bytes_be(sign, &bitvec.into_vec())))
}

pub fn n_bignum(mut input: NomInput) -> NomResult<BigUint> {
    let mut bitslice_vec: Vec<&BitSlice<u8, Msb0>> = Vec::new();
    let mut has_next = true;
    let mut missing_bits = 0;
    while has_next {
        let (new_input, byte) = take(1_u8)(input)?;
        input = new_input;
        let bits = byte.view_bits();
        has_next = bits[0];
        bitslice_vec.push(&bits[1..]);
        missing_bits += 1;
    }
    let mut bitvec = bitvec![u8, Msb0; 0; missing_bits % 8];
    for bitslice in bitslice_vec.into_iter().rev() {
        bitvec.extend_from_bitslice(bitslice);
    }
    Ok((input, BigUint::from_bytes_be(&bitvec.into_vec())))
}

mod integers {
    macro_rules! decode_integer {
        ($t:ident) => {
            #[inline(always)]
            pub fn $t(input: super::NomInput) -> super::NomResult<$t> {
                use nom::number::complete::$t;
                $t(nom::number::Endianness::Big)(input)
            }
        };
    }
    decode_integer!(i16);
    decode_integer!(i32);
    decode_integer!(i64);
    decode_integer!(u16);
    decode_integer!(u32);
    decode_integer!(u64);
}
pub use integers::*;

#[cfg(test)]
mod test {
    use num_bigint::BigInt;
    use num_traits::FromPrimitive;

    use super::error::*;
    use super::*;

    #[test]
    fn test_boolean() {
        let res: NomResult<bool> = boolean(&[0xff]);
        assert_eq!(res, Ok((&[][..], true)));

        let res: NomResult<bool> = boolean(&[0x00]);
        assert_eq!(res, Ok((&[][..], false)));

        let res: NomResult<bool> = boolean(&[0x01]);
        res.expect_err("Error is expected");
    }

    #[test]
    fn test_size() {
        let input = &[0xff, 0xff, 0xff, 0xff];
        let res: NomResult<u32> = size(input);
        assert_eq!(res, Ok((&[][..], 0xffffffff)))
    }

    #[test]
    fn test_bounded_size() {
        let input = &[0x00, 0x00, 0x00, 0x10];

        let res: NomResult<u32> = bounded_size(BoundedEncodingKind::String, 100)(input);
        assert_eq!(res, Ok((&[][..], 0x10)));

        let res: NomResult<u32> = bounded_size(BoundedEncodingKind::String, 0x10)(input);
        assert_eq!(res, Ok((&[][..], 0x10)));

        let res: NomResult<u32> = bounded_size(BoundedEncodingKind::String, 0xf)(input);
        let err = res.expect_err("Error is expected");
        assert_eq!(err, limit_error(input, BoundedEncodingKind::String));
    }

    #[test]
    fn test_bytes() {
        let input = &[0, 1, 2, 3];
        let res: NomResult<Vec<u8>> = bytes(input);
        assert_eq!(res, Ok((&[][..], vec![0, 1, 2, 3])))
    }

    #[test]
    fn test_optional_field() {
        let res: NomResult<Option<u8>> = optional_field(u8)(&[0x00, 0x01][..]);
        assert_eq!(res, Ok((&[0x01][..], None)));

        let res: NomResult<Option<u8>> = optional_field(u8)(&[0xff, 0x01][..]);
        assert_eq!(res, Ok((&[][..], Some(0x01))));

        let res: NomResult<Option<u8>> = optional_field(u8)(&[0x01, 0x01][..]);
        res.expect_err("Error is expected");
    }

    #[test]
    fn test_string() {
        let input = &[0, 0, 0, 3, 0x78, 0x78, 0x78, 0xff];
        let res: NomResult<String> = string(input);
        assert_eq!(res, Ok((&[0xffu8][..], "xxx".to_string())))
    }

    #[test]
    fn test_bounded_string() {
        let input = &[0, 0, 0, 3, 0x78, 0x78, 0x78, 0xff];

        let res: NomResult<String> = bounded_string(3)(input);
        assert_eq!(res, Ok((&[0xffu8][..], "xxx".to_string())));

        let res: NomResult<String> = bounded_string(4)(input);
        assert_eq!(res, Ok((&[0xffu8][..], "xxx".to_string())));

        let res: NomResult<String> = bounded_string(2)(input);
        let err = res.expect_err("Error is expected");
        assert_eq!(err, limit_error(input, BoundedEncodingKind::String));
    }

    #[test]
    fn test_sized_bytes() {
        let input = &[0, 1, 2, 3, 4, 5, 6];
        let res: NomResult<Vec<u8>> = sized(4, bytes)(input);
        assert_eq!(res, Ok((&[4, 5, 6][..], vec![0, 1, 2, 3])))
    }

    #[test]
    fn test_list() {
        let input = &[0, 1, 2, 3, 4, 5];
        let res: NomResult<Vec<u16>> = list(u16)(input);
        assert_eq!(res, Ok((&[][..], vec![0x0001, 0x0203, 0x0405])));
    }

    #[test]
    fn test_bounded_list() {
        let input = &[0, 1, 2, 3, 4, 5];

        let res: NomResult<Vec<u16>> = bounded_list(4, u16)(input);
        assert_eq!(res, Ok((&[][..], vec![0x0001, 0x0203, 0x0405])));

        let res: NomResult<Vec<u16>> = bounded_list(3, u16)(input);
        assert_eq!(res, Ok((&[][..], vec![0x0001, 0x0203, 0x0405])));

        let res: NomResult<Vec<u16>> = bounded_list(2, u16)(input);
        let err = res.expect_err("Error is expected");
        assert_eq!(err, limit_error(&input[4..], BoundedEncodingKind::List));
    }

    #[test]
    fn test_dynamic() {
        let input = &[0, 0, 0, 3, 0x78, 0x78, 0x78, 0xff];

        let res: NomResult<Vec<u8>> = dynamic(bytes)(input);
        assert_eq!(res, Ok((&[0xffu8][..], vec![0x78; 3])));

        let res: NomResult<u8> = dynamic(u8)(input);
        res.expect_err("Error is expected");
    }

    #[test]
    fn test_short_dynamic() {
        let input = &[3, 0x78, 0x78, 0x78, 0xff];

        let res: NomResult<Vec<u8>> = short_dynamic(bytes)(input);
        assert_eq!(res, Ok((&[0xff_u8][..], vec![0x78_u8; 3])));

        let res: NomResult<u8> = short_dynamic(u8)(input);
        res.expect_err("Error is expected");
    }

    #[test]
    fn test_bounded_dynamic() {
        let input = &[0, 0, 0, 3, 0x78, 0x78, 0x78, 0xff];

        let res: NomResult<Vec<u8>> = bounded_dynamic(4, bytes)(input);
        assert_eq!(res, Ok((&[0xffu8][..], vec![0x78; 3])));

        let res: NomResult<Vec<u8>> = bounded_dynamic(3, bytes)(input);
        assert_eq!(res, Ok((&[0xffu8][..], vec![0x78; 3])));

        let res: NomResult<Vec<u8>> = bounded_dynamic(2, bytes)(input);
        let err = res.expect_err("Error is expected");
        assert_eq!(err, limit_error(input, BoundedEncodingKind::Dynamic));
    }

    #[test]
    fn test_bounded() {
        let input = &[1, 2, 3, 4, 5];

        let res: NomResult<Vec<u8>> = bounded(4, bytes)(input);
        assert_eq!(res, Ok((&[5][..], vec![1, 2, 3, 4])));

        let res: NomResult<Vec<u8>> = bounded(3, bytes)(input);
        assert_eq!(res, Ok((&[4, 5][..], vec![1, 2, 3])));

        let res: NomResult<Vec<u8>> = bounded(10, bytes)(input);
        assert_eq!(res, Ok((&[][..], vec![1, 2, 3, 4, 5])));

        let res: NomResult<u32> = bounded(3, u32)(input);
        let err = res.expect_err("Error is expected");
        assert_eq!(err, limit_error(&input[..3], BoundedEncodingKind::Bounded));
    }

    #[test]
    fn test_reserved() {
        let input = &[0, 0, 0, 1];
        let mut parser = tuple((reserve(1, list(u8)), u8));
        let (input, res) = parser(input).unwrap();
        assert_eq!(input.len(), 0);
        assert_eq!(res, (vec![0; 3], 1));
    }

    #[test]
    fn test_n_bignum() {
        let data = [
            ("0", "00"),
            ("1", "01"),
            ("7f", "7f"),
            ("80", "8001"),
            ("81", "8101"),
            ("ff", "ff01"),
            ("100", "8002"),
            ("101", "8102"),
            ("7fff", "ffff01"),
            ("8000", "808002"),
            ("8001", "818002"),
            ("ffff", "ffff03"),
            ("10000", "808004"),
            ("10001", "818004"),
        ];

        for (hex, enc) in data {
            let num = hex_to_biguint(hex);
            let input = hex::decode(enc).unwrap();
            let (input, dec) = n_bignum(&input).unwrap();
            assert!(input.is_empty());
            assert_eq!(dec, num);
        }
    }

    #[test]
    fn test_z_bignum() {
        let data = [
            ("0", "00"),
            ("1", "01"),
            ("7f", "bf01"),
            ("80", "8002"),
            ("81", "8102"),
            ("ff", "bf03"),
            ("100", "8004"),
            ("101", "8104"),
            ("7fff", "bfff03"),
            ("8000", "808004"),
            ("8001", "818004"),
            ("ffff", "bfff07"),
            ("10000", "808008"),
            ("10001", "818008"),
            ("9da879e", "9e9ed49d01"),
        ];

        for (hex, enc) in data {
            let num = hex_to_bigint(hex);
            let input = hex::decode(enc).unwrap();
            let (input, dec) = z_bignum(&input).unwrap();
            assert!(input.is_empty());
            assert_eq!(dec, num);
        }
    }

    #[test]
    fn test_neg_z_bignum() {
        let data = [
            ("-0", "00"),
            ("-1", "41"),
            ("-7f", "ff01"),
            ("-80", "c002"),
            ("-81", "c102"),
            ("-ff", "ff03"),
            ("-100", "c004"),
            ("-101", "c104"),
            ("-7fff", "ffff03"),
            ("-8000", "c08004"),
            ("-8001", "c18004"),
            ("-ffff", "ffff07"),
            ("-10000", "c08008"),
            ("-10001", "c18008"),
        ];

        for (hex, enc) in data {
            let num = hex_to_bigint(hex);
            let input = hex::decode(enc).unwrap();
            let (input, dec) = z_bignum(&input).unwrap();
            assert!(input.is_empty());
            assert_eq!(dec, num);
        }
    }

    fn hex_to_bigint(s: &str) -> BigInt {
        num_bigint::BigInt::from_i64(i64::from_str_radix(s, 16).unwrap()).unwrap()
    }

    fn hex_to_biguint(s: &str) -> BigUint {
        num_bigint::BigUint::from_u64(u64::from_str_radix(s, 16).unwrap()).unwrap()
    }

    fn limit_error(input: NomInput, kind: BoundedEncodingKind) -> Err<NomError> {
        Err::Error(DecodeError {
            input,
            kind: DecodeErrorKind::Boundary(kind),
            other: None,
        })
    }
}

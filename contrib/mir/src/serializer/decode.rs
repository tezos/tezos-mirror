/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/* Copyright (c) [2022-2023] TriliTech <contact@trili.tech>                   */
/*                                                                            */
/******************************************************************************/

//! Micheline deserialization.

use bitvec::{order::Lsb0, vec::BitVec, view::BitView};
use num_bigint::{BigInt, Sign};
use smallvec::{smallvec, SmallVec};
use strum::EnumCount;
use typed_arena::Arena;

use crate::{
    ast::{
        annotations::{Annotations, NO_ANNS},
        Micheline,
    },
    lexer::{try_ann_from_str, Annotation, Prim},
};

#[derive(PartialEq, Debug, Clone, Copy, thiserror::Error)]
pub enum DecodeError {
    #[error("trailing bytes after decoding the value")]
    TrailingBytes,
    #[error("PACK tag 0x05 not found")]
    NoPackTag,
    #[error("expected more data, but got EOF")]
    UnexpectedEOF,
    #[error("unknown tag: {0}")]
    UnknownTag(u8),
    #[error("forbidden character in string")]
    ForbiddenStringCharacter,
    #[error("unknown primitive tag: {0}")]
    UnknownPrim(u8),
    #[error("could not decode annotation")]
    BadAnnotation,
}

/// Prefix denoting an encoded number.
const NUMBER_TAG: u8 = 0x00;
/// Prefix denoting an encoded string.
const STRING_TAG: u8 = 0x01;
/// Prefix denoting an encoded sequence.
const SEQ_TAG: u8 = 0x02;
/// Prefix denoting an encoded bytes sequence.
const BYTES_TAG: u8 = 0x0a;

// Tags for [Michelson::App].
const APP_NO_ARGS_NO_ANNOTS_TAG: u8 = 0x03;
const APP_NO_ARGS_WITH_ANNOTS_TAG: u8 = 0x04;
const APP_ONE_ARG_NO_ANNOTS_TAG: u8 = 0x05;
const APP_ONE_ARG_WITH_ANNOTS_TAG: u8 = 0x06;
const APP_TWO_ARGS_NO_ANNOTS_TAG: u8 = 0x07;
const APP_TWO_ARGS_WITH_ANNOTS_TAG: u8 = 0x08;
const APP_GENERIC: u8 = 0x09;

/// If the number of arguments is small, an allocation-avoiding optimization is
/// used. This constant specifies the upper bound for the number of arguments
/// where it triggers.
/// At most we expect primitives with 3 arguments.
const EXPECTED_MAX_APP_ARGS: usize = 3;

/// If the number of arguments is small, an allocation-avoiding optimization is
/// used. This constant specifies the upper bound for the number of sequence
/// elements where it triggers.
/// 3 elements doesn't waste too much stack space and seems like a reasonable
/// optimization for small sequences.
const EXPECTED_MAX_SEQ_ELTS: usize = 3;

impl<'a> Micheline<'a> {
    /// Decode raw binary data. Same as `decode_packed`, but doesn't expect the
    /// first byte to be `0x05` tag.
    pub fn decode_raw(
        arena: &'a Arena<Micheline<'a>>,
        bytes: &[u8],
    ) -> Result<Micheline<'a>, DecodeError> {
        let mut it = bytes.into();
        let res = decode_micheline(arena, &mut it)?;
        if it.peek().is_some() {
            // didn't consume bytes entirely, fail
            return Err(DecodeError::TrailingBytes);
        }
        Ok(res)
    }

    /// Decode data that was previously `PACK`ed. Checks for `0x05` tag as the
    /// first byte and strips it.
    pub fn decode_packed(
        arena: &'a Arena<Micheline<'a>>,
        bytes: &[u8],
    ) -> Result<Micheline<'a>, DecodeError> {
        // PACK marker
        if bytes.first() != Some(&0x05) {
            return Err(DecodeError::NoPackTag);
        }
        Micheline::decode_raw(arena, &bytes[1..])
    }
}

struct BytesIt<'a>(&'a [u8]);

impl<'a> BytesIt<'a> {
    fn take(&mut self, num: usize) -> Option<&'a [u8]> {
        if self.0.len() < num {
            return None;
        }
        let (cur, rest) = self.0.split_at(num);
        self.0 = rest;
        Some(cur)
    }

    fn take_const<const N: usize>(&mut self) -> Option<&'a [u8; N]> {
        self.take(N).map(|x| x.try_into().unwrap())
    }

    fn next(&mut self) -> Option<u8> {
        self.next_ref().copied()
    }

    fn next_ref(&mut self) -> Option<&u8> {
        if self.0.is_empty() {
            return None;
        }
        let res = &self.0[0];
        self.0 = &self.0[1..];
        Some(res)
    }

    fn peek(&self) -> Option<u8> {
        self.0.first().copied()
    }
}

impl<'a> From<&'a [u8]> for BytesIt<'a> {
    fn from(value: &'a [u8]) -> Self {
        BytesIt(value)
    }
}

enum NumArgs {
    Zero,
    One,
    Two,
    Many,
}

fn decode_micheline<'a>(
    arena: &'a Arena<Micheline<'a>>,
    bytes: &mut BytesIt,
) -> Result<Micheline<'a>, DecodeError> {
    match bytes.next() {
        None => Err(DecodeError::UnexpectedEOF),
        Some(b) => match b {
            NUMBER_TAG => decode_int(bytes),
            STRING_TAG => decode_string(bytes),
            SEQ_TAG => decode_seq(arena, bytes),
            BYTES_TAG => decode_bytes(bytes),
            APP_NO_ARGS_NO_ANNOTS_TAG => decode_app(NumArgs::Zero, false, arena, bytes),
            APP_NO_ARGS_WITH_ANNOTS_TAG => decode_app(NumArgs::Zero, true, arena, bytes),
            APP_ONE_ARG_NO_ANNOTS_TAG => decode_app(NumArgs::One, false, arena, bytes),
            APP_ONE_ARG_WITH_ANNOTS_TAG => decode_app(NumArgs::One, true, arena, bytes),
            APP_TWO_ARGS_NO_ANNOTS_TAG => decode_app(NumArgs::Two, false, arena, bytes),
            APP_TWO_ARGS_WITH_ANNOTS_TAG => decode_app(NumArgs::Two, true, arena, bytes),
            APP_GENERIC => decode_app(NumArgs::Many, true, arena, bytes),
            b => Err(DecodeError::UnknownTag(b)),
        },
    }
}

fn get_len(bytes: &mut BytesIt) -> Result<u32, DecodeError> {
    Ok(u32::from_be_bytes(
        *bytes.take_const::<4>().ok_or(DecodeError::UnexpectedEOF)?,
    ))
}

fn decode_int(bytes: &mut BytesIt) -> Result<Micheline<'static>, DecodeError> {
    let mut bitvec: BitVec<u8, Lsb0> = BitVec::new();
    let mut sign = Sign::Plus;
    let mut first = true;
    loop {
        let bits = bytes
            .next_ref()
            .ok_or(DecodeError::UnexpectedEOF)?
            .view_bits::<Lsb0>();
        let data_len = if first {
            sign = if bits[6] { Sign::Minus } else { Sign::Plus };
            first = false;
            6
        } else {
            7
        };
        bitvec.extend_from_bitslice(&bits[..data_len]);
        if !bits[7] {
            break;
        }
    }
    bitvec.set_uninitialized(false);
    return Ok(Micheline::Int(BigInt::from_bytes_le(
        sign,
        &bitvec.into_vec(),
    )));
}

fn get_bytes<'a>(bytes: &mut BytesIt<'a>) -> Result<&'a [u8], DecodeError> {
    let len = get_len(bytes)? as usize;
    bytes.take(len).ok_or(DecodeError::UnexpectedEOF)
}

fn validate_str(bytes: &[u8]) -> Result<&str, DecodeError> {
    // check if all characters are printable ASCII
    if !bytes
        .iter()
        .all(|c| matches!(c, b' '..=b'~' | b'\n' | b'\r'))
    {
        return Err(DecodeError::ForbiddenStringCharacter);
    }
    // SAFETY: we just checked all characters are ASCII.
    Ok(unsafe { std::str::from_utf8_unchecked(bytes) })
}

fn decode_string(bytes: &mut BytesIt) -> Result<Micheline<'static>, DecodeError> {
    Ok(Micheline::String(
        validate_str(get_bytes(bytes)?)?.to_owned(),
    ))
}

fn decode_bytes(bytes: &mut BytesIt) -> Result<Micheline<'static>, DecodeError> {
    Ok(Micheline::Bytes(get_bytes(bytes)?.to_vec()))
}

fn decode_seq_raw<'a, const EXPECTED_MAX_ELTS: usize>(
    arena: &'a Arena<Micheline<'a>>,
    bytes: &mut BytesIt,
) -> Result<SmallVec<[Micheline<'a>; EXPECTED_MAX_ELTS]>, DecodeError> {
    let mut bytes: BytesIt = get_bytes(bytes)?.into();
    let mut buf = SmallVec::new();
    while bytes.peek().is_some() {
        buf.push(decode_micheline(arena, &mut bytes)?);
    }
    Ok(buf)
}

fn decode_seq<'a>(
    arena: &'a Arena<Micheline<'a>>,
    bytes: &mut BytesIt,
) -> Result<Micheline<'a>, DecodeError> {
    let buf = decode_seq_raw::<EXPECTED_MAX_SEQ_ELTS>(arena, bytes)?;
    let res = Micheline::Seq(arena.alloc_extend(buf));
    Ok(res)
}

fn validate_ann(bytes: &[u8]) -> Result<Annotation<'static>, DecodeError> {
    // @%|@%%|%@|[@:%][_0-9a-zA-Z][_0-9a-zA-Z\.%@]*
    macro_rules! alpha_num {
      () => {
        b'_' | b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z'
      }
    }
    match bytes {
        b"@%" | b"@%%" | b"%@" => {}
        [b'@' | b':' | b'%', alpha_num!(), rest @ ..]
            if rest
                .iter()
                .all(|c| matches!(c, alpha_num!() | b'.' | b'%' | b'@')) => {}
        _ => return Err(DecodeError::BadAnnotation),
    }
    // SAFETY: we just checked all bytes are ASCII
    let str = unsafe { std::str::from_utf8_unchecked(bytes) };
    // unwrap is fine, we effectively validated against a regex
    Ok(try_ann_from_str(str).unwrap().into_owned())
}

fn decode_app<'a>(
    num_args: NumArgs,
    annotations: bool,
    arena: &'a Arena<Micheline<'a>>,
    bytes: &mut BytesIt,
) -> Result<Micheline<'a>, DecodeError> {
    let prim = bytes.next().ok_or(DecodeError::UnexpectedEOF)?;
    if prim as usize >= Prim::COUNT {
        return Err(DecodeError::UnknownPrim(prim));
    }
    // SAFETY: Prim is repr(u8), and we checked it's within bounds.
    let prim: Prim = unsafe { std::mem::transmute(prim) };
    let args: SmallVec<[_; EXPECTED_MAX_APP_ARGS]> = match num_args {
        NumArgs::Zero => SmallVec::new(),
        NumArgs::One => smallvec![decode_micheline(arena, bytes)?],
        NumArgs::Two => smallvec![
            decode_micheline(arena, bytes)?,
            decode_micheline(arena, bytes)?,
        ],
        NumArgs::Many => decode_seq_raw(arena, bytes)?,
    };
    let anns = if annotations {
        let str = get_bytes(bytes)?;
        if str.is_empty() {
            NO_ANNS
        } else {
            str.split(|c| c == &b' ')
                .map(validate_ann)
                .collect::<Result<Annotations, DecodeError>>()?
        }
    } else {
        NO_ANNS
    };
    Ok(Micheline::App(prim, arena.alloc_extend(args), anns))
}

#[cfg(test)]
mod test {
    use super::*;

    #[track_caller]
    fn check<'a>(v: impl Into<Micheline<'a>>, hex_bytes: &str) {
        let arena = Arena::new();
        let hex_bytes: &str = hex_bytes
            .strip_prefix("0x")
            .expect("The `expected` argument must start from 0x");
        assert_eq!(
            Micheline::decode_raw(
                &arena,
                &hex::decode(hex_bytes).expect("Bad hex string in `expected` argument")
            ),
            Ok(v.into())
        );
    }

    fn check_err(hex_bytes: &str, err: DecodeError) {
        let arena = Arena::new();
        let hex_bytes: &str = hex_bytes
            .strip_prefix("0x")
            .expect("The `expected` argument must start from 0x");
        assert_eq!(
            Micheline::decode_raw(
                &arena,
                &hex::decode(hex_bytes).expect("Bad hex string in `expected` argument")
            ),
            Err(err)
        );
    }
    // To figure out the expected bytes, use
    // octez-client convert data 'VALUE' from michelson to binary

    mod value {
        use crate::ast::micheline::test_helpers::{app, seq};

        use super::*;

        #[test]
        fn primitive_values() {
            check((), "0x030b");
            check(true, "0x030a");
            check(false, "0x0303");
        }

        #[test]
        fn errors() {
            check_err("0x030b00", DecodeError::TrailingBytes);
            check_err("0x", DecodeError::UnexpectedEOF);
            check_err("0x03", DecodeError::UnexpectedEOF);
            check_err("0x02", DecodeError::UnexpectedEOF);
            check_err("0x09", DecodeError::UnexpectedEOF);
            check_err("0xff", DecodeError::UnknownTag(0xff));
            check_err("0x03ff", DecodeError::UnknownPrim(0xff));
            check_err("0x010000000100", DecodeError::ForbiddenStringCharacter);
        }

        mod number {
            use super::*;

            #[test]
            fn zero() {
                check(0, "0x0000");
            }

            #[test]
            fn few_trivial_samples() {
                check(1, "0x0001");
                check(13, "0x000d");
            }

            #[test]
            fn largest_1_byte_long() {
                check(63, "0x003f");
            }

            #[test]
            fn smallest_2_bytes_long() {
                check(64, "0x008001");
            }

            #[test]
            fn large() {
                check(123456789, "0x0095b4de75");
            }

            #[test]
            fn negative() {
                check(-1, "0x0041");
                check(-36, "0x0064");
            }

            // Don't mind this "largest", it is in absolute numeric value sense
            #[test]
            fn negative_largest_1_byte_long() {
                check(-63, "0x007f");
            }

            #[test]
            fn negative_smallest_2_bytes_long() {
                check(-64, "0x00c001");
            }

            #[test]
            fn negative_large() {
                check(-987654321, "0x00f1a2f3ad07");
            }
        }

        #[test]
        fn simple_nested() {
            check(app!(Pair[true, ""]), "0x0707030a0100000000");
            check(app!(None[]), "0x0306");
            check(app!(Some[app!(Unit)]), "0x0509030b");
            check(app!(Elt[true, ()]), "0x0704030a030b");
            check(
                seq! { app!(DROP); app!(LAMBDA[app!(unit), app!(unit), seq!{}]) },
                "0x02000000150320093100000009036c036c020000000000000000",
            );
        }

        #[test]
        fn string() {
            check("", "0x0100000000");
            check("abc", "0x0100000003616263");
            check(
                "123456789123456789123456789",
                "0x010000001b313233343536373839313233343536373839313233343536373839",
            );
        }

        #[test]
        fn very_long_string() {
            // Using "\"$(printf 'x%.0s' {1..1000})\"" as a value
            // Verifies that length is encoded as a fixed-length number, not as zarith
            check(
                "x".repeat(1000),
               "0x01000003e878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878787878"
            );
        }

        #[test]
        fn bytes() {
            check(hex::decode("").unwrap(), "0x0a00000000");
            check(hex::decode("001234abff").unwrap(), "0x0a00000005001234abff");
        }

        #[test]
        fn list() {
            check(seq! {}, "0x0200000000");
            check(seq! {true; false}, "0x0200000004030a0303");
        }

        #[test]
        fn deeply_nested_list() {
            check(
                seq! {seq!{}; seq!{true}},
                "0x020000000c02000000000200000002030a",
            );
        }

        #[test]
        fn list_with_applications() {
            check(
                seq! {app!(Pair[3, 4]); app!(Pair[5, 6])},
                "0x020000000c070700030004070700050006",
            )
        }

        #[test]
        fn very_long_list() {
            // Using "{ $(printf 'Unit;%.0s' {1..1000}) }" as a value
            // Verifies that length is encoded as a fixed-length number, not as zarith
            check(
                Micheline::Seq(&vec![app!(Unit); 1000]),
                "0x02000007d0030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b",
          );
        }
    }

    mod annotations {
        use crate::parser::test_helpers::*;

        use super::*;

        #[test]
        fn trivial() {
            check(parse("(int %a)").unwrap(), "0x045b000000022561");
            check(parse("(int :a)").unwrap(), "0x045b000000023a61");
            check(
                parse("(int @abc123)").unwrap(),
                "0x045b0000000740616263313233",
            );
        }

        #[test]
        fn several_annotations() {
            check(
                parse("(int %a :b @c %d)").unwrap(),
                "0x045b0000000b2561203a62204063202564",
            );
        }

        #[test]
        fn nested_entries() {
            check(
                parse("(pair %a (int %b))").unwrap(),
                "0x0665045b000000022562000000022561",
            );
        }

        #[test]
        fn generic_case() {
            check(
                parse("LAMBDA (int %a) (int :b) {}").unwrap(),
                "0x093100000015045b000000022561045b000000023a62020000000000000000",
            );
            check(
                parse("LAMBDA (int %a %b %c %d) int {}").unwrap(),
                "0x093100000018045b0000000b2561202562202563202564035b020000000000000000",
            );
        }

        #[test]
        fn bad_annotations() {
            check_err("0x045b00000002257f", DecodeError::BadAnnotation);
            check_err("0x045b000000026161", DecodeError::BadAnnotation);
        }
    }
}

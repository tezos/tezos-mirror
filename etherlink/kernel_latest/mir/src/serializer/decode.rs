// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
// SPDX-FileCopyrightText: [2022-2023] TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Micheline deserialization.

use super::constants::*;
use bitvec::{order::Lsb0, vec::BitVec, view::BitView};
use num_bigint::{BigInt, Sign};
use smallvec::SmallVec;
use strum::EnumCount;
use typed_arena::Arena;

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

use crate::{
    ast::{
        annotations::{Annotations, NO_ANNS},
        Annotation, Micheline,
    },
    gas::{interpret_cost, Gas, OutOfGas},
    lexer::{ann_from_str, Prim},
};

/// Errors that can happen during deserialization.
#[derive(PartialEq, Eq, Debug, Clone, Copy, thiserror::Error)]
pub enum DecodeError {
    /// Trailing bytes present after decoding a value.
    #[error("trailing bytes after decoding the value")]
    TrailingBytes,
    /// Expected PACK format, but no leading 0x05 byte found.
    #[error("PACK tag 0x05 not found")]
    NoPackTag,
    /// Expected more data, but found EOF.
    #[error("expected more data, but got EOF")]
    UnexpectedEOF,
    /// Unknown data tag.
    #[error("unknown tag: {0}")]
    UnknownTag(u8),
    /// Forbidden character found during string deserialization.
    #[error("forbidden character in string")]
    ForbiddenStringCharacter,
    /// Expected a primitive, but could not interpret the byte as a primitive.
    #[error("unknown primitive tag: {0}")]
    UnknownPrim(u8),
    /// Failed to deserialize an annotation.
    #[error("could not decode annotation")]
    BadAnnotation,
    /// An annotation exceeds L1's 255-byte limit.
    #[error("annotation exceeds 255 bytes (was {0})")]
    OversizedAnnotation(usize),
    /// Non-canonical zarith encoding: a multi-byte number whose final byte is
    /// `0x00` carries an all-zero high-order group and could be shortened. L1
    /// rejects this as `Trailing_zero`, so UNPACK must return `None`.
    #[error("non-canonical zarith encoding: trailing zero byte")]
    ZarithTrailingZero,
}

impl<'a> Micheline<'a> {
    /// Decode raw binary data. Same as `decode_packed`, but doesn't expect the
    /// first byte to be `0x05` tag. Charges
    /// `interpret_cost::micheline_decoding_bytes` (mirrors L1's
    /// `cost_DECODING_MICHELINE_bytes`) against `gas` before parsing.
    /// The outer `Result` reports gas exhaustion; the inner `Result`
    /// reports deserialization failures.
    pub fn decode_raw(
        arena: &'a Arena<Micheline<'a>>,
        bytes: &[u8],
        gas: &mut Gas,
    ) -> Result<Result<Micheline<'a>, DecodeError>, OutOfGas> {
        gas.consume(
            interpret_cost::micheline_decoding_bytes(bytes.len())
                .map_err(|_| OutOfGas)?,
        )?;
        Ok(Self::decode_raw_unmetered(bytes, arena))
    }

    /// Inner decoder shared by [`Self::decode_raw`] and [`Self::decode_packed`].
    /// Performs no gas accounting; the caller is responsible for charging
    /// the bytes-based cost before calling.
    fn decode_raw_unmetered(
        bytes: &[u8],
        arena: &'a Arena<Micheline<'a>>,
    ) -> Result<Micheline<'a>, DecodeError> {
        let mut it: BytesIt = bytes.into();
        let res = decode_micheline(arena, &mut it)?;
        if it.peek().is_some() {
            // didn't consume bytes entirely, fail
            return Err(DecodeError::TrailingBytes);
        }
        Ok(res)
    }

    /// Decode exactly one Micheline expression from the start of `bytes`,
    /// returning it together with the number of bytes consumed.
    ///
    /// Unlike [`Self::decode_raw`], this does NOT error on trailing bytes.
    /// Charges `interpret_cost::micheline_decoding_bytes` on the **full**
    /// input length (a crafted long tail is paid for, even when only a
    /// short prefix is consumed).
    pub fn decode_raw_prefix(
        arena: &'a Arena<Micheline<'a>>,
        bytes: &[u8],
        gas: &mut Gas,
    ) -> Result<Result<(Micheline<'a>, usize), DecodeError>, OutOfGas> {
        gas.consume(
            interpret_cost::micheline_decoding_bytes(bytes.len())
                .map_err(|_| OutOfGas)?,
        )?;
        let mut it: BytesIt = bytes.into();
        Ok(decode_micheline(arena, &mut it).map(|res| (res, bytes.len() - it.0.len())))
    }

    /// Decode data that was previously `PACK`ed. Checks for `0x05` tag as the
    /// first byte and strips it. Charges
    /// `interpret_cost::micheline_decoding_bytes` on the **full** input
    /// length (including the `0x05` PACK marker), matching L1's
    /// `cost_DECODING_MICHELINE_bytes` applied to the UNPACK input.
    pub fn decode_packed(
        arena: &'a Arena<Micheline<'a>>,
        bytes: &[u8],
        gas: &mut Gas,
    ) -> Result<Result<Micheline<'a>, DecodeError>, OutOfGas> {
        gas.consume(
            interpret_cost::micheline_decoding_bytes(bytes.len())
                .map_err(|_| OutOfGas)?,
        )?;
        if bytes.first() != Some(&0x05) {
            return Ok(Err(DecodeError::NoPackTag));
        }
        Ok(Self::decode_raw_unmetered(&bytes[1..], arena))
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

/// Worklist frame for the iterative decoder.
///
/// `AppArgs` reads N children from the parent iterator. `Container` opens
/// a sub iterator scoped to one Seq or one many arg App and finalizes
/// when that sub iterator is exhausted.
enum DecFrame<'a> {
    AppArgs {
        prim: Prim,
        remaining: usize,
        annots: bool,
        acc: SmallVec<[Micheline<'a>; EXPECTED_MAX_APP_ARGS]>,
    },
    Container {
        kind: ContainerKind,
        acc: SmallVec<[Micheline<'a>; EXPECTED_MAX_SEQ_ELTS]>,
    },
}

enum ContainerKind {
    Seq,
    AppMany { prim: Prim, annots: bool },
}

/// Read the primitive byte and validate it.
fn read_prim(bytes: &mut BytesIt) -> Result<Prim, DecodeError> {
    let p = bytes.next().ok_or(DecodeError::UnexpectedEOF)?;
    if p as usize >= Prim::COUNT {
        return Err(DecodeError::UnknownPrim(p));
    }
    // SAFETY: Prim is repr(u8) and the value is within bounds.
    Ok(unsafe { std::mem::transmute::<u8, Prim>(p) })
}

/// Parse a length prefixed annotation block. Always returns owned
/// (static) annotations so callers can use them at any lifetime.
fn read_annots(bytes: &mut BytesIt) -> Result<Annotations<'static>, DecodeError> {
    let str = get_bytes(bytes)?;
    if str.is_empty() {
        Ok(NO_ANNS)
    } else {
        str.split(|c| c == &b' ')
            .map(validate_ann)
            .collect::<Result<Annotations<'static>, DecodeError>>()
    }
}

/// Drive the decoder from an explicit worklist instead of Rust recursion,
/// so deeply nested input does not depend on call stack depth.
fn decode_micheline<'a, 'b>(
    arena: &'a Arena<Micheline<'a>>,
    bytes: &mut BytesIt<'b>,
) -> Result<Micheline<'a>, DecodeError> {
    let initial = std::mem::replace(bytes, BytesIt(&[]));
    let mut iters: Vec<BytesIt<'b>> = vec![initial];
    let mut frames: Vec<DecFrame<'a>> = Vec::new();

    // Single helper to deliver a finished node to the top frame, or return
    // it as the root when the stack is empty.
    fn deliver<'a>(
        frames: &mut Vec<DecFrame<'a>>,
        m: Micheline<'a>,
    ) -> Option<Micheline<'a>> {
        match frames.last_mut() {
            None => Some(m),
            Some(DecFrame::AppArgs { remaining, acc, .. }) => {
                acc.push(m);
                *remaining -= 1;
                None
            }
            Some(DecFrame::Container { acc, .. }) => {
                acc.push(m);
                None
            }
        }
    }

    loop {
        // Phase A: finalize any frames whose work is done. Each finalization
        // produces a new node which is delivered to the parent frame, which
        // may itself become done, and so on.
        loop {
            let done = match frames.last() {
                Some(DecFrame::AppArgs { remaining: 0, .. }) => true,
                Some(DecFrame::Container { .. }) => {
                    iters.last().unwrap().peek().is_none()
                }
                _ => false,
            };
            if !done {
                break;
            }
            let m = match frames.pop().unwrap() {
                DecFrame::AppArgs {
                    prim, annots, acc, ..
                } => {
                    let active = iters.last_mut().unwrap();
                    let anns = if annots {
                        read_annots(active)?
                    } else {
                        NO_ANNS
                    };
                    // Allocating from a SmallVec is safe; the iterable does
                    // not touch the arena. See the note on alloc_extend.
                    #[allow(clippy::disallowed_methods)]
                    Micheline::App(prim, arena.alloc_extend(acc), anns)
                }
                DecFrame::Container { kind, acc } => {
                    // The scoped iter is exhausted; pop it so the parent
                    // iter is active again. Any annot suffix for AppMany
                    // is read from the parent iter.
                    iters.pop();
                    match kind {
                        ContainerKind::Seq =>
                        {
                            #[allow(clippy::disallowed_methods)]
                            Micheline::Seq(arena.alloc_extend(acc))
                        }
                        ContainerKind::AppMany { prim, annots } => {
                            let active = iters.last_mut().unwrap();
                            let anns = if annots {
                                read_annots(active)?
                            } else {
                                NO_ANNS
                            };
                            #[allow(clippy::disallowed_methods)]
                            Micheline::App(prim, arena.alloc_extend(acc), anns)
                        }
                    }
                }
            };
            if let Some(root) = deliver(&mut frames, m) {
                std::mem::swap(bytes, &mut iters[0]);
                return Ok(root);
            }
        }

        // Phase B: decode the next node from the active iter. The arms
        // that produce a finished `Micheline` directly (leaves and zero
        // argument `App`s) return `Some(m)`; the arms that open a sub
        // iterator or push a multi argument frame return `None`. The
        // single `deliver / swap / return` block after the match
        // handles either case uniformly.
        let produced: Option<Micheline<'a>> = {
            let active = iters.last_mut().unwrap();
            let b = active.next().ok_or(DecodeError::UnexpectedEOF)?;
            match b {
                NUMBER_TAG => Some(decode_int(active)?),
                STRING_TAG => Some(decode_string(active)?),
                BYTES_TAG => Some(decode_bytes(active)?),
                SEQ_TAG => {
                    let scoped: BytesIt<'b> = get_bytes(active)?.into();
                    iters.push(scoped);
                    frames.push(DecFrame::Container {
                        kind: ContainerKind::Seq,
                        acc: SmallVec::new(),
                    });
                    None
                }
                APP_NO_ARGS_NO_ANNOTS_TAG | APP_NO_ARGS_WITH_ANNOTS_TAG => {
                    let annots = b == APP_NO_ARGS_WITH_ANNOTS_TAG;
                    let prim = read_prim(active)?;
                    let anns = if annots {
                        read_annots(active)?
                    } else {
                        NO_ANNS
                    };
                    #[allow(clippy::disallowed_methods)]
                    Some(Micheline::App(prim, &[], anns))
                }
                APP_ONE_ARG_NO_ANNOTS_TAG | APP_ONE_ARG_WITH_ANNOTS_TAG => {
                    let annots = b == APP_ONE_ARG_WITH_ANNOTS_TAG;
                    let prim = read_prim(active)?;
                    frames.push(DecFrame::AppArgs {
                        prim,
                        remaining: 1,
                        annots,
                        acc: SmallVec::new(),
                    });
                    None
                }
                APP_TWO_ARGS_NO_ANNOTS_TAG | APP_TWO_ARGS_WITH_ANNOTS_TAG => {
                    let annots = b == APP_TWO_ARGS_WITH_ANNOTS_TAG;
                    let prim = read_prim(active)?;
                    frames.push(DecFrame::AppArgs {
                        prim,
                        remaining: 2,
                        annots,
                        acc: SmallVec::new(),
                    });
                    None
                }
                APP_GENERIC => {
                    let prim = read_prim(active)?;
                    let scoped: BytesIt<'b> = get_bytes(active)?.into();
                    iters.push(scoped);
                    frames.push(DecFrame::Container {
                        kind: ContainerKind::AppMany { prim, annots: true },
                        acc: SmallVec::new(),
                    });
                    None
                }
                other => return Err(DecodeError::UnknownTag(other)),
            }
        };
        if let Some(m) = produced {
            if let Some(root) = deliver(&mut frames, m) {
                std::mem::swap(bytes, &mut iters[0]);
                return Ok(root);
            }
        }
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
        let byte = *bytes.next_ref().ok_or(DecodeError::UnexpectedEOF)?;
        let bits = byte.view_bits::<Lsb0>();
        let is_first = first;
        let data_len = if first {
            sign = if bits[6] { Sign::Minus } else { Sign::Plus };
            first = false;
            6
        } else {
            7
        };
        bitvec.extend_from_bitslice(&bits[..data_len]);
        // bit 7 is the continuation flag; clear means this is the final byte.
        if !bits[7] {
            // A continuation byte (i.e. not the first one) that terminates the
            // number with value `0x00` contributes an all-zero high-order
            // group, so the number is encodable in fewer bytes. L1's zarith
            // reader rejects this as `Trailing_zero`; mirror that so UNPACK
            // returns `None` instead of accepting the non-canonical input.
            // The single-byte `0x00` (the canonical encoding of 0) and the
            // first-byte `0x40` (negative zero, handled elsewhere) are
            // unaffected because they keep `is_first` true.
            if !is_first && byte == 0 {
                return Err(DecodeError::ZarithTrailingZero);
            }
            break;
        }
    }
    bitvec.set_uninitialized(false);
    Ok(Micheline::Int(BigInt::from_bytes_le(
        sign,
        &bitvec.into_vec(),
    )))
}

fn get_bytes<'a>(bytes: &mut BytesIt<'a>) -> Result<&'a [u8], DecodeError> {
    let len = get_len(bytes)? as usize;
    bytes.take(len).ok_or(DecodeError::UnexpectedEOF)
}

fn validate_str(bytes: &[u8]) -> Result<&str, DecodeError> {
    // L1's `Script_string.of_string` accepts only newline `\n` (0x0a) and
    // printable ASCII `0x20..=0x7e`. UNPACK string decodes attacker-supplied
    // bytes at runtime, so mismatching this set lets MIR push `Some`
    // where L1 pushes `None` — notably for carriage return (0x0d), which
    // used to be allowed here but is forbidden on L1.
    if !bytes.iter().all(|c| matches!(c, b' '..=b'~' | b'\n')) {
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

fn validate_ann(bytes: &[u8]) -> Result<Annotation<'static>, DecodeError> {
    // L1's binary Micheline decoder rejects annotations longer than 255 bytes
    // (see src/lib_micheline/micheline_encoding.ml). MIR must do the same so a
    // blob L1 rejects does not decode here (e.g. UNPACK must return None).
    if bytes.len() > 255 {
        return Err(DecodeError::OversizedAnnotation(bytes.len()));
    }

    // @%|@%%|%@|@|:|%|[@:%][_0-9a-zA-Z][_0-9a-zA-Z\.%@]*
    macro_rules! alpha_num {
      () => {
        b'_' | b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z'
      }
    }
    match bytes {
        b"@%" | b"@%%" | b"%@" | b"@" | b":" | b"%" => {}
        [b'@' | b':' | b'%', alpha_num!(), rest @ ..]
            if rest
                .iter()
                .all(|c| matches!(c, alpha_num!() | b'.' | b'%' | b'@')) => {}
        _ => return Err(DecodeError::BadAnnotation),
    }
    // SAFETY: we just checked all bytes are ASCII
    let str = unsafe { std::str::from_utf8_unchecked(bytes) };
    ann_from_str(str)
        .map(Annotation::into_owned)
        .map_err(|_| DecodeError::BadAnnotation)
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
                &hex::decode(hex_bytes).expect("Bad hex string in `expected` argument"),
                &mut Gas::default(),
            ),
            Ok(Ok(v.into()))
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
                &hex::decode(hex_bytes).expect("Bad hex string in `expected` argument"),
                &mut Gas::default(),
            ),
            Ok(Err(err))
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
            // 0x00 (NUL) is forbidden in a Michelson string.
            check_err("0x010000000100", DecodeError::ForbiddenStringCharacter);
            // 0x0d (carriage return) is forbidden: L1 accepts only `\n` and
            // printable ASCII `0x20..=0x7e`. Encoding `"a\rb"` as a string
            // payload (string tag 01, length 00000003, content 61 0d 62)
            // must error, not yield `Some "a\rb"`.
            check_err("0x0100000003610d62", DecodeError::ForbiddenStringCharacter);
            // 0x09 (TAB) is likewise forbidden.
            check_err("0x010000000109", DecodeError::ForbiddenStringCharacter);
            // Sanity: 0x0a (newline) and 0x7e (`~`) are still allowed.
            check("\n", "0x01000000010a");
            check("~", "0x01000000017e");
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

            // Canonical encodings of 12345 and 1; control cases that must keep
            // decoding successfully after the trailing-zero check below.
            #[test]
            fn canonical_multibyte() {
                check(12345, "0x00b9c001");
                check(1, "0x0001");
            }

            // Non-canonical zarith: a multi-byte number terminated by a `0x00`
            // byte. L1 rejects these as `Trailing_zero`, so we must too,
            // otherwise UNPACK would return `Some` where L1 returns `None`.
            #[test]
            fn trailing_zero_rejected() {
                // 12345 with a spurious trailing 0x00 group.
                check_err("0x00b9c08100", DecodeError::ZarithTrailingZero);
                // 1 encoded in two bytes instead of one (non-minimal).
                check_err("0x008100", DecodeError::ZarithTrailingZero);
                // 0 encoded in two bytes instead of one.
                check_err("0x008000", DecodeError::ZarithTrailingZero);
                // Negative number with a trailing zero group is rejected too.
                check_err("0x00c1c08100", DecodeError::ZarithTrailingZero);
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

    /// Depth that would overflow the Rust call stack under the old
    /// recursive decoder; the worklist driver must complete it.
    mod deep_recursion {
        use super::*;

        #[test]
        fn deeply_nested_seq_roundtrips() {
            // Build SEQ_TAG plus a 4-byte length, then DEPTH copies of the
            // SEQ_TAG length-prefixed payload, innermost is empty.
            const DEPTH: usize = 100_000;
            let mut bytes: Vec<u8> = Vec::with_capacity(DEPTH * 5 + 5);
            for _ in 0..DEPTH {
                bytes.push(SEQ_TAG);
                let payload_len = (DEPTH * 5 - bytes.len() + 1) as u32;
                bytes.extend_from_slice(&payload_len.to_be_bytes());
            }
            // innermost empty Seq: tag + zero length
            bytes.push(SEQ_TAG);
            bytes.extend_from_slice(&0u32.to_be_bytes());

            let arena: Arena<Micheline<'_>> = Arena::new();
            let result =
                Micheline::decode_raw(&arena, &bytes, &mut crate::gas::Gas::default())
                    .expect("gas suffices")
                    .expect("deep seq decodes");
            // peel off DEPTH layers
            let mut node = &result;
            for _ in 0..DEPTH {
                match node {
                    Micheline::Seq([only]) => node = only,
                    _ => panic!("expected deeply nested Seq, got mismatch at this level"),
                }
            }
            assert!(matches!(node, Micheline::Seq([])));
        }

        /// L2-1413 / TZX-42 regression: a deeply nested `Some` chain (App
        /// nodes with a single argument) overflowed the old recursive decoder
        /// and crashed the kernel (sequencer DoS via `callMichelson`); the
        /// worklist driver must decode it iteratively. Binary per `Some` layer
        /// is `0x05 0x09` = App(Some, [1 arg]); innermost `0x03 0x0b` =
        /// App(Unit, []), so the full payload is `0509`*DEPTH ++ `030b`.
        #[test]
        fn deeply_nested_some_decodes_without_overflow() {
            const DEPTH: usize = 100_000;
            let mut bytes: Vec<u8> = Vec::with_capacity(DEPTH * 2 + 2);
            for _ in 0..DEPTH {
                bytes.push(0x05);
                bytes.push(0x09);
            }
            bytes.push(0x03);
            bytes.push(0x0b);

            let arena: Arena<Micheline<'_>> = Arena::new();
            let result =
                Micheline::decode_raw(&arena, &bytes, &mut crate::gas::Gas::default())
                    .expect("gas suffices")
                    .expect("deep Some decodes");
            // peel off DEPTH `Some` layers
            let mut node = &result;
            for _ in 0..DEPTH {
                match node {
                    Micheline::App(_, [only], _) => node = only,
                    _ => panic!("expected deeply nested Some App at this level"),
                }
            }
            assert!(matches!(node, Micheline::App(_, [], _))); // innermost Unit
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

        #[test]
        fn oversized_annotation() {
            // L1 caps annotation tokens (sigil included) at 255 bytes; MIR must
            // match on the binary decode path so UNPACK of an oversized blob
            // yields None. Build the packed bytes for `(true @aa...)` whose
            // annotation token is `len` bytes long.
            fn true_with_ann(len: usize) -> Vec<u8> {
                let mut bool = Micheline::from(true);
                bool.annotate(
                    Annotation::Variable("a".repeat(len - 1).into()),
                    &mut Gas::default(),
                )
                .unwrap();
                bool.encode(&mut Gas::default()).unwrap().unwrap()
            }

            // 255-byte token: accepted, matching L1.
            {
                let arena = Arena::new();
                assert!(matches!(
                    Micheline::decode_raw(
                        &arena,
                        &true_with_ann(255),
                        &mut Gas::default()
                    ),
                    Ok(Ok(_))
                ));
            }
            // 256-byte token: rejected, matching L1 (UNPACK -> None).
            {
                let arena = Arena::new();
                assert_eq!(
                    Micheline::decode_raw(
                        &arena,
                        &true_with_ann(256),
                        &mut Gas::default()
                    ),
                    Ok(Err(DecodeError::OversizedAnnotation(256)))
                );
            }
        }
    }
}

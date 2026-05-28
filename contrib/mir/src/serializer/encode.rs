// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! Micheline serialization.

use std::mem::size_of;
use std::ops::Range;
use tezos_data_encoding::{
    enc::{BinError, BinResult, BinWriter},
    types::Zarith,
};

use super::constants::*;
use crate::{
    ast::{Annotation, Annotations, Micheline},
    gas::{interpret_cost, Gas, OutOfGas},
};

impl Annotation<'_> {
    /// Serialize annotation to the output byte [Vec], using the `PACK` format.
    /// Essentially this means write the annotation with the corresponding tag
    /// character verbatim to the output, so, for example,
    ///
    /// ```
    /// use mir::ast::Annotation;
    /// let mut out = vec![];
    /// Annotation::Field("field".into()).encode_bytes(&mut out);
    /// assert_eq!(&out, b"%field");
    /// ```
    ///
    /// Note that [Annotation::Special] are written to the output verbatim:
    ///
    /// ```
    /// use mir::ast::Annotation;
    /// let mut out = vec![];
    /// Annotation::Special("@%".into()).encode_bytes(&mut out);
    /// assert_eq!(&out, b"@%");
    /// ```
    pub fn encode_bytes(&self, out: &mut Vec<u8>) {
        match self {
            Annotation::Special(s) => out.extend_from_slice(s.as_bytes()),
            Annotation::Field(s) => {
                out.push(b'%');
                out.extend_from_slice(s.as_bytes());
            }
            Annotation::Variable(s) => {
                out.push(b'@');
                out.extend_from_slice(s.as_bytes());
            }
            Annotation::Type(s) => {
                out.push(b':');
                out.extend_from_slice(s.as_bytes());
            }
        }
    }
}

impl Annotations<'_> {
    /// Serialize a collection of annotations to the output byte [Vec], using
    /// the `PACK` format. Essentially this means write 4 bytes of length,
    /// followed by annotations with the corresponding tag character verbatim to
    /// the output, separated by a space character `0x20`. So, for example,
    ///
    /// ```
    /// use mir::ast::{Annotations, Annotation};
    /// let mut out = vec![];
    /// Annotations::from([
    ///     Annotation::Field("field".into()),
    ///     Annotation::Variable("var".into()),
    /// ])
    /// .encode_bytes(&mut out);
    /// assert_eq!(&out, b"\x00\x00\x00\x0B%field @var");
    /// ```
    pub fn encode_bytes(&self, out: &mut Vec<u8>) {
        // Annotation encoding is infallible, so we adapt to
        // `with_patchback_len_result` by wrapping in `Ok(())`. Using
        // `.expect` here is safe because the closure cannot return an error.
        with_patchback_len_result(out, |out| {
            // Add them space-separated
            let mut is_first = true;
            for ann in self.iter() {
                if !is_first {
                    out.push(b' ')
                }
                is_first = false;
                ann.encode_bytes(out);
            }
            Ok(())
        })
        .expect("annotation encoding is infallible");
    }
}

/// Length of some container, usually stored as fixed-length number.
type Len = u32;

/// Put length of something.
fn put_len(len: Len, out: &mut Vec<u8>) {
    out.extend_from_slice(&len.to_be_bytes())
}

/// Put bytestring (with its length).
fn put_bytes(bs: &[u8], out: &mut Vec<u8>) {
    out.push(BYTES_TAG);
    put_len(bs.len() as Len, out);
    out.extend_from_slice(bs)
}

/// Put a Michelson string.
fn put_string(s: &str, out: &mut Vec<u8>) {
    out.push(STRING_TAG);
    put_len(s.len() as Len, out);
    out.extend_from_slice(s.as_bytes())
}

fn with_patchback_len_result(
    out: &mut Vec<u8>,
    f: impl FnOnce(&mut Vec<u8>) -> BinResult,
) -> BinResult {
    put_len(0, out); // don't know the right length in advance
    let i = out.len();
    let len_place = (i - size_of::<Len>())..i; // to fill length later
    f(out)?;
    let len_of_written = (out.len() - i) as Len;
    out[len_place].copy_from_slice(&len_of_written.to_be_bytes());
    Ok(())
}

/// Worklist frame for the iterative encoder. Visit pushes children in
/// reverse along with any post-children frames so LIFO popping yields the
/// correct byte order.
enum EncFrame<'a> {
    Visit(&'a Micheline<'a>),
    PatchLen { len_place: Range<usize> },
    WriteAnnots(&'a Annotations<'a>),
}

/// Write a 4-byte length placeholder and return the range to patch later.
fn open_len(out: &mut Vec<u8>) -> Range<usize> {
    put_len(0, out);
    let end = out.len();
    (end - size_of::<Len>())..end
}

/// Patch a length placeholder with the byte distance from the placeholder
/// to the current write head.
fn patch_len(out: &mut Vec<u8>, len_place: Range<usize>) {
    let len = (out.len() - len_place.end) as Len;
    out[len_place].copy_from_slice(&len.to_be_bytes());
}

fn visit_node<'a>(
    node: &'a Micheline<'a>,
    out: &mut Vec<u8>,
    frames: &mut Vec<EncFrame<'a>>,
) -> BinResult {
    use Micheline::*;
    match node {
        Int(n) => {
            out.push(NUMBER_TAG);
            Zarith(n.clone()).bin_write(out)?;
        }
        String(s) => put_string(s, out),
        Bytes(b) => put_bytes(b, out),
        Seq(s) => {
            out.push(SEQ_TAG);
            let len_place = open_len(out);
            frames.push(EncFrame::PatchLen { len_place });
            for child in s.iter().rev() {
                frames.push(EncFrame::Visit(child));
            }
        }
        App(prim, args, anns) => {
            let with_annots = !anns.is_empty();
            match args.len() {
                0 => {
                    out.push(if with_annots {
                        APP_NO_ARGS_WITH_ANNOTS_TAG
                    } else {
                        APP_NO_ARGS_NO_ANNOTS_TAG
                    });
                    prim.encode(out);
                    if with_annots {
                        anns.encode_bytes(out);
                    }
                }
                1 => {
                    out.push(if with_annots {
                        APP_ONE_ARG_WITH_ANNOTS_TAG
                    } else {
                        APP_ONE_ARG_NO_ANNOTS_TAG
                    });
                    prim.encode(out);
                    if with_annots {
                        frames.push(EncFrame::WriteAnnots(anns));
                    }
                    frames.push(EncFrame::Visit(&args[0]));
                }
                2 => {
                    out.push(if with_annots {
                        APP_TWO_ARGS_WITH_ANNOTS_TAG
                    } else {
                        APP_TWO_ARGS_NO_ANNOTS_TAG
                    });
                    prim.encode(out);
                    if with_annots {
                        frames.push(EncFrame::WriteAnnots(anns));
                    }
                    frames.push(EncFrame::Visit(&args[1]));
                    frames.push(EncFrame::Visit(&args[0]));
                }
                _ => {
                    out.push(APP_GENERIC);
                    prim.encode(out);
                    let len_place = open_len(out);
                    // APP_GENERIC always writes an annot block, even when
                    // empty (a 4-byte zero length).
                    frames.push(EncFrame::WriteAnnots(anns));
                    frames.push(EncFrame::PatchLen { len_place });
                    for arg in args.iter().rev() {
                        frames.push(EncFrame::Visit(arg));
                    }
                }
            }
        }
    }
    Ok(())
}

/// Encode a `Micheline` value using an explicit-frame worklist instead of
/// Rust recursion, so encoding does not depend on call-stack depth.
fn encode_micheline<'a>(mich: &'a Micheline<'a>, out: &mut Vec<u8>) -> BinResult {
    let mut frames: Vec<EncFrame<'a>> = vec![EncFrame::Visit(mich)];
    while let Some(frame) = frames.pop() {
        match frame {
            EncFrame::Visit(node) => visit_node(node, out, &mut frames)?,
            EncFrame::PatchLen { len_place } => patch_len(out, len_place),
            EncFrame::WriteAnnots(anns) => anns.encode_bytes(out),
        }
    }
    Ok(())
}

impl Micheline<'_> {
    /// Serialize a value. Charges `interpret_cost::micheline_encoding`
    /// (mirrors L1's `cost_ENCODING_MICHELINE`) against `gas` before
    /// writing. The outer `Result` reports gas exhaustion; the inner
    /// `Result` reports serialization failures.
    pub fn encode(&self, gas: &mut Gas) -> Result<Result<Vec<u8>, BinError>, OutOfGas> {
        self.encode_starting_with(&[], gas)
    }

    /// Serialize a value like PACK does. Same gas semantics as [`Self::encode`].
    pub fn encode_for_pack(&self, gas: &mut Gas) -> Result<Result<Vec<u8>, BinError>, OutOfGas> {
        self.encode_starting_with(&[0x05], gas)
    }

    /// Like [`Self::encode`], but allows specifying a prefix, useful for
    /// `PACK` implementation.
    pub(crate) fn encode_starting_with(
        &self,
        start_bytes: &[u8],
        gas: &mut Gas,
    ) -> Result<Result<Vec<u8>, BinError>, OutOfGas> {
        gas.consume(interpret_cost::micheline_encoding(self).map_err(|_| OutOfGas)?)?;
        let mut out = Vec::from(start_bytes);
        Ok(encode_micheline(self, &mut out).map(|()| out))
    }
}

#[cfg(test)]
mod test_encoding {
    use super::*;

    #[track_caller]
    fn check<'a>(v: impl Into<Micheline<'a>>, hex_bytes: &str) {
        let hex_bytes: &str = hex_bytes
            .strip_prefix("0x")
            .unwrap_or_else(|| panic!("The `expected` argument must start from 0x"));
        assert_eq!(
            v.into().encode(&mut Gas::default()).unwrap().unwrap(),
            hex::decode(hex_bytes)
                .unwrap_or_else(|_| panic!("Bad hex string in `expected` argument"))
        )
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
        fn very_long_list() {
            // Using "{ $(printf 'Unit;%.0s' {1..1000}) }" as a value
            // Verifies that length is encoded as a fixed-length number, not as zarith
            check(
                Micheline::Seq(&vec![app!(Unit); 1000]),
                "0x02000007d0030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b030b",
          );
        }
    }

    /// Depth-stress tests for the iterative encoder. The depth here is
    /// bounded by `gas::collect_micheline_size`, which still walks the
    /// tree recursively and is charged via `interpret_cost::micheline_encoding`
    /// inside the gas-threaded `encode_starting_with`. The fully unbounded
    /// depth-100k variants will return alongside the iterative
    /// `collect_micheline_size` (tracked separately). The symmetric decoder
    /// regression already runs at depth 100k because the decoder's gas
    /// charge is byte-based (`micheline_decoding_bytes`) and does not
    /// traverse the tree.
    mod deep_recursion {
        use super::*;
        use crate::ast::annotations::NO_ANNS;
        use typed_arena::Arena;

        /// Bounded by the still-recursive `gas::collect_micheline_size`.
        /// On a 2 MiB test thread this comfortably fits; 100k would not.
        const DEPTH: usize = 1_000;

        #[test]
        fn deeply_nested_seq_encodes() {
            let arena: Arena<Micheline<'_>> = Arena::new();
            let mut current: Micheline<'_> = Micheline::Seq(&[]);
            for _ in 0..DEPTH {
                let slot = arena.alloc_extend(std::iter::once(current));
                current = Micheline::Seq(slot);
            }
            let bytes = current
                .encode(&mut Gas::default())
                .expect("no OOG with unmetered")
                .expect("deep seq encodes");
            assert!(!bytes.is_empty());
        }

        #[test]
        fn deeply_nested_pair_encodes() {
            use crate::lexer::Prim;
            let arena: Arena<Micheline<'_>> = Arena::new();
            let mut current: Micheline<'_> = Micheline::Int(0.into());
            for _ in 0..DEPTH {
                let pair_args = arena.alloc_extend([Micheline::Int(0.into()), current]);
                current = Micheline::App(Prim::Pair, pair_args, NO_ANNS);
            }
            let bytes = current
                .encode(&mut Gas::default())
                .expect("no OOG with unmetered")
                .expect("deep pair encodes");
            assert!(!bytes.is_empty());
        }
    }

    /// Tests that exercise the `Result`-returning encode contract.
    mod result_contract {
        use super::*;

        /// Confirms `encode` returns `Ok(Ok(_))` for a typical Micheline
        /// value: outer `Result` reports gas exhaustion, inner reports
        /// serialization failures.
        #[test]
        fn encode_returns_nested_ok() {
            let bytes = Micheline::Int(42.into())
                .encode(&mut Gas::default())
                .expect("no OOG with unmetered")
                .expect("no BinError for Int");
            assert!(!bytes.is_empty());
        }

        /// Confirms `encode_for_pack` likewise nests `Result`s and prefixes
        /// with `0x05`.
        #[test]
        fn encode_for_pack_returns_nested_ok() {
            let bytes = Micheline::Int(0.into())
                .encode_for_pack(&mut Gas::default())
                .expect("no OOG with unmetered")
                .expect("no BinError for Int");
            assert_eq!(bytes.first(), Some(&0x05));
        }

        /// Confirms that a `BinError` produced by an inner encoder propagates
        /// out of `with_patchback_len_result` correctly.
        #[test]
        fn patchback_propagates_errors() {
            let mut out = vec![];
            let err: BinResult = with_patchback_len_result(&mut out, |_out| {
                Err(BinError::custom("synthetic encode failure".to_string()))
            });
            assert!(err.is_err());
            let msg = format!("{}", err.unwrap_err());
            assert!(
                msg.contains("synthetic encode failure"),
                "unexpected error message: {msg}"
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
    }
}

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
use nom::sequence::{pair, preceded, tuple};
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
/// Sequence encoding case tag.
pub const MICHELINE_SEQ_TAG: u8 = 2;
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
/// any primitive (with or without annotations) encoding case tag.
pub const MICHELINE_PRIM_GENERIC_TAG: u8 = 9;
/// Bytes encoding case tag.
pub const MICHELINE_BYTES_TAG: u8 = 10;

// -----------
// Annotations
// -----------

mod annots {
    use regex::Regex;
    use std::fmt;
    use tezos_data_encoding::enc::{self, BinResult, BinWriter};
    use tezos_data_encoding::nom::{self as nom_read, NomError, NomReader, NomResult};

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub struct Annotation(String);

    #[derive(Debug, PartialEq, Eq, Clone, Default)]
    pub struct Annotations(Vec<Annotation>);

    impl fmt::Display for Annotation {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let Annotation(s) = self;
            write!(f, "{}", s)
        }
    }

    impl fmt::Display for Annotations {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let Annotations(vec) = self;
            match vec.as_slice() {
                [] => (),
                [a, rest @ ..] => {
                    a.fmt(f)?;
                    for a in rest {
                        f.write_str(" ")?;
                        a.fmt(f)?;
                    }
                }
            }
            Ok(())
        }
    }

    impl BinWriter for Annotations {
        fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
            enc::string(self.to_string(), output)
        }
    }

    impl NomReader for Annotations {
        fn nom_read(input: &[u8]) -> NomResult<Self> {
            // TODO: #6665
            // this does two passes over the input buffer (up to `dynamic` size)
            // could be done in a single pass instead, but for future work
            // (would require replacing the regex too, with a handwritten parser).
            use nom::error::ParseError;

            let (remaining, anns) = nom_read::string(input)?;
            let re =
                Regex::new(r"^[@:$&%!?][a-zA-Z0-9_.%@]*$").expect("Expected valid regex");

            anns.split(' ')
                .filter(|&x| !x.is_empty()) // needed for the empty string
                .map(|ann| {
                    if re.is_match(ann) {
                        Ok(Annotation(ann.into()))
                    } else {
                        Err(nom::Err::Error(NomError::from_error_kind(
                            input,
                            nom::error::ErrorKind::RegexpMatch,
                        )))
                    }
                })
                .collect::<Result<Vec<Annotation>, nom::Err<NomError>>>()
                .map(|anns| (remaining, Annotations(anns)))
        }
    }

    impl Annotations {
        pub(crate) fn is_empty(&self) -> bool {
            self.0.is_empty()
        }
    }
}

use annots::Annotations;

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
    pub(crate) annots: Annotations,
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
    pub(crate) annots: Annotations,
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
    pub(crate) annots: Annotations,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Node {
    Int(Zarith),
    String(String),
    Bytes(Vec<u8>),
    Seq(Vec<Node>),
    Prim {
        prim_tag: u8,
        args: Vec<Node>,
        annots: Annotations,
    },
}

impl From<MichelineInt> for Node {
    fn from(i: MichelineInt) -> Self {
        let MichelineInt(i) = i;
        Node::Int(i)
    }
}

impl From<MichelineString> for Node {
    fn from(s: MichelineString) -> Self {
        let MichelineString(s) = s;
        Node::String(s)
    }
}

impl From<MichelineBytes> for Node {
    fn from(s: MichelineBytes) -> Self {
        let MichelineBytes(s) = s;
        Node::Bytes(s)
    }
}

impl<const PRIM_TAG: u8> From<MichelinePrimNoArgsNoAnnots<PRIM_TAG>> for Node {
    fn from(_: MichelinePrimNoArgsNoAnnots<PRIM_TAG>) -> Self {
        Node::Prim {
            prim_tag: PRIM_TAG,
            args: vec![],
            annots: Annotations::default(),
        }
    }
}

impl<const PRIM_TAG: u8> From<MichelinePrimNoArgsSomeAnnots<PRIM_TAG>> for Node {
    fn from(a: MichelinePrimNoArgsSomeAnnots<PRIM_TAG>) -> Self {
        Node::Prim {
            prim_tag: PRIM_TAG,
            args: vec![],
            annots: a.annots,
        }
    }
}

impl<Arg, const PRIM_TAG: u8> From<MichelinePrim1ArgNoAnnots<Arg, PRIM_TAG>> for Node
where
    Arg: Debug + PartialEq + Eq,
    Node: From<Arg>,
{
    fn from(a: MichelinePrim1ArgNoAnnots<Arg, PRIM_TAG>) -> Self {
        Node::Prim {
            prim_tag: PRIM_TAG,
            args: vec![a.arg.into()],
            annots: Annotations::default(),
        }
    }
}

impl<Arg, const PRIM_TAG: u8> From<MichelinePrim1ArgSomeAnnots<Arg, PRIM_TAG>> for Node
where
    Arg: Debug + PartialEq + Eq,
    Node: From<Arg>,
{
    fn from(a: MichelinePrim1ArgSomeAnnots<Arg, PRIM_TAG>) -> Self {
        Node::Prim {
            prim_tag: PRIM_TAG,
            args: vec![a.arg.into()],
            annots: a.annots,
        }
    }
}

impl<Arg1, Arg2, const PRIM_TAG: u8>
    From<MichelinePrim2ArgsNoAnnots<Arg1, Arg2, PRIM_TAG>> for Node
where
    Arg1: Debug + PartialEq + Eq,
    Arg2: Debug + PartialEq + Eq,
    Node: From<Arg1>,
    Node: From<Arg2>,
{
    fn from(a: MichelinePrim2ArgsNoAnnots<Arg1, Arg2, PRIM_TAG>) -> Self {
        Node::Prim {
            prim_tag: PRIM_TAG,
            args: vec![a.arg1.into(), a.arg2.into()],
            annots: Annotations::default(),
        }
    }
}

impl<Arg1, Arg2, const PRIM_TAG: u8>
    From<MichelinePrim2ArgsSomeAnnots<Arg1, Arg2, PRIM_TAG>> for Node
where
    Arg1: Debug + PartialEq + Eq,
    Arg2: Debug + PartialEq + Eq,
    Node: From<Arg1>,
    Node: From<Arg2>,
{
    fn from(a: MichelinePrim2ArgsSomeAnnots<Arg1, Arg2, PRIM_TAG>) -> Self {
        Node::Prim {
            prim_tag: PRIM_TAG,
            args: vec![a.arg1.into(), a.arg2.into()],
            annots: a.annots,
        }
    }
}

// ----------
// CONVERSION
// ----------
impl From<i32> for MichelineInt {
    fn from(int: i32) -> Self {
        MichelineInt(Zarith(int.into()))
    }
}

impl From<i32> for Node {
    fn from(int: i32) -> Self {
        Node::Int(Zarith(int.into()))
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
            Annotations::nom_read,
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
            pair(Arg::nom_read, Annotations::nom_read),
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
            pair(Arg1::nom_read, pair(Arg2::nom_read, Annotations::nom_read)),
        );

        map(parse, |(arg1, (arg2, annots))| {
            MichelinePrim2ArgsSomeAnnots { arg1, arg2, annots }
        })(input)
    }
}

// Auxilary function for deserializing a `Node` in the `Prim` case.
// `micheline_prim_tag` should be one of the `MICHELINE_PRIM_*` tags
// and is expected to be the first byte of the input.  The next byte
// is interpreter as the tag for the primitive, and then the `next`
// parser is used to parse the arguments and annotations of the
// primitive application.
fn nom_read_app_aux<'a, T>(
    micheline_prim_tag: u8,
    next: impl FnMut(NomInput<'a>) -> NomResult<T>,
    into: impl Fn((u8, T)) -> Node,
) -> impl FnMut(NomInput<'a>) -> NomResult<Node> {
    use nom::number::complete::u8;

    preceded(tag([micheline_prim_tag]), map(pair(u8, next), into))
}

impl Node {
    fn nom_read_app(input: NomInput) -> NomResult<Node> {
        use nom::branch::alt;
        use nom::combinator::success;
        use Node::Prim;

        alt((
            nom_read_app_aux(
                MICHELINE_PRIM_NO_ARGS_NO_ANNOTS_TAG,
                success(()),
                |(prim_tag, ())| Prim {
                    prim_tag,
                    args: vec![],
                    annots: Annotations::default(),
                },
            ),
            nom_read_app_aux(
                MICHELINE_PRIM_NO_ARGS_SOME_ANNOTS_TAG,
                Annotations::nom_read,
                |(prim_tag, annots)| Prim {
                    prim_tag,
                    args: vec![],
                    annots,
                },
            ),
            nom_read_app_aux(
                MICHELINE_PRIM_1_ARG_NO_ANNOTS_TAG,
                Self::nom_read,
                |(prim_tag, arg)| Prim {
                    prim_tag,
                    args: vec![arg],
                    annots: Annotations::default(),
                },
            ),
            nom_read_app_aux(
                MICHELINE_PRIM_1_ARG_SOME_ANNOTS_TAG,
                pair(Self::nom_read, Annotations::nom_read),
                |(prim_tag, (arg, annots))| Prim {
                    prim_tag,
                    args: vec![arg],
                    annots,
                },
            ),
            nom_read_app_aux(
                MICHELINE_PRIM_2_ARGS_NO_ANNOTS_TAG,
                pair(Self::nom_read, Self::nom_read),
                |(prim_tag, (arg1, arg2))| Prim {
                    prim_tag,
                    args: vec![arg1, arg2],
                    annots: Annotations::default(),
                },
            ),
            nom_read_app_aux(
                MICHELINE_PRIM_2_ARGS_SOME_ANNOTS_TAG,
                tuple((Self::nom_read, Self::nom_read, Annotations::nom_read)),
                |(prim_tag, (arg1, arg2, annots))| Prim {
                    prim_tag,
                    args: vec![arg1, arg2],
                    annots,
                },
            ),
            nom_read_app_aux(
                MICHELINE_PRIM_GENERIC_TAG,
                pair(
                    nom_read::dynamic(nom_read::list(Node::nom_read)),
                    Annotations::nom_read,
                ),
                |(prim_tag, (args, annots))| Prim {
                    prim_tag,
                    args,
                    annots,
                },
            ),
        ))(input)
    }
    fn nom_read_seq(input: NomInput) -> NomResult<Node> {
        let parse = preceded(
            tag([MICHELINE_SEQ_TAG]),
            nom_read::dynamic(nom_read::list(Node::nom_read)),
        );
        map(parse, Node::Seq)(input)
    }
}

impl NomReader for Node {
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        nom::branch::alt((
            map(nom_read_micheline_int, Node::Int),
            map(nom_read_micheline_string, Node::String),
            map(nom_read_micheline_bytes(nom_read::bytes), Node::Bytes),
            Self::nom_read_seq,
            Self::nom_read_app,
        ))(input)
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
        bin_write_prim_no_args_no_annots(PRIM_TAG, output)
    }
}

impl<const PRIM_TAG: u8> BinWriter for MichelinePrimNoArgsSomeAnnots<PRIM_TAG> {
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        bin_write_prim_no_args_some_annots(PRIM_TAG, &self.annots, output)
    }
}

impl<Arg, const PRIM_TAG: u8> BinWriter for MichelinePrim1ArgNoAnnots<Arg, PRIM_TAG>
where
    Arg: BinWriter + Debug + PartialEq + Eq,
{
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        bin_write_prim_1_arg_no_annots(PRIM_TAG, &self.arg, output)
    }
}

impl<Arg, const PRIM_TAG: u8> BinWriter for MichelinePrim1ArgSomeAnnots<Arg, PRIM_TAG>
where
    Arg: BinWriter + Debug + PartialEq + Eq,
{
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        bin_write_prim_1_arg_some_annots(PRIM_TAG, &self.arg, &self.annots, output)
    }
}

impl<Arg1, Arg2, const PRIM_TAG: u8> BinWriter
    for MichelinePrim2ArgsNoAnnots<Arg1, Arg2, PRIM_TAG>
where
    Arg1: BinWriter + Debug + PartialEq + Eq,
    Arg2: BinWriter + Debug + PartialEq + Eq,
{
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        bin_write_prim_2_args_no_annots(PRIM_TAG, &self.arg1, &self.arg2, output)
    }
}

impl<Arg1, Arg2, const PRIM_TAG: u8> BinWriter
    for MichelinePrim2ArgsSomeAnnots<Arg1, Arg2, PRIM_TAG>
where
    Arg1: BinWriter + Debug + PartialEq + Eq,
    Arg2: BinWriter + Debug + PartialEq + Eq,
{
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        bin_write_prim_2_args_some_annots(
            PRIM_TAG,
            &self.arg1,
            &self.arg2,
            &self.annots,
            output,
        )
    }
}

impl BinWriter for Node {
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        match self {
            Node::Int(i) => bin_write_micheline_int(i, output),
            Node::String(s) => bin_write_micheline_string(&s, output),
            Node::Bytes(s) => bin_write_micheline_bytes(enc::bytes)(s.as_slice(), output),
            Node::Seq(args) => bin_write_micheline_seq(args, output),
            Node::Prim {
                prim_tag,
                args,
                annots,
            } => match (args.as_slice(), annots.is_empty()) {
                ([], true) => bin_write_prim_no_args_no_annots(*prim_tag, output),
                ([], false) => {
                    bin_write_prim_no_args_some_annots(*prim_tag, annots, output)
                }
                ([arg], true) => bin_write_prim_1_arg_no_annots(*prim_tag, arg, output),
                ([arg], false) => {
                    bin_write_prim_1_arg_some_annots(*prim_tag, arg, annots, output)
                }
                ([arg0, arg1], true) => {
                    bin_write_prim_2_args_no_annots(*prim_tag, arg0, arg1, output)
                }
                ([arg0, arg1], false) => bin_write_prim_2_args_some_annots(
                    *prim_tag, arg0, arg1, annots, output,
                ),
                (_, true) => bin_write_prim_generic(
                    *prim_tag,
                    args,
                    &Annotations::default(),
                    output,
                ),
                (_, false) => bin_write_prim_generic(*prim_tag, args, annots, output),
            },
        }
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
pub(crate) fn bin_write_prim_no_args_no_annots(
    prim_tag: u8,
    output: &mut Vec<u8>,
) -> BinResult {
    enc::put_bytes(&[MICHELINE_PRIM_NO_ARGS_NO_ANNOTS_TAG, prim_tag], output);
    Ok(())
}

/// Write `PRIM_TAG` & `annots` into an `obj2` encoding, prefixed with the
/// [MICHELINE_PRIM_NO_ARGS_SOME_ANNOTS_TAG].
pub(crate) fn bin_write_prim_no_args_some_annots(
    prim_tag: u8,
    annots: &Annotations,
    output: &mut Vec<u8>,
) -> BinResult {
    enc::put_bytes(&[MICHELINE_PRIM_NO_ARGS_SOME_ANNOTS_TAG, prim_tag], output);

    annots.bin_write(output)?;

    Ok(())
}

/// Write `PRIM_TAG` & `arg` into an `obj2` encoding, prefixed with the
/// [MICHELINE_PRIM_1_ARG_NO_ANNOTS_TAG].
pub(crate) fn bin_write_prim_1_arg_no_annots<Arg>(
    prim_tag: u8,
    arg: &Arg,
    output: &mut Vec<u8>,
) -> BinResult
where
    Arg: BinWriter,
{
    enc::put_bytes(&[MICHELINE_PRIM_1_ARG_NO_ANNOTS_TAG, prim_tag], output);

    arg.bin_write(output)?;

    Ok(())
}

/// Write `PRIM_TAG`, `arg` & `annots` into an `obj3` encoding, prefixed with the
/// [MICHELINE_PRIM_1_ARG_SOME_ANNOTS_TAG].
pub(crate) fn bin_write_prim_1_arg_some_annots<Arg>(
    prim_tag: u8,
    arg: &Arg,
    annots: &Annotations,
    output: &mut Vec<u8>,
) -> BinResult
where
    Arg: BinWriter,
{
    enc::put_bytes(&[MICHELINE_PRIM_1_ARG_SOME_ANNOTS_TAG, prim_tag], output);

    arg.bin_write(output)?;
    annots.bin_write(output)?;

    Ok(())
}

/// Write `PRIM_TAG`, `arg1` & `arg2` into an `obj3` encoding, prefixed with the
/// [MICHELINE_PRIM_2_ARGS_NO_ANNOTS_TAG].
pub(crate) fn bin_write_prim_2_args_no_annots<Arg1, Arg2>(
    prim_tag: u8,
    arg1: &Arg1,
    arg2: &Arg2,
    output: &mut Vec<u8>,
) -> BinResult
where
    Arg1: BinWriter,
    Arg2: BinWriter,
{
    enc::put_bytes(&[MICHELINE_PRIM_2_ARGS_NO_ANNOTS_TAG, prim_tag], output);

    arg1.bin_write(output)?;
    arg2.bin_write(output)?;

    Ok(())
}

/// Write `PRIM_TAG`, `arg1`, `arg2` & `annots` into an `obj4` encoding, prefixed with the
/// [MICHELINE_PRIM_2_ARGS_SOME_ANNOTS_TAG].
pub(crate) fn bin_write_prim_2_args_some_annots<Arg1, Arg2>(
    prim_tag: u8,
    arg1: &Arg1,
    arg2: &Arg2,
    annots: &Annotations,
    output: &mut Vec<u8>,
) -> BinResult
where
    Arg1: BinWriter,
    Arg2: BinWriter,
{
    enc::put_bytes(&[MICHELINE_PRIM_2_ARGS_SOME_ANNOTS_TAG, prim_tag], output);

    arg1.bin_write(output)?;
    arg2.bin_write(output)?;
    annots.bin_write(output)?;

    Ok(())
}

/// Write `PRIM_TAG`, `args` & `annots` into an `obj3` encoding, prefixed with the
/// [MICHELINE_PRIM_GENERIC_TAG].
pub(crate) fn bin_write_prim_generic<Arg>(
    prim_tag: u8,
    args: &Vec<Arg>,
    annots: &Annotations,
    output: &mut Vec<u8>,
) -> BinResult
where
    Arg: BinWriter,
{
    enc::put_bytes(&[MICHELINE_PRIM_GENERIC_TAG, prim_tag], output);
    enc::dynamic(enc::list(Arg::bin_write))(args, output)?;
    Annotations::bin_write(annots, output)?;

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

pub(crate) fn bin_write_micheline_seq(
    args: &Vec<Node>,
    output: &mut Vec<u8>,
) -> BinResult {
    enc::put_bytes(&[MICHELINE_SEQ_TAG], output);
    enc::dynamic(enc::list(Node::bin_write))(args, output)?;

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    // Conversion from %str to Annotations is only implemented to
    // simplify writing tests. That's why it is not exposed outside
    // this test module.

    #[derive(Debug)]
    pub enum AnnotationsFromStringError {
        StringEncodingError(String),
        AnnotationsDecodingError(Vec<u8>),
    }

    impl TryFrom<&str> for Annotations {
        type Error = AnnotationsFromStringError;

        fn try_from(s: &str) -> Result<Self, Self::Error> {
            let mut buf = Vec::new();
            enc::string(s, &mut buf)
                .map_err(|_| AnnotationsFromStringError::StringEncodingError(s.into()))?;
            let (_remaining, annots) = Self::nom_read(&buf).map_err(|_| {
                AnnotationsFromStringError::AnnotationsDecodingError(buf.clone())
            })?;
            Ok(annots)
        }
    }

    #[test]
    fn micheline_no_annots_encode_decode() {
        assert_eq!("".split(' ').collect::<Vec<&str>>(), vec![""]);
        let tests: Vec<(&str, &[u8])> = vec![
            ("", b"\0\0\0\0"),
            (":foo", b"\0\0\0\x04:foo"),
            (":foo @bar", b"\0\0\0\x09:foo @bar"),
            (":foo @bar %baz", b"\0\0\0\x0e:foo @bar %baz"),
        ];

        for (s, bytes) in tests {
            let annots =
                Annotations::try_from(s).expect("Expect valid annotation string");

            let mut bin = Vec::new();
            annots.bin_write(&mut bin).unwrap();
            assert_eq!(bin, bytes);

            let (remaining_input, decoded): (&[u8], Annotations) =
                NomReader::nom_read(bytes).unwrap();
            assert!(remaining_input.is_empty());
            assert_eq!(decoded, annots);
        }
    }

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
    fn micheline_empty_string_encode() {
        let test = "";
        let mut expected = vec![1, 0, 0, 0, 0];
        expected.append(&mut test.as_bytes().to_vec());

        let mut bin = Vec::new();

        MichelineString(test.into()).bin_write(&mut bin).unwrap();

        assert_eq!(expected, bin);
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
    fn micheline_empty_string_decode() {
        let expected = "";
        let mut test = vec![1, 0, 0, 0, 0];
        test.append(&mut expected.as_bytes().to_vec());

        let (input_remaining, value) =
            MichelineString::nom_read(test.as_slice()).unwrap();

        assert!(input_remaining.is_empty());
        assert_eq!(MichelineString(expected.into()), value);
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

        let annots: Annotations = Annotations::try_from(":foo").unwrap();
        let expected = MichelinePrimNoArgsSomeAnnots::<98> { annots };

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

        let annots: Annotations = Annotations::try_from(":foo").unwrap();
        let test = MichelinePrimNoArgsSomeAnnots::<98> { annots };

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
        let annots: Annotations = Annotations::try_from(":foo").unwrap();
        let expected = MichelinePrim1ArgSomeAnnots::<_, 99> { arg: nat, annots };

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
        let annots: Annotations = Annotations::try_from(":foo").unwrap();
        let test = MichelinePrim1ArgSomeAnnots::<_, 99> { arg: nat, annots };

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
        let annots: Annotations = Annotations::try_from(":foo").unwrap();
        let expected = MichelinePrim2ArgsSomeAnnots::<_, _, 100> {
            arg1: nat,
            arg2: int,
            annots,
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
        let annots: Annotations = Annotations::try_from(":foo").unwrap();
        let test = MichelinePrim2ArgsSomeAnnots::<_, _, 100> {
            arg1: nat,
            arg2: int,
            annots,
        };

        let mut bin = Vec::new();
        test.bin_write(&mut bin).unwrap();

        assert_eq!(expected, bin);
    }

    #[test]
    fn micheline_pair3_annot_decode() {
        // Decode `Pair :foo Unit 0 0`
        let test = vec![
            9, // Prim_generic
            7, // Prim tag: Pair
            0, 0, 0, 6,  // length of args
            3,  // Prim_0 (no annots)
            11, // Prim tag: Unit
            0,  // Int tag
            0,  // 0
            0,  // Int tag
            0,  // 0
            0, 0, 0, 4, // length of the annotation string
            b':', b'f', b'o', b'o', // annotation
        ];

        let unit = MichelinePrimNoArgsNoAnnots::<11> {};
        let annots: Annotations = Annotations::try_from(":foo").unwrap();
        let expected = Node::Prim {
            prim_tag: 7,
            args: vec![unit.into(), 0.into(), 0.into()],
            annots,
        };

        let (remaining_input, optnatfoo) = NomReader::nom_read(test.as_slice()).unwrap();

        assert!(remaining_input.is_empty());
        assert_eq!(expected, optnatfoo);
    }

    #[test]
    fn micheline_pair3_annot_encode() {
        let expected = vec![
            9, // Prim_generic
            7, // Prim tag: Pair
            0, 0, 0, 6,  // length of args
            3,  // Prim_0 (no annots)
            11, // Prim tag: Unit
            0,  // Int tag
            0,  // 0
            0,  // Int tag
            0,  // 0
            0, 0, 0, 4, // length of the annotation string
            b':', b'f', b'o', b'o', // annotation
        ];

        let unit = MichelinePrimNoArgsNoAnnots::<11> {};
        let annots: Annotations = Annotations::try_from(":foo").unwrap();
        let test = Node::Prim {
            prim_tag: 7,
            args: vec![unit.into(), 0.into(), 0.into()],
            annots,
        };

        let mut bin = Vec::new();
        test.bin_write(&mut bin).unwrap();

        assert_eq!(expected, bin);
    }

    #[test]
    fn micheline_seq_decode() {
        // Decode `{Unit; 0; 0}`
        let test = vec![
            2, // Seq tag
            0, 0, 0, 6,  // length of args
            3,  // Prim_0 (no annots)
            11, // Prim tag: Unit
            0,  // Int tag
            0,  // 0
            0,  // Int tag
            0,  // 0
        ];

        let unit = MichelinePrimNoArgsNoAnnots::<11> {};
        let expected = Node::Seq(vec![unit.into(), 0.into(), 0.into()]);

        let (remaining_input, optnatfoo) = NomReader::nom_read(test.as_slice()).unwrap();

        assert!(remaining_input.is_empty());
        assert_eq!(expected, optnatfoo);
    }

    #[test]
    fn micheline_seq_encode() {
        let expected = vec![
            2, // Seq tag
            0, 0, 0, 6,  // length of args
            3,  // Prim_0 (no annots)
            11, // Prim tag: Unit
            0,  // Int tag
            0,  // 0
            0,  // Int tag
            0,  // 0
        ];

        let unit = MichelinePrimNoArgsNoAnnots::<11> {};
        let test = Node::Seq(vec![unit.into(), 0.into(), 0.into()]);

        let mut bin = Vec::new();
        test.bin_write(&mut bin).unwrap();

        assert_eq!(expected, bin);
    }

    fn hex_to_bigint(s: &str) -> num_bigint::BigInt {
        use num_traits::FromPrimitive;
        num_bigint::BigInt::from_u64(u64::from_str_radix(s, 16).unwrap()).unwrap()
    }
}

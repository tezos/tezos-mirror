/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use std::borrow::Cow;

use logos::Logos;
pub mod errors;
pub mod macros;

pub use errors::*;
use macros::*;
use num_bigint::BigInt;
use strum_macros::EnumCount;

/// Expand to the first argument if not empty; otherwise, the second argument.
macro_rules! coalesce {
    (, $r:expr) => {
        $r
    };
    ($l:expr, $r:expr) => {
        $l
    };
}

/// Takes a list of primitive names, creates a simple `enum` with the names
/// provided, and defines `FromStr` implementation using stringified
/// representation of the identifiers.
macro_rules! defprim {
    ($ty:ident; $($(#[token($str:expr)])? $prim:ident),* $(,)*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, EnumCount)]
        #[allow(non_camel_case_types, clippy::upper_case_acronyms)]
        #[repr(u8)]
        pub enum $ty {
            $($prim),*
        }

        impl std::fmt::Display for $ty {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        $ty::$prim => write!(f, "{}", coalesce!($($str)?, stringify!($prim))),
                        )*
                }
            }
        }


        impl std::str::FromStr for $ty {
            type Err = PrimError;
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    $(coalesce!($($str)?, stringify!($prim)) => Ok($ty::$prim),)*
                        _ => Err(PrimError(s.to_owned()))
                }
            }
        }
    };
}

// NB: Primitives will be lexed as written, so capitalization matters.
//
// NB: Order matters too, it is used in binary serialization.
// For the correct order, see
// `src/proto_alpha/lib_protocol/michelson_v1_primitives.ml` file in this
// repository, `prim_encoding` function.
// TODO: https://gitlab.com/tezos/tezos/-/issues/6632
// Add a test on ordering
defprim! {
    Prim;
    parameter, storage, code, False, Elt, Left,
    None, Pair, Right, Some, True, Unit,
    PACK, UNPACK, BLAKE2B, SHA256, SHA512, ABS,
    ADD, AMOUNT, AND, BALANCE, CAR, CDR,
    CHECK_SIGNATURE, COMPARE, CONCAT, CONS,
    CREATE_ACCOUNT, CREATE_CONTRACT, IMPLICIT_ACCOUNT, DIP,
    DROP, DUP, EDIV, EMPTY_MAP, EMPTY_SET, EQ,
    EXEC, FAILWITH, GE, GET, GT, HASH_KEY,
    IF, IF_CONS, IF_LEFT, IF_NONE, INT, LAMBDA,
    LE, LEFT, LOOP, LSL, LSR, LT, MAP,
    MEM, MUL, NEG, NEQ, NIL, NONE, NOT,
    NOW, OR, PAIR, PUSH, RIGHT, SIZE,
    SOME, SOURCE, SENDER, SELF, STEPS_TO_QUOTA,
    SUB, SWAP, TRANSFER_TOKENS, SET_DELEGATE, UNIT,
    UPDATE, XOR, ITER, LOOP_LEFT, ADDRESS,
    CONTRACT, ISNAT, CAST, RENAME, bool,
    contract, int, key, key_hash, lambda, list,
    map, big_map, nat, option, or, pair,
    set, signature, string, bytes, mutez,
    timestamp, unit, operation, address, SLICE,
    DIG, DUG, EMPTY_BIG_MAP, APPLY, chain_id,
    CHAIN_ID, LEVEL, SELF_ADDRESS, never, NEVER,
    UNPAIR, VOTING_POWER, TOTAL_VOTING_POWER, KECCAK,
    SHA3, PAIRING_CHECK, bls12_381_g1, bls12_381_g2,
    bls12_381_fr, sapling_state, sapling_transaction_deprecated,
    SAPLING_EMPTY_STATE, SAPLING_VERIFY_UPDATE, ticket,
    TICKET_DEPRECATED, READ_TICKET, SPLIT_TICKET,
    JOIN_TICKETS, GET_AND_UPDATE, chest, chest_key,
    OPEN_CHEST, VIEW, view, constant, SUB_MUTEZ,
    tx_rollup_l2_address, MIN_BLOCK_TIME, sapling_transaction,
    EMIT, Lambda_rec, LAMBDA_REC, TICKET, BYTES,
    NAT,
    // Unstable primitives (they are not part of a released protocol)
    Transfer_tokens, Set_delegate
    // If you add anything here, see the note about the order above.
}

impl Prim {
    // Our [Prim] enum has its variants in the right order, so its
    // discriminant should match the ID.
    pub fn encode(&self, out: &mut Vec<u8>) {
        out.push(*self as u8)
    }
}

defprim! {
    TztPrim;
    Stack_elt,
    input,
    output,
    Failed,
    amount,
    balance,
    MutezOverflow,
    GeneralOverflow,
    StaticError,
    #[token("self")]
    self_,
    #[token("_")]
    Underscore,
    other_contracts,
    Contract,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Noun {
    Prim(Prim),
    TztPrim(TztPrim),
    MacroPrim(Macro),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Annotation<'a> {
    Special(Cow<'a, str>),
    Field(Cow<'a, str>),
    Variable(Cow<'a, str>),
    Type(Cow<'a, str>),
}

impl Annotation<'_> {
    pub fn into_owned(self) -> Annotation<'static> {
        match self {
            Annotation::Special(s) => Annotation::Special(Cow::Owned(s.into_owned())),
            Annotation::Field(s) => Annotation::Field(Cow::Owned(s.into_owned())),
            Annotation::Variable(s) => Annotation::Variable(Cow::Owned(s.into_owned())),
            Annotation::Type(s) => Annotation::Type(Cow::Owned(s.into_owned())),
        }
    }
}

impl std::fmt::Display for Annotation<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Annotation::Special(s) => write!(f, "{s}"),
            Annotation::Field(s) => write!(f, "%{s}"),
            Annotation::Variable(s) => write!(f, "@{s}"),
            Annotation::Type(s) => write!(f, ":{s}"),
        }
    }
}

pub(crate) fn try_ann_from_str(value: &str) -> Option<Annotation> {
    match value {
        s @ ("@%" | "@%%" | "%@") => Some(Annotation::Special(Cow::Borrowed(s))),
        s => match s.as_bytes()[0] {
            b'@' => Some(Annotation::Variable(Cow::Borrowed(&s[1..]))),
            b'%' => Some(Annotation::Field(Cow::Borrowed(&s[1..]))),
            b':' => Some(Annotation::Type(Cow::Borrowed(&s[1..]))),
            _ => None,
        },
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Logos)]
#[logos(error = LexerError, skip r"[ \t\r\n\v\f]+|#[^\n]*\n")]
pub enum Tok<'a> {
    #[regex(r"[A-Za-z_][A-Za-z_0-9]*", lex_noun)]
    Noun(Noun),

    #[regex("([+-]?)[0-9]+", lex_number)]
    Number(BigInt),

    #[regex(r#""(\\.|[^\\"])*""#, lex_string)]
    String(String),

    #[regex(r#"0x[0-9a-fA-F]*"#, lex_bytes)]
    Bytes(Vec<u8>),

    // regex as per https://tezos.gitlab.io/active/michelson.html#syntax
    #[regex(r"@%|@%%|%@|[@:%][_0-9a-zA-Z][_0-9a-zA-Z\.%@]*", lex_annotation)]
    Annotation(Annotation<'a>),

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(";")]
    Semi,
}

impl std::fmt::Display for Noun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Noun::Prim(p) => p.fmt(f),
            Noun::TztPrim(p) => p.fmt(f),
            Noun::MacroPrim(m) => m.fmt(f),
        }
    }
}

impl std::fmt::Display for Tok<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Tok::Noun(noun) => noun.fmt(f),
            Tok::Number(n) => n.fmt(f),
            Tok::String(s) => s.fmt(f),
            Tok::Bytes(bs) => write!(f, "0x{}", hex::encode(bs)),
            Tok::Annotation(ann) => write!(f, "{ann}"),
            Tok::LParen => write!(f, "("),
            Tok::RParen => write!(f, ")"),
            Tok::LBrace => write!(f, "{{"),
            Tok::RBrace => write!(f, "}}"),
            Tok::Semi => write!(f, ";"),
        }
    }
}

type Lexer<'a> = logos::Lexer<'a, Tok<'a>>;

fn lex_macro(lex: &mut Lexer) -> Result<Macro, PrimError> {
    let slice = lex.slice();
    let mut inner = Macro::lexer(slice);
    let next = inner.next().ok_or_else(|| PrimError(slice.to_string()))?;
    // check if lexed token is at EOF
    if matches!(inner.next(), Some(_)) {
        return Err(PrimError(slice.to_string()));
    }
    next.map_err(|_| PrimError(slice.to_string()))
}

fn lex_noun(lex: &mut Lexer) -> Result<Noun, LexerError> {
    lex.slice()
        .parse()
        .map(Noun::Prim)
        .or_else(|_| lex.slice().parse().map(Noun::TztPrim))
        .or_else(|_| lex_macro(lex).map(Noun::MacroPrim))
        .map_err(LexerError::PrimError)
}

fn lex_number(lex: &mut Lexer) -> Result<BigInt, LexerError> {
    lex.slice()
        .parse()
        .map_err(|_| LexerError::NumericLiteral(lex.slice().to_owned()))
}

/// Takes a string _with_ the sourrounding quotes, strips the quotes, checks the
/// string is valid (i.e. contains only printable ASCII characters) and replaces
/// escapes with corresponding characters.
fn lex_string(lex: &mut Lexer) -> Result<String, LexerError> {
    let s = lex.slice();
    // strip the quotes
    let s = &s[1..s.len() - 1];

    // check if all characters are printable ASCII
    if !s.chars().all(|c| matches!(c, ' '..='~')) {
        return Err(LexerError::ForbiddenCharacterIn(s.to_owned()));
    }

    let mut res = String::new();
    // this may overreserve, but no more than 2x
    res.reserve(s.len());

    let unescape_char = |c| match c {
        'n' => Ok('\n'),
        'r' => Ok('\r'),
        '"' => Ok('"'),
        '\\' => Ok('\\'),
        _ => Err(LexerError::UndefinedEscape(c)),
    };

    let mut in_escape: bool = false;
    for c in s.chars() {
        if in_escape {
            res.push(unescape_char(c)?);
            in_escape = false;
        } else if matches!(c, '\\') {
            in_escape = true;
        } else {
            res.push(c);
        }
    }

    Ok(res)
}

/// Takes a lexed slice of hexadecimal digits prefixed by `0x`, removes the
/// prefix and converts the digits pairwise to `u8`.
fn lex_bytes(lex: &mut Lexer) -> Result<Vec<u8>, LexerError> {
    Ok(hex::decode(&lex.slice()[2..])?)
}

fn lex_annotation<'a>(lex: &mut Lexer<'a>) -> Annotation<'a> {
    try_ann_from_str(lex.slice()).expect("regex from annotation ensures it's valid")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unescape_string() {
        macro_rules! assert_parse {
            ($s:expr, $e:expr) => {
                assert_eq!(
                    Tok::lexer($s)
                        .map(|i| i.map_err(|x| x.to_string()))
                        .collect::<Vec<Result<Tok, String>>>(),
                    vec![($e as Result<&str, &str>)
                        .map(|v| Tok::String(v.to_owned()))
                        .map_err(|e| e.to_owned())]
                )
            };
        }
        assert_parse!(r#""bar""#, Ok("bar"));
        assert_parse!(r#""foo\nbar""#, Ok("foo\nbar"));
        assert_parse!(r#""foo\"bar\"""#, Ok("foo\"bar\""));
        assert_parse!(r#""foo\rbar""#, Ok("foo\rbar"));
        assert_parse!(r#""foo\\rbar""#, Ok("foo\\rbar"));
        assert_parse!(r#""foo\\nbar""#, Ok("foo\\nbar"));
        assert_parse!(r#""foo\\\\bar""#, Ok("foo\\\\bar"));
        // unicode is not accepted
        assert_parse!(
            r#""हिन्दी""#,
            Err("forbidden character found in string literal \"हिन्दी\"")
        );
        // unknown escapes are not accepted
        assert_parse!(r#""\a""#, Err("undefined escape sequence: \"\\a\""));
        // unterminated strings are not accepted
        assert_parse!(r#"""#, Err("unknown token"));
        assert_parse!(r#""\""#, Err("unknown token"));
    }

    #[test]
    fn unknown_prim_err() {
        assert_eq!(
            Tok::lexer("foo").next().unwrap().unwrap_err().to_string(),
            "unknown primitive: foo"
        )
    }

    #[test]
    fn lex_bytes_test() {
        #[track_caller]
        fn assert_parse<const N: usize>(s: &str, e: Result<[u8; N], &str>) {
            assert_eq!(
                Tok::lexer(s).map(|i| i.map_err(|x| x.to_string())).last(),
                Some(e.map(|v| Tok::Bytes(v.to_vec())).map_err(|e| e.to_owned()))
            )
        }
        assert_parse("0x01", Ok([0x01]));
        assert_parse("0x010203", Ok([0x01, 0x02, 0x03]));
        assert_parse("0xfffe", Ok([0xff, 0xfe]));
        assert_parse("0x0000", Ok([0x00, 0x00]));
        assert_parse("0xabcd", Ok([0xab, 0xcd]));
        assert_parse("0x", Ok([]));
        assert_parse::<0>("0x1", Err("invalid hex sequence: Odd number of digits"));
        assert_parse::<0>("0xzz", Err("unknown primitive: zz"));
    }
}

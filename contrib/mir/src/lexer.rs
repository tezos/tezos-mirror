/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use logos::Logos;

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
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        #[allow(non_camel_case_types, clippy::upper_case_acronyms)]
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

#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
#[error("unknown primitive: {0}")]
pub struct PrimError(String);

// NB: Primitives will be lexed as written, so capitalization matters.
defprim! {
    Prim;
    parameter,
    storage,
    code,
    int,
    nat,
    bool,
    mutez,
    string,
    unit,
    operation,
    pair,
    option,
    list,
    map,
    True,
    False,
    Unit,
    None,
    Pair,
    Some,
    Elt,
    PUSH,
    INT,
    GT,
    EQ,
    LE,
    LOOP,
    DIP,
    ADD,
    DROP,
    SWAP,
    IF,
    DUP,
    FAILWITH,
    UNIT,
    CAR,
    CDR,
    PAIR,
    IF_NONE,
    SOME,
    COMPARE,
    AMOUNT,
    NIL,
    GET,
    UPDATE,
    UNPAIR,
    CONS,
    IF_CONS,
    ITER,
    or,
    Left,
    Right,
    IF_LEFT,
    contract,
    address,
    chain_id,
    CHAIN_ID,
    SELF,
    bytes,
    key,
    signature,
    CHECK_SIGNATURE,
}

defprim! {
    TztPrim;
    Stack_elt,
    input,
    output,
    Failed,
    amount,
    MutezOverflow,
    GeneralOverflow,
    StaticError,
    #[token("self")]
    self_,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimWithTzt {
    Prim(Prim),
    TztPrim(TztPrim),
    Underscore,
    // Including underscore spearately from TztPrim because the `defprim` macro won't work if we
    // used a literal underscore there. parsing for this variant is handled specially in the
    // `lex_prim` function as well.
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Annotation<'a> {
    Special(&'a str),
    Field(&'a str),
    Variable(&'a str),
    Type(&'a str),
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

#[derive(Debug, Clone, PartialEq, Eq, Logos)]
#[logos(error = LexerError, skip r"[ \t\r\n\v\f]+|#[^\n]*\n")]
pub enum Tok<'a> {
    #[regex(r"[A-Za-z_]+", lex_prim)]
    Prim(PrimWithTzt),

    #[regex("([+-]?)[0-9]+", lex_number)]
    Number(i128),

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

impl std::fmt::Display for Tok<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tok::Prim(PrimWithTzt::Prim(p)) => p.fmt(f),
            Tok::Prim(PrimWithTzt::TztPrim(p)) => p.fmt(f),
            Tok::Prim(PrimWithTzt::Underscore) => write!(f, "_"),
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

#[derive(Debug, PartialEq, Clone, thiserror::Error)]
pub enum LexerError {
    #[error("unknown token")]
    UnknownToken,
    #[error("parsing of numeric literal {0} failed")]
    NumericLiteral(String),
    #[error("forbidden character found in string literal \"{0}\"")]
    ForbiddenCharacterIn(String),
    #[error("undefined escape sequence: \"\\{0}\"")]
    UndefinedEscape(char),
    #[error(transparent)]
    PrimError(#[from] PrimError),
    #[error("invalid hex sequence: {0}")]
    InvalidHex(#[from] hex::FromHexError),
}

impl Default for LexerError {
    fn default() -> Self {
        LexerError::UnknownToken
    }
}

type Lexer<'a> = logos::Lexer<'a, Tok<'a>>;

fn lex_prim(lex: &mut Lexer) -> Result<PrimWithTzt, LexerError> {
    lex.slice()
        .parse()
        .map(PrimWithTzt::Prim)
        .or_else(|_| lex.slice().parse().map(PrimWithTzt::TztPrim))
        .or_else(|_| match lex.slice() {
            "_" => Ok(PrimWithTzt::Underscore),
            s => Err(PrimError(s.to_owned())),
        })
        .map_err(LexerError::from)
}

fn lex_number(lex: &mut Lexer) -> Result<i128, LexerError> {
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
    match lex.slice() {
        s @ ("@%" | "@%%" | "%@") => Annotation::Special(s),
        s => {
            if let Some(s) = s.strip_prefix('@') {
                Annotation::Variable(s)
            } else if let Some(s) = s.strip_prefix('%') {
                Annotation::Field(s)
            } else if let Some(s) = s.strip_prefix(':') {
                Annotation::Type(s)
            } else {
                unreachable!("regex for Annotation ensures it's either one of three")
            }
        }
    }
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

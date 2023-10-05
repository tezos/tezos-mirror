/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use logos::Logos;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum Prim {
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
}

impl std::fmt::Display for Prim {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", &self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Logos)]
#[logos(error = LexerError, skip r"[ \t\r\n\v\f]+")]
pub enum Tok {
    #[token("int", |_| Prim::int)]
    #[token("nat", |_| Prim::nat)]
    #[token("bool", |_| Prim::bool)]
    #[token("mutez", |_| Prim::mutez)]
    #[token("string", |_| Prim::string)]
    #[token("unit", |_| Prim::unit)]
    #[token("operation", |_| Prim::operation)]
    #[token("pair", |_| Prim::pair)]
    #[token("option", |_| Prim::option)]
    #[token("list", |_| Prim::list)]
    #[token("map", |_| Prim::map)]
    #[token("True", |_| Prim::True)]
    #[token("False", |_| Prim::False)]
    #[token("Unit", |_| Prim::Unit)]
    #[token("None", |_| Prim::None)]
    #[token("Pair", |_| Prim::Pair)]
    #[token("Some", |_| Prim::Some)]
    #[token("Elt", |_| Prim::Elt)]
    #[token("PUSH", |_| Prim::PUSH)]
    #[token("INT", |_| Prim::INT)]
    #[token("GT", |_| Prim::GT)]
    #[token("LOOP", |_| Prim::LOOP)]
    #[token("DIP", |_| Prim::DIP)]
    #[token("ADD", |_| Prim::ADD)]
    #[token("DROP", |_| Prim::DROP)]
    #[token("SWAP", |_| Prim::SWAP)]
    #[token("IF", |_| Prim::IF)]
    #[token("DUP", |_| Prim::DUP)]
    #[token("FAILWITH", |_| Prim::FAILWITH)]
    #[token("UNIT", |_| Prim::UNIT)]
    #[token("CAR", |_| Prim::CAR)]
    #[token("CDR", |_| Prim::CDR)]
    #[token("PAIR", |_| Prim::PAIR)]
    #[token("IF_NONE", |_| Prim::IF_NONE)]
    #[token("SOME", |_| Prim::SOME)]
    #[token("COMPARE", |_| Prim::COMPARE)]
    #[token("AMOUNT", |_| Prim::AMOUNT)]
    #[token("NIL", |_| Prim::NIL)]
    #[token("GET", |_| Prim::GET)]
    #[token("UPDATE", |_| Prim::UPDATE)]
    Prim(Prim),

    #[regex("([+-]?)[0-9]+", lex_number)]
    Number(i128),

    #[regex(r#""(\\.|[^\\"])*""#, lex_string)]
    String(String),

    // regex as per https://tezos.gitlab.io/active/michelson.html#syntax
    #[regex(r"@%|@%%|%@|[@:%][_0-9a-zA-Z][_0-9a-zA-Z\.%@]*")]
    Annotation,

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

impl std::fmt::Display for Tok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Tok::Prim(p) => p.fmt(f),
            Tok::Number(n) => n.fmt(f),
            Tok::String(s) => s.fmt(f),
            Tok::Annotation => write!(f, "<ann>"),
            Tok::LParen => write!(f, "("),
            Tok::RParen => write!(f, ")"),
            Tok::LBrace => write!(f, "{{"),
            Tok::RBrace => write!(f, "}}"),
            Tok::Semi => write!(f, ";"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum LexerError {
    #[error("unknown token")]
    UnknownToken,
    #[error("parsing of numeric literal {0} failed")]
    NumericLiteral(String),
    #[error("forbidden character found in string literal \"{0}\"")]
    ForbiddenCharacterIn(String),
    #[error("undefined escape sequence: \"\\{0}\"")]
    UndefinedEscape(char),
}

impl Default for LexerError {
    fn default() -> Self {
        LexerError::UnknownToken
    }
}

type Lexer<'a> = logos::Lexer<'a, Tok>;

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
}

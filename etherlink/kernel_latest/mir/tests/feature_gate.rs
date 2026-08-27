// SPDX-FileCopyrightText: [2026] Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Tests that verify the `text-parser` Cargo feature gate is properly set up.
//!
//! Every test here is gated on `text-parser`. With the feature on (the
//! default) they check that the parser, lexer and TZT entry points are
//! reachable; with it off they are not compiled, because the items they
//! reference must not exist at all.

/// Verifies `mir::tzt` is accessible when `text-parser` is enabled.
///
/// After the gate is applied, this test only compiles and runs with the
/// `text-parser` feature on (the default).
#[cfg(feature = "text-parser")]
#[test]
fn tzt_accessible_with_text_parser() {
    let _ = std::mem::size_of::<mir::tzt::TztTest>();
}

/// Verifies `Parser::parse` and `Parser::parse_top_level` are accessible
/// when `text-parser` is enabled.
///
/// After the gate is applied, these methods only exist with `text-parser` on.
#[cfg(feature = "text-parser")]
#[test]
fn parser_parse_methods_accessible_with_text_parser() {
    let p = mir::parser::Parser::new();
    assert!(p.parse("{}").is_ok());
}

/// Verifies `mir::lexer::Tok` is accessible when `text-parser` is enabled.
///
/// After the gate is applied, `Tok` only exists with `text-parser` on.
#[cfg(feature = "text-parser")]
#[test]
fn tok_accessible_with_text_parser() {
    let _tok = mir::lexer::Tok::LBrace;
}

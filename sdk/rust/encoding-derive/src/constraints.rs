// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-License-Identifier: MIT

use proc_macro2::{Span, TokenStream};
use syn::{punctuated::Punctuated, Token, WhereClause, WherePredicate};

/// A stream of tokens with additional constraints to be added as a
/// where clause. This structure is used to gather constraints of the
/// form T : Trait while traversing an encoding to produce the
/// implementation of Trait.
pub(crate) struct TokenStreamWithConstraints {
    pub stream: TokenStream,
    pub constraints: Punctuated<WherePredicate, Token![,]>,
}

impl TokenStreamWithConstraints {
    pub fn map_stream(self, f: impl FnOnce(TokenStream) -> TokenStream) -> Self {
        let Self {
            stream,
            constraints,
        } = self;
        Self {
            stream: f(stream),
            constraints,
        }
    }
}

impl From<TokenStream> for TokenStreamWithConstraints {
    fn from(stream: TokenStream) -> Self {
        Self {
            stream,
            constraints: Punctuated::new(),
        }
    }
}

pub(crate) fn add_where_clauses(
    where_clause: Option<WhereClause>,
    constraints: Punctuated<WherePredicate, Token![,]>,
    span: Span,
) -> Option<WhereClause> {
    if constraints.is_empty() {
        return where_clause;
    }
    match where_clause {
        None => Some(WhereClause {
            where_token: Token![where](span),
            predicates: constraints,
        }),
        Some(WhereClause {
            where_token,
            mut predicates,
        }) => {
            predicates.extend(constraints);
            Some(WhereClause {
                where_token,
                predicates,
            })
        }
    }
}

// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use syn::{
    parse::{Parse, ParseStream},
    Ident, LitStr, Token,
};

#[derive(Debug)]
pub(crate) struct RuntimeConfig {
    pub static_inbox: Option<String>,
}

impl Parse for RuntimeConfig {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        if input.peek(syn::Ident) {
            let arg_name: Ident = input.parse()?;
            if arg_name != "static_inbox" {
                return Err(syn::Error::new_spanned(
                    &arg_name,
                    format!("unsupported runtime attribute `{arg_name}`, expected `static_inbox`"),
                ));
            }

            let _: Token![=] = input.parse()?;

            let path: LitStr = input.parse()?;

            Ok(Self {
                static_inbox: Some(path.value()),
            })
        } else if input.is_empty() {
            Ok(Self { static_inbox: None })
        } else {
            Err(input.error("Expected identifier `static_inbox` or no arguments"))
        }
    }
}

// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Ident, ItemFn, LitStr, Token,
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

/// Implementation for [`super::runtime`].
pub(crate) fn runtime_attribute(attr: TokenStream, item: TokenStream) -> TokenStream {
    let runtime: RuntimeConfig = parse_macro_input!(attr);

    let code: TokenStream = match runtime.static_inbox {
        None => {
            // No inbox path given, behave as a no-op
            item
        }
        Some(path) => {
            let ItemFn {
                attrs,
                vis,
                sig,
                block,
            } = parse_macro_input!(item);
            let fn_stmts = &block.stmts;
            let fn_name = sig.ident.to_token_stream();
            // An inbox path was given, evaluate in the shell for environment variables.
            let input_file = shellexpand::full(&path).unwrap();
            quote! {
                // Function attributes are passed to the nested function to keep
                // "intuitive" style of calling syntactic modifiers.
                // More detailed expalantion in tezos_smart_rollup::entrypoint module.
                #vis fn #fn_name(host: &mut impl Runtime) {
                    // Override the function name so that we call into the nested kernel.
                    // While keeping the name for the outer function the same so that
                    // the outer macros keep operating on the same name (but modified contents)
                    #(#attrs)*
                    #sig {
                        #(#fn_stmts)*
                    }

                    mod __tezos_runtime_static_inbox {
                        use tezos_smart_rollup::entrypoint::internal::StaticInbox;
                        use std::cell::RefCell;
                        use std::thread_local;
                        use tezos_smart_rollup::prelude::*;

                        const INPUT: &str = include_str!(#input_file);

                        thread_local! {
                            pub static STATIC_INBOX: RefCell<StaticInbox> = RefCell::new(StaticInbox::new_from_json(INPUT));
                        }

                    }
                    __tezos_runtime_static_inbox::STATIC_INBOX.with_borrow_mut(|inbox| {
                        let mut host = inbox.wrap_runtime(host);

                        #fn_name(&mut host)
                    });
                }
            }
            .into()
        }
    };

    code
}

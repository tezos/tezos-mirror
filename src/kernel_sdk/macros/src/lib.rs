// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Procedural macros designed to be used in conjunction with the rest of the sdk.
//! You should not depend on this crate directly, but instead use them as exported
//! from `tezos_smart_rollup`.

use proc_macro::TokenStream;
use proc_macro_error::{abort, proc_macro_error};
use quote::{quote, ToTokens};

/// Mark a function of type `fn(&mut impl Runtime)` as the entrypoint of the kernel.
#[proc_macro_error]
#[proc_macro_attribute]
pub fn main(attr: TokenStream, item: TokenStream) -> TokenStream {
    if !attr.is_empty() {
        let err = format!(
            "Expected no attributes in kernel_entrypoint macro invocation, got: {attr}"
        );
        let suggestion = "Try #[kernel_entrypoint]";
        abort! { err,
            format!("Unexpected attributes {}", attr);
            note = err;
            help = suggestion;
        }
    }

    let item = syn::parse_macro_input!(item as syn::ItemFn);
    let fn_name = item.sig.ident.to_token_stream();

    let kernel_fn_code: TokenStream = quote! {
        #item
        tezos_smart_rollup::kernel_entry!(#fn_name);
    }
    .into();

    kernel_fn_code
}

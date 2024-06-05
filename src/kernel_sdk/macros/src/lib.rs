// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Procedural macros designed to be used in conjunction with the rest of the sdk.
//! You should not depend on this crate directly, but instead use them as exported
//! from `tezos_smart_rollup`.

mod runtime;

use proc_macro::TokenStream;
use proc_macro_error::{abort, proc_macro_error};
use quote::{quote, ToTokens};
use runtime::RuntimeConfig;
use syn::{parse_macro_input, ItemFn};

/// Mark a function of type `fn(&mut impl Runtime)` as the entrypoint of the kernel.
///
/// ### Example
/// ```
/// use tezos_smart_rollup::prelude::*;
///
/// #[entrypoint::main]
/// pub fn f(host: &mut impl Runtime) {
///     // user kernel code
/// }
/// ```
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

/// Wrap the runtime of a function of type `fn(&mut impl Runtime)` according to the arguments given.
/// Use argument `static_inbox = "<PATH_TO_FILE>"` to specify static inbox file.
/// The path will be shell-expanded at compile time
///
/// ```
/// use tezos_smart_rollup::prelude::Runtime;
/// use tezos_smart_rollup::entrypoint;
///
/// #[entrypoint::main]
/// #[entrypoint::runtime(static_inbox = "./path/to/inbox.json")]
/// pub fn entry<R: Runtime>(host: &mut R) {
///     // do nothing
/// }
/// ```
#[proc_macro_attribute]
pub fn runtime(attr: TokenStream, item: TokenStream) -> TokenStream {
    let item_copy = item.clone();
    parse_macro_input!(attr as RuntimeConfig);
    parse_macro_input!(item_copy as ItemFn);
    item
}

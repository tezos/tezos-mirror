// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use proc_macro::TokenStream;

#[cfg(feature = "tracing")]
#[proc_macro_attribute]
pub fn trace_kernel(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut func = syn::parse_macro_input!(item as syn::ItemFn);
    let name = if attr.is_empty() {
        syn::LitStr::new(&func.sig.ident.to_string(), func.sig.ident.span())
    } else {
        syn::parse_macro_input!(attr as syn::LitStr)
    };

    let block = &func.block;
    func.block =
        syn::parse_quote!({ tezos_evm_logging::internal_trace_kernel(#name, || #block) });

    quote::quote!(#func).into()
}

#[cfg(not(feature = "tracing"))]
#[proc_macro_attribute]
pub fn trace_kernel(_attr: TokenStream, item: TokenStream) -> TokenStream {
    item
}

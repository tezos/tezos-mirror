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

    let has_host = func.sig.inputs.iter().any(|arg| {
        if let syn::FnArg::Typed(pat_type) = arg {
            if let syn::Pat::Ident(ident) = &*pat_type.pat {
                return ident.ident == "host";
            }
        }
        false
    });

    if !has_host {
        return syn::Error::new_spanned(
            &func.sig.ident,
            "the function annotated with #[trace_kernel] must have a `host` parameter",
        )
        .to_compile_error()
        .into();
    }

    let block = &func.block;
    func.block = syn::parse_quote!({ tezos_evm_logging::internal_trace_kernel(host, #name, |host| #block) });

    quote::quote!(#func).into()
}

#[cfg(not(feature = "tracing"))]
#[proc_macro_attribute]
pub fn trace_kernel(_attr: TokenStream, item: TokenStream) -> TokenStream {
    item
}

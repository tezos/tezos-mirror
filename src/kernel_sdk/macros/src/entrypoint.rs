// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use proc_macro::TokenStream;
use proc_macro_error2::abort;
use quote::{quote, ToTokens};

/// Implementation for [`super::main`].
pub(crate) fn main_attribute(attr: TokenStream, item: TokenStream) -> TokenStream {
    if !attr.is_empty() {
        let err = format!(
            "Expected no attributes in entrypoint::main macro invocation, got: {attr}"
        );
        let suggestion = "Try #[entrypoint::main]";
        abort! { err,
            format!("Unexpected attributes {}", attr);
            note = err;
            help = suggestion;
        }
    }

    let item = syn::parse_macro_input!(item as syn::ItemFn);
    let fn_name = item.sig.ident.to_token_stream();

    let wasm_code = quote! {
        /// The `kernel_run` function is called by the wasm host at regular intervals.
        #[cfg(target_arch = "wasm32")]
        #[no_mangle]
        pub extern "C" fn kernel_run() {
            tezos_smart_rollup::entrypoint::kernel_entrypoint_fn(#fn_name);
        }
    };

    let hermit_code = quote! {
        #[cfg(all(target_arch = "riscv64", target_os = "hermit"))]
        fn main() {
            tezos_smart_rollup::entrypoint::kernel_entrypoint_fn(#fn_name);
        }
    };

    // The native-kernel feature is here to ensure backwards compatibility
    // for downstream users which already define main functions. (also helps to pass cargo check)
    // (e.g. a wasm kernel defined in a lib, and the crate also has a binary with a main)
    #[cfg(feature = "native-kernel")]
    let native_code = quote! {
        // Note: keep this cfg mutual exclusive to the wasm and hermit ones to ensure backwards compatibility
        #[cfg(not(any(
            target_arch = "wasm32",
            all(target_arch = "riscv64", target_os = "hermit")
        )))]
        fn main() {
            tezos_smart_rollup::entrypoint::kernel_entrypoint_fn(#fn_name);
        }
    };
    #[cfg(not(feature = "native-kernel"))]
    let native_code = quote! {};

    let code = quote! {
        #item

        #wasm_code
        #hermit_code
        #native_code
    };

    code.into()
}

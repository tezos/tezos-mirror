// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Procedural macros designed to be used in conjunction with the rest of the sdk.
//! You should not depend on this crate directly, but instead use them as exported
//! from `tezos_smart_rollup`.

mod runtime;

use proc_macro::TokenStream;
use proc_macro_error2::{abort, proc_macro_error};
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

/// Wrap the runtime of a function of type `fn(&mut impl Runtime)` according to the arguments given.
/// Use argument `static_inbox = "<PATH_TO_FILE>"` to specify static inbox file.
/// The path will be shell-expanded at compile time
///
/// ### Example
/// ```
/// use tezos_smart_rollup::prelude::*;
///
/// #[entrypoint::main]
/// #[entrypoint::runtime(static_inbox = "../tests/inbox.json")]
/// pub fn entry(host: &mut impl Runtime) {
///     // do nothing
/// }
/// ```
#[proc_macro_attribute]
pub fn runtime(attr: TokenStream, item: TokenStream) -> TokenStream {
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

// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Procedural macros designed to be used in conjunction with the rest of the sdk.
//! You should not depend on this crate directly, but instead use them as exported
//! from `tezos_smart_rollup`.

#[cfg(feature = "runtime-options")]
mod runtime;

#[cfg(feature = "entrypoint")]
mod entrypoint;

use proc_macro::TokenStream;
use proc_macro_error2::proc_macro_error;

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
#[cfg(feature = "entrypoint")]
#[proc_macro_error]
#[proc_macro_attribute]
pub fn main(attr: TokenStream, item: TokenStream) -> TokenStream {
    entrypoint::main_attribute(attr, item)
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
#[cfg(feature = "runtime-options")]
#[proc_macro_attribute]
pub fn runtime(attr: TokenStream, item: TokenStream) -> TokenStream {
    runtime::runtime_attribute(attr, item)
}

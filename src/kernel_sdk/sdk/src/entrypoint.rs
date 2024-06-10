// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! # Entrypoint
//!
//! Macros and utilities for configuring kernel entrypoint functions.
//!
//! Consider `A`, `B`, `C` are entrypoint macros to be used
//! on top of a kernel entrypoint function.
//! Macros will be evaluated top-down in a function-like composable way.
//!
//! ### Example
//!
//! Consider the following scenario:
//! ```ignore
//! #[A(..)]
//! #[B(..)]
//! #[C(..)]
//! pub fn f() {}
//! ```
//!
//! in the first step will get expanded to:
//!
//! ```ignore
//! pub fn A_f() {
//!     #[B(..)]
//!     #[C(..)]
//!     pub fn f()
//!
//!     // #[A(..)] specific code which calls f()
//!     // Note, f will be altered by `B` and `C`
//! }
//! ```
//!
//! Note, `A_f()` is named only for presentation purposes.
//! In reality the original `f()` will be used
//!
//! After all entrypoint macros are expanded,
//! the logical equivalent will be of the form
//! ```ignore
//! A(B(C(f)))
//! ```
//! where A(...) signifies that A modifies the entrypoint/kernel code given as input.

pub use tezos_smart_rollup_macros::main;

#[doc(hidden)]
pub mod internal;

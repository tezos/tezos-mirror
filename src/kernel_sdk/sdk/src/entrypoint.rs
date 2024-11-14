// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! # Entrypoint
//!
//! Macros and utilities for configuring kernel entrypoint functions.
//!
//! Macros will be evaluated top-down in a function-like composable way.
//! For this reason, the top-most entrypoint macro should be the [`main`] macro.
//!
//! ### Example
//!
//! Consider the following scenario:
//! ```
//! use tezos_smart_rollup::prelude::*;
//!
//! #[entrypoint::main]
//! #[entrypoint::runtime(static_inbox = "../data/inbox.json")]
//! pub fn f(host: &mut impl Runtime) {
//!     // user kernel code
//! }
//! ```
//!
//! After all entrypoint macros are expanded,
//! the logical equivalent will be of the form
//!
//! ```ignore
//! entrypoint::main(entrypoint::runtime(f))
//! ```
//!
//! where `entrypoint::macro_name(...)` signifies that `macro_name`
//! modifies the entrypoint/kernel code given as input.

pub use tezos_smart_rollup_macros::{main, runtime};

#[doc(hidden)]
pub mod internal;

pub use tezos_smart_rollup_entrypoint::*;

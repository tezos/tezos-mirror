// SPDX-FileCopyrightText: 2023-2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! This crate defines common types and functionalities used by the RISC-V and
//! New Durable Storage API crates.

pub mod bytes;
pub mod move_semantics;
pub mod safe_pointer;
pub mod try_clone;

/// Return type for functions that will result in an OCaml exception being thrown on error.
pub type OcamlFallible<T> = Result<T, ocaml::Error>;

// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

mod array_utils;
pub mod bits;
mod cache_utils;
pub mod default;
pub mod devicetree;
pub mod instruction_context;
mod interpreter;
pub mod jit;
pub mod kernel_loader;
pub mod machine_state;
pub mod parser;
pub mod program;
pub mod pvm;
pub mod range_utils;
pub mod state_backend;
pub mod stepper;
pub mod storage;
pub mod traps;

#[cfg(feature = "ocaml-api")]
pub mod ocaml_api;

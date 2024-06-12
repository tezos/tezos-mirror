// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

pub mod posix;
pub mod pvm;

/// Outcome of handling an ECALL
#[derive(Debug)]
pub enum EcallOutcome {
    /// Handling the ECALL ended in a fatal error
    Fatal { message: String },

    /// ECALL was handled successfully
    Handled { continue_eval: bool },
}

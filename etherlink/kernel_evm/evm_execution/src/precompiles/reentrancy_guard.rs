// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Reentrancy guard prevents circular impure precompile calls, such as:
//!   * FA bridge -> Arbitrary contract -> XTZ bridge
//!   * FA bridge -> Arbitrary contract -> FA bridge
//!
//! Although such calls do not immediately introduce a vulnerability,
//! it's still a good practice that can prevent potential issues.
//!
//! NOTE that this does not prevent batched precompile calls.
//!
//! Read more: https://blog.openzeppelin.com/reentrancy-after-istanbul

use std::borrow::Cow;

use evm::ExitError;
use primitive_types::H160;

#[derive(Debug)]
pub struct ReentrancyGuard {
    /// List of impure precompiles
    stoplist: Vec<H160>,
    /// Flag is set when kernel encounters the first "impure" precompile
    /// in the call stack.
    enabled_at: Option<u32>,
    /// Current call depth (only precompile calls count)
    level: u32,
}

impl ReentrancyGuard {
    /// Create new reentrancy guard from a list of impure precompiles.
    pub fn new(stoplist: Vec<H160>) -> Self {
        Self {
            stoplist,
            enabled_at: None,
            level: 0,
        }
    }

    /// Try to begin a precompile call.
    ///
    /// If the address belongs to an impure precompile this method AND
    /// this is a circular call then this method will fail with an error.
    pub fn begin_precompile_call(&mut self, address: &H160) -> Result<(), ExitError> {
        if self.stoplist.contains(address) {
            if self.enabled_at.is_some() {
                return Err(ExitError::Other(Cow::from(
                    "Circular calls are not allowed",
                )));
            }
            self.enabled_at = Some(self.level);
        }
        self.level += 1;
        Ok(())
    }

    /// End precompile call.
    pub fn end_precompile_call(&mut self) {
        self.level -= 1;
        if self.enabled_at == Some(self.level) {
            self.enabled_at = None;
        }
    }

    #[cfg(test)]
    pub fn disable(&mut self) {
        self.stoplist.clear();
    }
}

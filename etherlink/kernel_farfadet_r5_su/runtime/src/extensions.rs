// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

// The kernel runtime requires both the standard Runtime and the
// new Extended one.

pub trait WithGas {
    fn add_execution_gas(&mut self, gas: u64);
    fn executed_gas(&self) -> u64;
}

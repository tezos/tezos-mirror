// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pub trait WithGas {
    fn add_execution_gas(&mut self, gas: u64);
    fn executed_gas(&self) -> u64;
}

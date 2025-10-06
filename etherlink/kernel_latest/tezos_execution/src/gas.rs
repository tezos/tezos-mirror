// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

// Gas management for Tezlink operations.
/// This module defines a structure to manage gas consumption for Tezlink operations.
/// It provides functionality to initialize gas limits, track current gas usage,
/// and calculate gas consumed by operations.
///
/// Every Gas Related Function or Constant should be defined here.
use mir::gas;
use tezos_data_encoding::types::Narith;

pub struct TezlinkOperationGas {
    milligas_limit: u32,
    pub current_gas: gas::Gas,
}

impl TezlinkOperationGas {
    fn start_internal(&mut self) {
        self.milligas_limit = self.current_gas.milligas();
    }

    /// Calculates the amount of milligas consumed by the current operation and resets the gas tracker.
    ///
    /// This method computes the difference between the initial gas limit and the remaining gas,
    /// representing the total milligas consumed during the operation. After calculating the
    /// consumed amount, it automatically restarts the gas tracking with the remaining gas as
    /// the new limit for subsequent operations.
    ///
    /// # Returns
    ///
    /// The number of milligas consumed by the operation as a `u64`.
    ///
    /// # Side Effects
    ///
    /// This method mutates the internal state by calling [`start_internal`](Self::start_internal), which:
    /// - Reinitializes the gas tracker with the new limit
    ///
    /// # Example
    ///
    /// ```ignore
    /// # use tezos_data_encoding::types::Narith;
    /// # use num_bigint::BigUint;
    /// let limit = Narith(BigUint::from(1000u32));
    /// let mut gas = TezlinkOperationGas::start(&limit).unwrap();
    ///
    /// // Simulate some gas consumption here
    /// // gas.current_gas.consume(...);
    ///
    /// let consumed = gas.milligas_consumed_by_operation();
    /// println!("Operation consumed {} milligas", consumed);
    /// ```
    pub fn milligas_consumed_by_operation(&mut self) -> Narith {
        let consumed = self.milligas_limit - self.current_gas.milligas();
        self.start_internal();
        Narith::from(consumed as u64)
    }

    pub fn _consume(&mut self, amount: u32) -> Result<(), gas::OutOfGas> {
        self.current_gas.consume(amount)
    }
}

impl Default for TezlinkOperationGas {
    /// Constructs [Gas] with [DEFAULT_GAS_AMOUNT] gas remaining.
    fn default() -> Self {
        let current_gas = gas::Gas::default();
        Self {
            milligas_limit: current_gas.milligas(),
            current_gas,
        }
    }
}

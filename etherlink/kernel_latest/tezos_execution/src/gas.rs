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
use thiserror::Error;

pub struct TezlinkOperationGas {
    milligas_limit: u32,
    pub current_gas: gas::Gas,
}

#[derive(Debug, Error)]
pub enum GasLimitError {
    #[error(
        "The gas limit provided is too high the limit is {0} gas units but {1} gas units were given"
    )]
    GasLimitTooHigh(u32, num_bigint::BigUint),
    #[error("Failed gas limit conversion: {0}")]
    CannotConvertToU32(num_bigint::TryFromBigIntError<num_bigint::BigUint>),
}

pub struct Cost(pub u32);
impl Cost {
    /// This corresponds to the defaults costs in gas.
    /// Currently can be found in Tezos source code at: src/proto_023_PtSeouLo/lib_protocol/michelson_v1_gas.ml
    const GAS_COST_MANAGER_OPERATION: u32 = 100_000;
    const GAS_COST_TRANSACTION: u32 = 2_000_000;

    pub fn manager_operation() -> Self {
        Cost(Self::GAS_COST_MANAGER_OPERATION)
    }

    pub fn transaction() -> Self {
        Cost(Self::GAS_COST_TRANSACTION)
    }
}

impl TezlinkOperationGas {
    /// This corresponds to the default value of the `hard_gas_limit_per_operation` parametric constant.
    /// Currently can be found in Tezos source code at: src/proto_023_PtSeouLo/lib_parameters/default_parameter.ml
    pub const MAX_GAS_UNIT_AMOUNT: u32 = 1_040_000;

    pub fn start(
        operation_gas_limit: &tezos_data_encoding::types::Narith,
    ) -> Result<Self, GasLimitError> {
        if operation_gas_limit.0 > num_bigint::BigUint::from(Self::MAX_GAS_UNIT_AMOUNT) {
            return Err(GasLimitError::GasLimitTooHigh(
                Self::MAX_GAS_UNIT_AMOUNT,
                operation_gas_limit.0.clone(),
            ));
        }

        let operation_miligas_limit = &operation_gas_limit.0 * 1000u32;
        // Should never fail because of the previous check
        let milligas_limit = operation_miligas_limit
            .try_into()
            .map_err(GasLimitError::CannotConvertToU32)?;
        Ok(Self {
            milligas_limit,
            current_gas: gas::Gas::new(milligas_limit),
        })
    }

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

    pub fn consume(&mut self, cost: Cost) -> Result<(), gas::OutOfGas> {
        self.current_gas.consume(cost.0)
    }
}

#[cfg(test)]
impl Default for TezlinkOperationGas {
    /// Constructs [Gas] with [MAX_GAS_UNIT_AMOUNT] gas remaining.
    fn default() -> Self {
        Self {
            milligas_limit: Self::MAX_GAS_UNIT_AMOUNT * 1000,
            current_gas: gas::Gas::new(Self::MAX_GAS_UNIT_AMOUNT * 1000),
        }
    }
}

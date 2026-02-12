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
use tezos_crypto_rs::CryptoError;
use tezos_data_encoding::types::Narith;
use tezos_smart_rollup::types::PublicKey;
use thiserror::Error;

/// Container used to track the gas left during the execution of an operation (including internal operations)
pub struct TezlinkOperationGas {
    /// Maximum gas the current operation could consume, in milligas
    limit: u32,
    /// Gas remaining for the current operation, in milligas
    pub remaining: gas::Gas,
}

/// Every error should express the gas in unit (1 gas unit = 1000 milligas).
#[derive(Debug, Error)]
pub enum GasLimitError {
    #[error(
        "The gas limit provided is too high the limit is {0} gas units but {1} gas units were given"
    )]
    GasLimitTooHigh(u32, num_bigint::BigUint),
    #[error("Failed gas limit conversion: {0}")]
    CannotConvertToU32(num_bigint::TryFromBigIntError<num_bigint::BigUint>),
}

pub struct Cost(u32);
impl Cost {
    /// This corresponds to the defaults costs in gas.
    /// Currently can be found in Tezos source code at: src/proto_023_PtSeouLo/lib_protocol/michelson_v1_gas.ml
    const GAS_COST_MANAGER_OPERATION: u32 = 100_000;
    const GAS_COST_TRANSACTION: u32 = 2_000_000;

    /// Cost for a manager operation in gas units.
    pub fn manager_operation() -> Self {
        Cost(Self::GAS_COST_MANAGER_OPERATION)
    }

    /// Cost for a transaction in gas units.
    pub fn transaction() -> Self {
        Cost(Self::GAS_COST_TRANSACTION)
    }

    /// Returns the gas cost of checking a signature for the given payload.
    pub fn check_signature(k: &PublicKey, msg: &[u8]) -> Result<Self, CryptoError> {
        mir::gas::interpret_cost::check_signature(k, msg).map(Cost)
    }
}

impl TezlinkOperationGas {
    /// This corresponds to the default value of the `hard_gas_limit_per_operation` parametric constant.
    /// Currently can be found in Tezos source code at: src/proto_023_PtSeouLo/lib_parameters/default_parameter.ml
    pub const MAX_LIMIT: u32 = 1_040_000_000;

    /// Initializes operation gas from the provided limit and validates it
    /// against the per-operation and remaining block limits.
    pub fn start(
        limit_in_gas_unit: &tezos_data_encoding::types::Narith,
    ) -> Result<Self, GasLimitError> {
        let limit_in_milligas = &limit_in_gas_unit.0 * 1000u32;
        if limit_in_milligas > num_bigint::BigUint::from(Self::MAX_LIMIT) {
            return Err(GasLimitError::GasLimitTooHigh(
                Self::MAX_LIMIT / 1000,
                limit_in_milligas / 1000u32,
            ));
        }

        // Should never fail because of the previous check
        let limit = limit_in_milligas
            .try_into()
            .map_err(GasLimitError::CannotConvertToU32)?;

        Ok(Self {
            limit,
            remaining: gas::Gas::new(limit),
        })
    }

    /// Resets the internal limit to the current remaining milligas.
    fn start_internal(&mut self) {
        self.limit = self.remaining.milligas();
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
    /// The number of milligas consumed by the operation as a [`Narith`].
    ///
    /// # Side Effects
    ///
    /// This method mutates the internal state by calling [`start_internal`](Self::start_internal), which:
    /// - Reinitializes the gas tracker with the new limit
    ///
    /// # Example
    ///
    /// ```ignore
    /// # use num_bigint::BigUint;
    /// # use tezos_data_encoding::types::Narith;
    /// # use tezos_execution_latest::gas::TezlinkOperationGas;
    /// let limit = Narith(BigUint::from(1000u32));
    /// let mut gas = TezlinkOperationGas::start(&limit).unwrap();
    ///
    /// // Simulate some gas consumption here
    /// // gas.remaining.consume(...);
    ///
    /// let consumed = gas.milligas_consumed_by_operation();
    /// println!("Operation consumed {:?} milligas", consumed);
    /// ```
    pub fn milligas_consumed_by_operation(&mut self) -> Narith {
        let consumed = self.limit - self.remaining.milligas();
        self.start_internal();
        Narith::from(consumed as u64)
    }

    /// Consumes the provided cost from the operation gas tracker.
    pub fn consume(&mut self, cost: Cost) -> Result<(), gas::OutOfGas> {
        self.remaining.consume(cost.0)
    }
}

#[cfg(test)]
impl Default for TezlinkOperationGas {
    /// Constructs [Gas] with [MAX_LIMIT] gas remaining.
    fn default() -> Self {
        Self {
            limit: Self::MAX_LIMIT,
            remaining: gas::Gas::new(Self::MAX_LIMIT),
        }
    }
}

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

/// Tracks gas consumption during the execution of a single manager
/// operation, including all its internal sub-operations.
pub struct TezlinkOperationGas {
    /// The gas budget set at construction, in milligas. **Never modified.**
    /// Used by [`total_milligas_consumed`](Self::total_milligas_consumed) to
    /// compute the total gas consumed since the start of the operation.
    initial_limit: u32,
    /// A movable baseline, in milligas, used by
    /// [`get_and_reset_milligas_consumed`](Self::get_and_reset_milligas_consumed)
    /// to measure gas consumed by individual sub-steps (e.g. a single internal
    /// transfer). Reset to `remaining` after each measurement.
    current_limit: u32,
    /// The actual gas left for the current operation, only decreased by
    /// [`consume`](Self::consume). Never artificially reset.
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
    /// Maximum gas (in milligas) for a single Tezos operation in Tezos X.
    ///
    /// Raised above the L1 mainnet default (1_040_000 gas = 1_040_000_000
    /// milligas) so that a cross-runtime call originating on the EVM side,
    /// which can carry up to 30_000_000 EVM gas of remaining budget, can
    /// propagate its full gas limit through `X-Tezos-Gas-Limit` (1 EVM gas
    /// = 100 milligas, so 30_000_000 * 100 = 3_000_000_000 milligas).
    /// This value is overridden in the Tezlink parametric constants
    /// (`etherlink/bin_node/lib_dev/tezlink/tezlink_constants.ml`).
    pub const MAX_LIMIT: u32 = 3_000_000_000;

    /// Initializes operation gas from the provided limit (in gas units) and
    /// validates it against the per-operation hard limit.
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
            initial_limit: limit,
            current_limit: limit,
            remaining: gas::Gas::new(limit),
        })
    }

    /// Initializes operation gas from a limit already expressed in milligas.
    ///
    /// This is used by the cross-runtime protocol where `X-Tezos-Gas-Limit`
    /// carries milligas directly, avoiding a redundant milligas↔gas round-trip.
    pub fn start_milligas(limit_in_milligas: u64) -> Result<Self, GasLimitError> {
        let limit: u32 = limit_in_milligas.try_into().map_err(|_| {
            GasLimitError::GasLimitTooHigh(
                Self::MAX_LIMIT / 1000,
                num_bigint::BigUint::from(limit_in_milligas / 1000),
            )
        })?;
        if limit > Self::MAX_LIMIT {
            return Err(GasLimitError::GasLimitTooHigh(
                Self::MAX_LIMIT / 1000,
                num_bigint::BigUint::from(limit / 1000),
            ));
        }
        Ok(Self {
            initial_limit: limit,
            current_limit: limit,
            remaining: gas::Gas::new(limit),
        })
    }

    /// Returns the milligas consumed since the last reset and resets the
    /// baseline for the next measurement.
    ///
    /// The consumed amount is `current_limit - remaining`. After reading it,
    /// `current_limit` is reset to `remaining` so that the next call only
    /// measures gas spent by the following sub-operation.
    ///
    /// Returns [`OutOfGas`](mir::gas::OutOfGas) if gas has been exhausted
    /// (`current_limit` is still reset to 0 before returning the error).
    ///
    /// See [`total_milligas_consumed`](Self::total_milligas_consumed) for the
    /// cumulative total across all resets.
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
    /// // gas.consume(...);
    ///
    /// match gas.get_and_reset_milligas_consumed() {
    ///     Ok(consumed) => println!("Operation consumed {:?} milligas", consumed),
    ///     Err(_) => println!("Gas exhausted"),
    /// }
    /// ```
    pub fn get_and_reset_milligas_consumed(&mut self) -> Result<Narith, gas::OutOfGas> {
        match self.remaining.milligas() {
            Some(remaining) => {
                let consumed = self.current_limit - remaining;
                self.current_limit = remaining;
                Ok(Narith::from(consumed as u64))
            }
            None => {
                self.current_limit = 0;
                Err(gas::OutOfGas)
            }
        }
    }

    /// Returns the total milligas consumed since this tracker was created.
    ///
    /// Unlike [`get_and_reset_milligas_consumed`](Self::get_and_reset_milligas_consumed), this is immune
    /// to baseline resets performed by
    /// [`get_and_reset_milligas_consumed`](Self::get_and_reset_milligas_consumed).
    ///
    /// If gas has been exhausted, returns `initial_limit` (all gas was consumed).
    ///
    /// Cannot overflow: both `initial_limit` and `remaining` are `u32`,
    /// and `remaining <= initial_limit` is maintained by [`consume`](Self::consume).
    pub fn total_milligas_consumed(&self) -> u32 {
        match self.remaining.milligas() {
            Some(remaining) => self.initial_limit - remaining,
            None => self.initial_limit,
        }
    }

    /// Consumes the provided cost from the operation gas tracker.
    pub fn consume(&mut self, cost: Cost) -> Result<(), gas::OutOfGas> {
        self.remaining.consume(cost.0)
    }

    /// Consumes a raw milligas amount from the operation gas tracker.
    /// L2-1044: Avoid unnecessary cast by using a better suited type for gas in MIR
    pub fn cast_and_consume_milligas(
        &mut self,
        milligas: u64,
    ) -> Result<(), gas::OutOfGas> {
        let milligas: u32 = milligas.try_into().map_err(|_| gas::OutOfGas)?;
        self.remaining.consume(milligas)
    }
}

#[cfg(test)]
impl Default for TezlinkOperationGas {
    /// Constructs [Gas] with [MAX_LIMIT] gas remaining.
    fn default() -> Self {
        Self {
            initial_limit: Self::MAX_LIMIT,
            current_limit: Self::MAX_LIMIT,
            remaining: gas::Gas::new(Self::MAX_LIMIT),
        }
    }
}

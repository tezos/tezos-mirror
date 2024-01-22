// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

//! DAL parameters returned from the reveal DAL parameters host function.

use core::num::TryFromIntError;

/// Size of raw DAL parameters type.
/// 4 * size_of(u64) because RollupDalParameters has 4 fields of u64.
pub const DAL_PARAMETERS_SIZE: usize = 4 * core::mem::size_of::<u64>();

/// Type returned from [`reveal_dal_parameters`].
///
/// [`reveal_dal_parameters`]: tezos_smart_rollup_host::runtime::Runtime::reveal_dal_parameters.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RollupDalParameters {
    /// number of slots
    pub number_of_slots: u64,
    /// attestation lag
    pub attestation_lag: u64,
    /// slot size
    pub slot_size: u64,
    /// page size
    pub page_size: u64,
}

impl TryFrom<[u8; DAL_PARAMETERS_SIZE]> for RollupDalParameters {
    type Error = TryFromIntError;

    fn try_from(value: [u8; DAL_PARAMETERS_SIZE]) -> Result<Self, Self::Error> {
        let (number_of_slots, value) = value.split_at(core::mem::size_of::<i64>());
        let (attestation_lag, value) = value.split_at(core::mem::size_of::<i64>());
        let (slot_size, value) = value.split_at(core::mem::size_of::<i64>());
        let (page_size, _value) = value.split_at(core::mem::size_of::<i64>());

        let number_of_slots = i64::from_be_bytes(number_of_slots.try_into().unwrap());
        let attestation_lag = i64::from_be_bytes(attestation_lag.try_into().unwrap());
        let slot_size = i64::from_be_bytes(slot_size.try_into().unwrap());
        let page_size = i64::from_be_bytes(page_size.try_into().unwrap());

        let number_of_slots = u64::try_from(number_of_slots)?;
        let attestation_lag = u64::try_from(attestation_lag)?;
        let slot_size = u64::try_from(slot_size)?;
        let page_size = u64::try_from(page_size)?;

        Ok(RollupDalParameters {
            number_of_slots,
            attestation_lag,
            slot_size,
            page_size,
        })
    }
}

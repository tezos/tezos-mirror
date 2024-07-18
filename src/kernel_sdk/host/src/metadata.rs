// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

//! Rollup Metadata: `sr1..` address & origination level.
#[cfg(feature = "crypto")]
use tezos_crypto_rs::hash::SmartRollupHash;

/// Size of raw rollup address - [`SmartRollupHash`].
pub const RAW_ROLLUP_ADDRESS_SIZE: usize = 20;

/// Size of raw metadata: 20 (rollup address) + 4 (origination level).
pub const METADATA_SIZE: usize = RAW_ROLLUP_ADDRESS_SIZE + core::mem::size_of::<i32>();

/// Type returned from [`reveal_metadata`].
///
/// The raw structure is 24 bytes (20-byte rollup address followed
/// by 4-byte origination level (i32 big-endian) )
///
/// [`reveal_metadata`]: tezos_smart_rollup_host::runtime::Runtime::reveal_metadata.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RollupMetadata {
    /// Rollup address
    pub raw_rollup_address: [u8; RAW_ROLLUP_ADDRESS_SIZE],
    /// Origination level
    pub origination_level: u32,
}

impl RollupMetadata {
    /// The address of the smart rollup.
    #[cfg(feature = "crypto")]
    pub fn address(&self) -> SmartRollupHash {
        SmartRollupHash(self.raw_rollup_address.to_vec())
    }
}

impl From<[u8; METADATA_SIZE]> for RollupMetadata {
    fn from(value: [u8; METADATA_SIZE]) -> Self {
        let (address, origination_level) = value.split_at(20);
        let rollup_address: [u8; RAW_ROLLUP_ADDRESS_SIZE] = address.try_into().unwrap();
        let origination_level: [u8; core::mem::size_of::<i32>()] =
            origination_level.try_into().unwrap();
        // Origination level is guaranteed to be positive
        let origination_level = i32::from_be_bytes(origination_level) as u32;
        RollupMetadata {
            raw_rollup_address: rollup_address,
            origination_level,
        }
    }
}

impl From<RollupMetadata> for [u8; METADATA_SIZE] {
    fn from(value: RollupMetadata) -> [u8; METADATA_SIZE] {
        let mut data = [0; METADATA_SIZE];
        data[..20].copy_from_slice(&value.raw_rollup_address);
        data[20..METADATA_SIZE]
            .copy_from_slice(&i32::to_be_bytes(value.origination_level as i32));
        data
    }
}

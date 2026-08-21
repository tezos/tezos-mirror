// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Reads and writes, at a key space key, the two 256-bit integers the kernel
//! uses: the `primitive_types` one the storage speaks, and the `revm` one the
//! EVM demands.
//!
//! Either one is the four `u64`s it is made of, stored least or most
//! significant first depending on the accessor. This crate is the deepest one
//! the storage and the EVM sides both depend on, which is why the accessors
//! live here rather than next to either of them.

use primitive_types::U256;
use revm::primitives::U256 as EvmU256;
use tezos_smart_rollup_keyspace::{Key, KeySpace, KeySpaceWriteError};

/// Reads and writes 256-bit integers at a key.
///
/// Implemented for every [`KeySpace`], so bringing the trait into scope is
/// enough to call these on any key space handle.
pub trait KeySpaceExtU256: KeySpace {
    /// Returns the little-endian integer at `key`, or `None` when the key is
    /// absent or holds fewer than 32 bytes.
    fn get_u256_le(&self, key: &Key) -> Option<U256> {
        get_le_array(self, key).map(U256)
    }

    /// [`Self::get_u256_le`], falling back to `default`.
    fn get_u256_le_or(&self, key: &Key, default: U256) -> U256 {
        self.get_u256_le(key).unwrap_or(default)
    }

    /// [`Self::get_u256_le`] for a value stored most significant first.
    fn get_u256_be(&self, key: &Key) -> Option<U256> {
        get_be_array(self, key).map(U256)
    }

    /// [`Self::get_u256_be`], falling back to `default`.
    fn get_u256_be_or(&self, key: &Key, default: U256) -> U256 {
        self.get_u256_be(key).unwrap_or(default)
    }

    /// Writes `value` least significant first.
    fn store_u256_le(
        &mut self,
        key: &Key,
        value: U256,
    ) -> Result<(), KeySpaceWriteError> {
        store_le_array(self, key, value.0)
    }

    /// Writes `value` most significant first.
    fn store_u256_be(
        &mut self,
        key: &Key,
        value: U256,
    ) -> Result<(), KeySpaceWriteError> {
        store_be_array(self, key, value.0)
    }

    /// [`Self::get_u256_le`] for the EVM's own integer.
    fn get_evm_u256_le(&self, key: &Key) -> Option<EvmU256> {
        get_le_array(self, key).map(EvmU256::from_limbs)
    }

    /// [`Self::get_evm_u256_le`], falling back to `default`.
    fn get_evm_u256_le_or(&self, key: &Key, default: EvmU256) -> EvmU256 {
        self.get_evm_u256_le(key).unwrap_or(default)
    }

    /// [`Self::get_u256_be`] for the EVM's own integer, the layout its storage
    /// slots are stored in.
    fn get_evm_u256_be(&self, key: &Key) -> Option<EvmU256> {
        get_be_array(self, key).map(EvmU256::from_limbs)
    }

    /// [`Self::get_evm_u256_be`], falling back to `default`.
    fn get_evm_u256_be_or(&self, key: &Key, default: EvmU256) -> EvmU256 {
        self.get_evm_u256_be(key).unwrap_or(default)
    }

    /// [`Self::store_u256_le`] for the EVM's own integer.
    fn store_evm_u256_le(
        &mut self,
        key: &Key,
        value: EvmU256,
    ) -> Result<(), KeySpaceWriteError> {
        store_le_array(self, key, value.into_limbs())
    }

    /// [`Self::store_u256_be`] for the EVM's own integer.
    fn store_evm_u256_be(
        &mut self,
        key: &Key,
        value: EvmU256,
    ) -> Result<(), KeySpaceWriteError> {
        store_be_array(self, key, value.into_limbs())
    }
}

impl<KS: KeySpace> KeySpaceExtU256 for KS {}

/// Returns the `N` `u64`s of the value at `key`, least significant first, or
/// `None` when the key is absent or holds fewer than `N * 8` bytes.
fn get_le_array<KS: KeySpace + ?Sized, const N: usize>(
    ks: &KS,
    key: &Key,
) -> Option<[u64; N]> {
    u64_chunks(ks, key).map(|chunks| chunks.map(u64::from_le_bytes))
}

/// [`get_le_array`] for a value whose most significant `u64` comes first. The
/// array is still least significant first.
fn get_be_array<KS: KeySpace + ?Sized, const N: usize>(
    ks: &KS,
    key: &Key,
) -> Option<[u64; N]> {
    u64_chunks(ks, key).map(|mut chunks| {
        chunks.reverse();
        chunks.map(u64::from_be_bytes)
    })
}

/// Writes `values` least significant first, the layout [`get_le_array`] reads.
fn store_le_array<KS: KeySpace + ?Sized, const N: usize>(
    ks: &mut KS,
    key: &Key,
    values: [u64; N],
) -> Result<(), KeySpaceWriteError> {
    let chunks = values.map(u64::to_le_bytes);
    ks.set(key, chunks.as_flattened())
}

/// Writes `values` most significant first, the layout [`get_be_array`] reads.
fn store_be_array<KS: KeySpace + ?Sized, const N: usize>(
    ks: &mut KS,
    key: &Key,
    values: [u64; N],
) -> Result<(), KeySpaceWriteError> {
    let mut chunks = values.map(u64::to_be_bytes);
    chunks.reverse();
    ks.set(key, chunks.as_flattened())
}

/// Returns the `N` eight-byte chunks the value at `key` is made of, or `None`
/// when the key is absent or holds fewer than `N * 8` bytes.
///
/// The chunking is what gives the read a buffer of `N * 8` bytes at all: no
/// stable Rust says that length, so [`KeySpace::read_exact_in`] cannot be
/// handed such an array.
fn u64_chunks<KS: KeySpace + ?Sized, const N: usize>(
    ks: &KS,
    key: &Key,
) -> Option<[[u8; 8]; N]> {
    let mut chunks = [[0u8; 8]; N];
    let buffer = chunks.as_flattened_mut();
    (ks.read(key, 0, buffer)? == buffer.len()).then_some(chunks)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_keyspace::KeySpaceLoader;

    /// The four integers the value under test is made of.
    const LIMBS: [u64; 4] = [1, 2, 3, 4];

    /// A value one below a limb boundary, with a bit set in the most
    /// significant limb: adding one to it carries into the second limb.
    const HUGE: [u64; 4] = [u64::MAX, 0, 0, 1];

    /// What adding one to [`HUGE`] gives.
    const HUGE_PLUS_ONE: [u64; 4] = [0, 1, 0, 1];

    fn key(bytes: &[u8]) -> Key {
        Key::from_bytes(bytes).unwrap()
    }

    #[test]
    fn a_u256_round_trips_in_either_layout() {
        let mut host = MockKernelHost::default();
        let mut ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
        let k = key(b"/n");
        let value = U256(LIMBS);
        let mut expected = [0u8; 32];

        ks.store_u256_le(&k, value).unwrap();
        assert_eq!(ks.get_u256_le(&k), Some(value));
        // The bytes the type converts itself to, so the accessors and the
        // type's own conversions stay interchangeable.
        value.to_little_endian(&mut expected);
        assert_eq!(ks.get(&k), Some(expected.to_vec()));

        ks.store_u256_be(&k, value).unwrap();
        assert_eq!(ks.get_u256_be(&k), Some(value));
        value.to_big_endian(&mut expected);
        assert_eq!(ks.get(&k), Some(expected.to_vec()));

        // Read the other way round, the same bytes are another value.
        assert_ne!(ks.get_u256_le(&k), Some(value));
    }

    #[test]
    fn an_evm_u256_round_trips_in_either_layout() {
        let mut host = MockKernelHost::default();
        let mut ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
        let k = key(b"/n");
        let value = EvmU256::from_limbs(LIMBS);

        ks.store_evm_u256_le(&k, value).unwrap();
        assert_eq!(ks.get_evm_u256_le(&k), Some(value));
        assert_eq!(ks.get(&k), Some(value.to_le_bytes::<32>().to_vec()));

        ks.store_evm_u256_be(&k, value).unwrap();
        assert_eq!(ks.get_evm_u256_be(&k), Some(value));
        assert_eq!(ks.get(&k), Some(value.to_be_bytes::<32>().to_vec()));

        assert_ne!(ks.get_evm_u256_le(&k), Some(value));
    }

    #[test]
    fn both_types_read_the_bytes_the_other_one_wrote() {
        let mut host = MockKernelHost::default();
        let mut ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
        let k = key(b"/n");

        ks.store_u256_le(&k, U256(LIMBS)).unwrap();
        assert_eq!(ks.get_evm_u256_le(&k), Some(EvmU256::from_limbs(LIMBS)));

        ks.store_evm_u256_be(&k, EvmU256::from_limbs(LIMBS))
            .unwrap();
        assert_eq!(ks.get_u256_be(&k), Some(U256(LIMBS)));
    }

    #[test]
    fn short_of_32_bytes_is_missing_and_takes_the_default() {
        let mut host = MockKernelHost::default();
        let mut ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
        let absent = key(b"/absent");
        let default = U256(LIMBS);
        let evm_default = EvmU256::from_limbs(LIMBS);

        assert_eq!(ks.get_u256_le(&absent), None);
        assert_eq!(ks.get_u256_be(&absent), None);
        assert_eq!(ks.get_evm_u256_le(&absent), None);
        assert_eq!(ks.get_evm_u256_be(&absent), None);
        assert_eq!(ks.get_u256_le_or(&absent, default), default);
        assert_eq!(ks.get_u256_be_or(&absent, default), default);
        assert_eq!(ks.get_evm_u256_le_or(&absent, evm_default), evm_default);
        assert_eq!(ks.get_evm_u256_be_or(&absent, evm_default), evm_default);

        // A value one byte short is missing rather than zero-extended.
        let short = key(b"/short");
        ks.set(&short, [1u8; 31]).unwrap();
        assert_eq!(ks.get_u256_le(&short), None);
        assert_eq!(ks.get_u256_le_or(&short, default), default);
    }

    #[test]
    fn a_large_value_still_adds_up_after_a_round_trip() {
        let mut host = MockKernelHost::default();
        let mut ks = host.load_or_create("/ks".parse().unwrap()).unwrap();
        let k = key(b"/n");

        // Arithmetic, not equality, is what a limb the accessors put back in
        // the wrong place would fail: the carry would land elsewhere.
        let value = U256(HUGE);
        let carried = U256(HUGE_PLUS_ONE);
        ks.store_u256_le(&k, value).unwrap();
        assert_eq!(ks.get_u256_le(&k).unwrap() + U256::one(), carried);
        ks.store_u256_be(&k, value).unwrap();
        assert_eq!(ks.get_u256_be(&k).unwrap() + U256::one(), carried);

        let evm_value = EvmU256::from_limbs(HUGE);
        let evm_carried = EvmU256::from_limbs(HUGE_PLUS_ONE);
        ks.store_evm_u256_le(&k, evm_value).unwrap();
        assert_eq!(ks.get_evm_u256_le(&k).unwrap() + EvmU256::ONE, evm_carried);
        ks.store_evm_u256_be(&k, evm_value).unwrap();
        assert_eq!(ks.get_evm_u256_be(&k).unwrap() + EvmU256::ONE, evm_carried);

        // And the two libraries read the same bytes as the same number.
        ks.store_u256_le(&k, value).unwrap();
        assert_eq!(
            ks.get_evm_u256_le(&k).unwrap().to_string(),
            value.to_string()
        );
    }
}

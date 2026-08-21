// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Extension traits over [`KeySpace`], each implemented for every key space.
//!
//! A key space stores bytes; these traits read and write typed values at a
//! key. Bringing one into scope makes its accessors available on any key
//! space handle. The ones that need an encoding crate sit behind a feature.

use crate::{Key, KeySpace, KeySpaceWriteError};
use num_traits::{FromBytes, ToBytes};

/// Typed integer reads and writes over a [`KeySpace`].
pub trait KeySpaceExtNum: KeySpace {
    /// Returns the little-endian integer at `key`, or `None` when the key is
    /// absent or holds fewer bytes than the integer takes.
    ///
    /// `N` is pinned by [`FromBytes::Bytes`], so a caller names the integer
    /// type and never a byte count. A type whose `Bytes` is not an array does
    /// not compile here, which keeps out the ones that panic on a length
    /// mismatch.
    fn get_le<const N: usize, T: FromBytes<Bytes = [u8; N]>>(
        &self,
        key: &Key,
    ) -> Option<T> {
        self.get_prefix_exact::<N>(key)
            .map(|bytes| T::from_le_bytes(&bytes))
    }

    /// The big-endian counterpart of [`Self::get_le`].
    fn get_be<const N: usize, T: FromBytes<Bytes = [u8; N]>>(
        &self,
        key: &Key,
    ) -> Option<T> {
        self.get_prefix_exact::<N>(key)
            .map(|bytes| T::from_be_bytes(&bytes))
    }

    /// [`Self::get_le`], falling back to `default` when the key is absent.
    fn get_le_or<const N: usize, T: FromBytes<Bytes = [u8; N]>>(
        &self,
        key: &Key,
        default: T,
    ) -> T {
        self.get_le(key).unwrap_or(default)
    }

    /// [`Self::get_be`], falling back to `default` when the key is absent.
    fn get_be_or<const N: usize, T: FromBytes<Bytes = [u8; N]>>(
        &self,
        key: &Key,
        default: T,
    ) -> T {
        self.get_be(key).unwrap_or(default)
    }

    /// Writes `value` little-endian at `key`.
    fn store_le<T: ToBytes>(
        &mut self,
        key: &Key,
        value: T,
    ) -> Result<(), KeySpaceWriteError> {
        self.set(key, value.to_le_bytes())
    }

    /// The big-endian counterpart of [`Self::store_le`].
    fn store_be<T: ToBytes>(
        &mut self,
        key: &Key,
        value: T,
    ) -> Result<(), KeySpaceWriteError> {
        self.set(key, value.to_be_bytes())
    }
}

impl<KS: KeySpace> KeySpaceExtNum for KS {}

/// Rlp reads and writes over a [`KeySpace`].
#[cfg(feature = "rlp")]
pub trait KeySpaceExtRlp: KeySpace {
    /// Writes the rlp encoding of `value` at `key`.
    fn store_rlp<T: rlp::Encodable>(
        &mut self,
        key: &Key,
        value: &T,
    ) -> Result<(), KeySpaceWriteError> {
        self.set(key, value.rlp_bytes())
    }

    /// Returns the rlp value at `key`, or `None` when the key is absent.
    /// Bytes that do not decode into `T` are a [`rlp::DecoderError`].
    fn read_rlp<T: rlp::Decodable>(
        &self,
        key: &Key,
    ) -> Result<Option<T>, rlp::DecoderError> {
        match self.get(key) {
            Some(bytes) => rlp::decode(&bytes).map(Some),
            None => Ok(None),
        }
    }

    /// [`Self::read_rlp`], falling back to `default` when the key is absent.
    /// Only absence takes the fallback, bytes that do not decode stay an
    /// error.
    fn read_rlp_or<T: rlp::Decodable>(
        &self,
        key: &Key,
        default: T,
    ) -> Result<T, rlp::DecoderError> {
        Ok(self.read_rlp(key)?.unwrap_or(default))
    }
}

#[cfg(feature = "rlp")]
impl<KS: KeySpace> KeySpaceExtRlp for KS {}

#[cfg(all(test, feature = "irmin-compat"))]
mod tests {
    use super::*;
    use crate::irmin_ds::{IrminKeySpaceRegistry, StorageV1KeySpaceCompat};
    use tezos_smart_rollup_mock::{InMemoryStore, MockHost};

    fn key(s: &[u8]) -> Key {
        Key::from_bytes(s).unwrap()
    }

    /// Load `name` over `host`, the way the crate root's tests do.
    fn load(
        registry: &mut IrminKeySpaceRegistry,
        host: &mut MockHost,
        name: &str,
    ) -> StorageV1KeySpaceCompat<InMemoryStore> {
        // SAFETY: this registry is the only one minting key spaces over `host`,
        // and it rejects overlapping names.
        unsafe { registry.load_or_create(host, name.parse().unwrap()) }
            .expect("name does not overlap a loaded one")
    }

    #[test]
    fn integer_accessors_round_trip_both_endiannesses() {
        let mut host = MockHost::default();
        let mut registry = IrminKeySpaceRegistry::new();
        let mut ks = load(&mut registry, &mut host, "/ints");
        let k = key(b"/n");

        ks.store_le(&k, 0x0102_0304u32).unwrap();
        assert_eq!(ks.get_le(&k), Some(0x0102_0304u32));
        // Read the other way round, the same bytes give the swapped value.
        assert_eq!(ks.get_be(&k), Some(0x0403_0201u32));

        ks.store_be(&k, 0x0102_0304u32).unwrap();
        assert_eq!(ks.get_be(&k), Some(0x0102_0304u32));

        // An absent key falls back to the default.
        let missing = key(b"/missing");
        assert_eq!(ks.get_le_or(&missing, 7u64), 7);
        assert_eq!(ks.get_be_or(&missing, 7u16), 7);
        assert_eq!(ks.get_le::<8, u64>(&missing), None);
    }

    #[cfg(feature = "rlp")]
    #[test]
    fn rlp_accessors_round_trip_and_report_an_absent_key() {
        let mut host = MockHost::default();
        let mut registry = IrminKeySpaceRegistry::new();
        let mut ks = load(&mut registry, &mut host, "/rlp");
        let k = key(b"/value");

        ks.store_rlp(&k, &vec![1u8, 2, 3]).unwrap();
        assert_eq!(ks.read_rlp::<Vec<u8>>(&k).unwrap(), Some(vec![1, 2, 3]));

        // The bytes written are the rlp encoding itself, so a reader of the
        // raw value sees them unchanged.
        assert_eq!(ks.get(&k), Some(rlp::encode(&vec![1u8, 2, 3]).to_vec()));

        // An absent key is `None`, or the default where the caller asked for
        // one, while bytes that do not decode are an error either way.
        let missing = key(b"/missing");
        assert_eq!(ks.read_rlp::<Vec<u8>>(&missing).unwrap(), None);
        assert_eq!(ks.read_rlp_or(&missing, vec![9u8]).unwrap(), vec![9]);
        ks.set(&k, [0xc0, 0xff]).unwrap();
        assert!(ks.read_rlp::<Vec<u8>>(&k).is_err());
        assert!(ks.read_rlp_or(&k, vec![9u8]).is_err());
    }
}

// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Extension traits over [`KeySpace`] that this crate adds to the SDK ones.
//! This crate implements each trait for every keyspace. The traits report
//! their own failures as a [`StorageReadErrorKind`].

use tezos_crypto_rs::base58::FromBase58CheckError;
use tezos_crypto_rs::hash::{ContractKt1Hash, HashTrait};
use tezos_crypto_rs::public_key::PublicKey;
use tezos_data_encoding::nom::NomReader;
use tezos_smart_rollup_keyspace::{Key, KeySpace, KeySpaceError, Name, ReadKind};

use crate::error::StorageReadErrorKind;
use crate::KT1_B58_SIZE;

/// Binary reads over a [`KeySpace`] that read at most a given number of
/// bytes.
///
/// Unlike [`KeySpaceExtBin::read_nom`], which reads the whole value, a read
/// stops at `max_bytes`. `max_bytes` must be an upper bound on the encoded
/// size of `T` at this key. A longer value fails with
/// [`StorageReadErrorKind::ValueExceedsBound`] when its first `max_bytes` bytes do
/// not decode as `T`. Otherwise, it reads as the `T` that these bytes encode.
///
/// [`KeySpaceExtBin::read_nom`]: tezos_smart_rollup_keyspace::extensions::KeySpaceExtBin::read_nom
pub trait KeySpaceExtBounded: KeySpace {
    /// Returns the value whose binary encoding is stored at `key`, or `None`
    /// when the key is absent. The read takes at most `max_bytes` bytes.
    ///
    /// # Errors
    ///
    /// - [`StorageReadErrorKind::ValueExceedsBound`] when the bytes read do not
    ///   decode as `T` and the value at `key` is longer than `max_bytes`.
    /// - [`ReadKind::Nom`] when the bytes read are not exactly one encoding of
    ///   `T` and the value fits in `max_bytes`.
    fn read_nom_bounded<T: for<'a> NomReader<'a>>(
        &self,
        key: &Key,
        max_bytes: usize,
    ) -> Result<Option<T>, KeySpaceError<StorageReadErrorKind>> {
        Ok(self
            .read_nom_bounded_with_len(key, max_bytes)?
            .map(|(value, _len)| value))
    }

    /// Returns what [`Self::read_nom_bounded`] returns, together with the
    /// number of bytes read. That number is the encoded size of the value.
    fn read_nom_bounded_with_len<T: for<'a> NomReader<'a>>(
        &self,
        key: &Key,
        max_bytes: usize,
    ) -> Result<Option<(T, usize)>, KeySpaceError<StorageReadErrorKind>> {
        let mut buffer = vec![0; max_bytes];
        let Some(len) = self.read(key, 0, &mut buffer) else {
            return Ok(None);
        };
        let value = T::nom_read_exact(&buffer[..len]).map_err(|err| {
            // A value that does not decode can be a value cut by the bound. Only
            // this failure path pays for the length lookup.
            let kind = match self.value_length(key) {
                Some(length) if length > max_bytes => {
                    ReadKind::Ext(StorageReadErrorKind::ValueExceedsBound {
                        length,
                        max_bytes,
                    })
                }
                _ => err.into(),
            };
            KeySpaceError::read(self.name(), key, kind)
        })?;
        Ok(Some((value, len)))
    }
}

impl<KS: KeySpace> KeySpaceExtBounded for KS {}

/// Decodes the base58check text in `bytes` into a `T` with `from_b58check`.
///
/// # Errors
///
/// - [`StorageReadErrorKind::InvalidBase58`], as a read error of `key` in the
///   keyspace `keyspace_name`. It holds the error of `from_b58check`, or
///   [`FromBase58CheckError::InvalidBase58`] when `bytes` is not UTF-8.
fn decode_b58<T>(
    keyspace_name: &Name,
    key: &Key,
    bytes: &[u8],
    from_b58check: impl FnOnce(&str) -> Result<T, FromBase58CheckError>,
) -> Result<T, KeySpaceError<StorageReadErrorKind>> {
    std::str::from_utf8(bytes)
        // Base58 is ASCII, so bytes that are not UTF-8 are not base58 either.
        .map_err(|_| FromBase58CheckError::InvalidBase58)
        .and_then(from_b58check)
        .map_err(|err| KeySpaceError::read(keyspace_name, key, ReadKind::Ext(err.into())))
}

/// Base58 reads of contract addresses and public keys over a [`KeySpace`].
///
/// The value holds the base58 text. Invalid values are reported as
/// [`StorageReadErrorKind::InvalidBase58`] instead of being silently treated
/// as missing.
pub trait KeySpaceExtB58: KeySpace {
    /// Returns the base58 contract address stored at `key`.
    ///
    /// The first [`crate::KT1_B58_SIZE`] bytes of the value are decoded as a
    /// [`ContractKt1Hash`]. A missing key or a value shorter than that size
    /// returns `Ok(None)`.
    ///
    /// # Errors
    ///
    /// - [`StorageReadErrorKind::InvalidBase58`] when the bytes are not a
    ///   valid base58 contract address.
    fn get_b58_kt1(
        &self,
        key: &Key,
    ) -> Result<Option<ContractKt1Hash>, KeySpaceError<StorageReadErrorKind>> {
        let result = self.get_prefix_exact::<KT1_B58_SIZE>(key);
        if let Some(buffer) = result {
            decode_b58(self.name(), key, &buffer, ContractKt1Hash::from_b58check)
                .map(Some)
        } else {
            Ok(None)
        }
    }

    /// Returns the base58check public key stored at `key`.
    ///
    /// The whole value is read and decoded as a [`PublicKey`], in the format
    /// that [`PublicKey::to_b58check`] writes. A missing key returns
    /// `Ok(None)`.
    ///
    /// # Errors
    ///
    /// - [`StorageReadErrorKind::InvalidBase58`] when the bytes are not a
    ///   valid base58check public key.
    fn get_b58_public_key(
        &self,
        key: &Key,
    ) -> Result<Option<PublicKey>, KeySpaceError<StorageReadErrorKind>> {
        let Some(bytes) = self.get(key) else {
            return Ok(None);
        };
        decode_b58(self.name(), key, &bytes, PublicKey::from_b58check).map(Some)
    }
}

impl<KS: KeySpace> KeySpaceExtB58 for KS {}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_data_encoding::enc::BinWriter;
    use tezos_data_encoding::types::Narith;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_host::path::RefPath;
    use tezos_smart_rollup_keyspace::KeySpaceLoader;

    use crate::store_bin;

    const PATH: RefPath = RefPath::assert_from(b"/some/value");

    fn key(bytes: &[u8]) -> Key {
        Key::from_bytes(bytes).unwrap()
    }

    /// Makes sure that a bound larger than the value reads the value back.
    /// The test also makes sure that the `_with_len` variant reports the
    /// encoded size of the value, not the bound.
    #[test]
    fn bounded_read_matches_written_value() {
        let mut host = MockKernelHost::default();
        let value: Narith = 123_456_u64.into();
        store_bin(&value, &mut host, &PATH).unwrap();
        let mut encoded = Vec::new();
        value.bin_write(&mut encoded).unwrap();

        let ks = host.load_or_create("/some".parse().unwrap()).unwrap();
        let read: Option<Narith> = ks.read_nom_bounded(&key(b"/value"), 32).unwrap();
        assert_eq!(Some(value.clone()), read);

        let read_with_len = ks.read_nom_bounded_with_len(&key(b"/value"), 32).unwrap();
        assert_eq!(Some((value, encoded.len())), read_with_len);
    }

    /// Makes sure that an absent key reads as `Ok(None)` through both functions.
    #[test]
    fn bounded_read_missing_key() {
        let mut host = MockKernelHost::default();
        let ks = host.load_or_create("/some".parse().unwrap()).unwrap();

        let read: Option<Narith> = ks.read_nom_bounded(&key(b"/value"), 32).unwrap();
        assert_eq!(None, read);

        let read_with_len: Option<(Narith, usize)> =
            ks.read_nom_bounded_with_len(&key(b"/value"), 32).unwrap();
        assert_eq!(None, read_with_len);
    }

    /// Makes sure that a bound smaller than the value fails with
    /// `ValueExceedsBound`, which names the stored length and the bound.
    /// `128` encodes to two `Narith` bytes, so a one-byte bound cuts it.
    #[test]
    fn bounded_read_too_small_bound_fails() {
        let mut host = MockKernelHost::default();
        let value: Narith = 128_u64.into();
        store_bin(&value, &mut host, &PATH).unwrap();

        let ks = host.load_or_create("/some".parse().unwrap()).unwrap();
        let k = key(b"/value");
        let read: Result<Option<Narith>, _> = ks.read_nom_bounded(&k, 1);
        assert_eq!(
            read,
            Err(KeySpaceError::read(
                ks.name(),
                &k,
                ReadKind::Ext(StorageReadErrorKind::ValueExceedsBound {
                    length: 2,
                    max_bytes: 1,
                }),
            ))
        );
        assert_eq!(
            read.unwrap_err().to_string(),
            "at key /value of key space /some: \
             value of 2 bytes exceeds the bound of 1 bytes"
        );

        let read_with_len: Result<Option<(Narith, usize)>, _> =
            ks.read_nom_bounded_with_len(&key(b"/value"), 1);
        assert!(read_with_len.is_err());
    }

    /// Makes sure that a public key reads back, that an absent key reads as
    /// `Ok(None)`, and that a value that is not a base58check public key
    /// fails with `InvalidBase58`.
    #[test]
    fn public_key_base58_read() {
        let mut host = MockKernelHost::default();
        let mut ks = host.load_or_create("/some".parse().unwrap()).unwrap();
        let key = key(b"/public_key");
        let public_key_b58 = "edpkv4NmL2YPe8eiVGXUDXmPQybD725ofKirTzGRxs1X9UmaG3voKw";
        let public_key = PublicKey::from_b58check(public_key_b58).unwrap();

        assert_eq!(ks.get_b58_public_key(&key).unwrap(), None);

        ks.set(&key, public_key_b58.as_bytes()).unwrap();
        assert_eq!(ks.get_b58_public_key(&key).unwrap(), Some(public_key));

        let name = ks.name().clone();
        let invalid_base58 = |err| {
            Err(KeySpaceError::read(
                &name,
                &key,
                ReadKind::Ext(StorageReadErrorKind::InvalidBase58(err)),
            ))
        };

        ks.set(&key, b"not a public key").unwrap();
        let err = PublicKey::from_b58check("not a public key").unwrap_err();
        assert_eq!(ks.get_b58_public_key(&key), invalid_base58(err));

        ks.set(&key, [0xff, 0xfe]).unwrap();
        assert_eq!(
            ks.get_b58_public_key(&key),
            invalid_base58(FromBase58CheckError::InvalidBase58)
        );
    }
}

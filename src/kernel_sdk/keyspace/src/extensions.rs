// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Extension traits over [`KeySpace`], each implemented for every key space.
//!
//! A key space stores bytes; these traits read and write typed values at a
//! key. Bringing one into scope makes its accessors available on any key
//! space handle. The ones that need an encoding crate sit behind a feature.

use crate::{Key, KeySpace, KeySpaceError};
use num_traits::{FromBytes, ToBytes};
#[cfg(feature = "data-reveal")]
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
#[cfg(feature = "data-reveal")]
use tezos_smart_rollup_encoding::dac::pages::{
    reveal_loop, RevealLoopError, MAX_PAGE_SIZE,
};
#[cfg(feature = "data-reveal")]
use tezos_smart_rollup_host::reveal::HostReveal;

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
        value: &T,
    ) -> Result<(), KeySpaceError> {
        self.set(key, value.to_le_bytes())
    }

    /// The big-endian counterpart of [`Self::store_le`].
    fn store_be<T: ToBytes>(
        &mut self,
        key: &Key,
        value: &T,
    ) -> Result<(), KeySpaceError> {
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
    ) -> Result<(), KeySpaceError> {
        self.set(key, value.rlp_bytes())
    }

    /// Returns the rlp value at `key`, or `None` when the key is absent.
    /// Bytes that do not decode into `T` are a [`ReadKind::Rlp`](crate::ReadKind::Rlp).
    fn read_rlp<T: rlp::Decodable>(&self, key: &Key) -> Result<Option<T>, KeySpaceError> {
        match self.get(key) {
            Some(bytes) => rlp::decode(&bytes)
                .map(Some)
                .map_err(|err| KeySpaceError::read(self.name(), key, err.into())),
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
    ) -> Result<T, KeySpaceError> {
        Ok(self.read_rlp(key)?.unwrap_or(default))
    }
}

#[cfg(feature = "rlp")]
impl<KS: KeySpace> KeySpaceExtRlp for KS {}

/// Binary reads and writes over a [`KeySpace`], through the
/// `tezos_data_encoding` [`BinWriter`] and [`NomReader`] traits.
///
/// [`BinWriter`]: tezos_data_encoding::enc::BinWriter
/// [`NomReader`]: tezos_data_encoding::nom::NomReader
#[cfg(feature = "tezos-encoding")]
pub trait KeySpaceExtBin: KeySpace {
    /// Writes the binary encoding of `value` at `key`.
    ///
    /// # Errors
    ///
    /// - [`WriteKind::Encode`](crate::WriteKind::Encode) when `value` does not encode.
    /// - [`WriteKind::ValueSizeExceeded`](crate::WriteKind::ValueSizeExceeded) when the encoding exceeds the
    ///   largest value the key space accepts.
    fn store_bin(
        &mut self,
        key: &Key,
        value: &impl tezos_data_encoding::enc::BinWriter,
    ) -> Result<(), KeySpaceError> {
        let mut bytes = Vec::new();
        value
            .bin_write(&mut bytes)
            .map_err(|err| KeySpaceError::write(self.name(), key, err.into()))?;
        self.set(key, bytes)
    }

    /// Returns the value whose binary encoding is stored at `key`, or `None`
    /// when the key is absent.
    ///
    /// The decoder consumes the whole value: bytes left over are a
    /// [`ReadKind::Nom`](crate::ReadKind::Nom), not a shorter value.
    fn read_nom<T: for<'a> tezos_data_encoding::nom::NomReader<'a>>(
        &self,
        key: &Key,
    ) -> Result<Option<T>, KeySpaceError> {
        let Some(bytes) = self.get(key) else {
            return Ok(None);
        };
        T::nom_read_exact(&bytes)
            .map(Some)
            .map_err(|err| KeySpaceError::read(self.name(), key, err.into()))
    }

    /// [`Self::read_nom`], falling back to `default` when the key is absent.
    /// Only absence takes the fallback, bytes that do not decode stay an
    /// error.
    fn read_nom_or<T: for<'a> tezos_data_encoding::nom::NomReader<'a>>(
        &self,
        key: &Key,
        default: T,
    ) -> Result<T, KeySpaceError> {
        Ok(self.read_nom(key)?.unwrap_or(default))
    }
}

#[cfg(feature = "tezos-encoding")]
impl<KS: KeySpace> KeySpaceExtBin for KS {}

/// Depth of the preimage trees that [`KeySpaceExtDataReveal::reveal_to_key`]
/// reveals: up to three levels of hash pages above the content pages.
#[cfg(feature = "data-reveal")]
const MAX_TREE_LEVELS: usize = 4;

/// Failure of [`KeySpaceExtDataReveal::reveal_to_key`].
#[cfg(feature = "data-reveal")]
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum DataRevealError {
    /// The reveal of the preimage tree failed, or the write of one of its
    /// content pages.
    #[error(transparent)]
    Reveal(#[from] RevealLoopError<KeySpaceError>),
    /// The value at the key could not be emptied before the reveal.
    #[error(transparent)]
    KeySpace(#[from] KeySpaceError),
}

/// Reveals of a preimage tree into a [`KeySpace`].
#[cfg(feature = "data-reveal")]
pub trait KeySpaceExtDataReveal: KeySpace {
    /// Writes at `key`, in place of the value there, the content that the
    /// preimage tree of root `root_hash` reveals through `host`, one content
    /// page at a time: the content never sits whole in memory.
    ///
    /// The tree holds up to three levels of hash pages above its content
    /// pages.
    ///
    /// # Errors
    ///
    /// - [`DataRevealError::Reveal`] with the failure of [`reveal_loop`]: a
    ///   preimage that does not reveal or is not a page, a tree that is too
    ///   deep, or the write of a content page
    ///   ([`RevealLoopError::Content`]).
    /// - [`DataRevealError::KeySpace`] when the value at `key` cannot be
    ///   emptied first.
    ///
    /// On an error, the value at `key` holds the content written before it.
    fn reveal_to_key(
        &mut self,
        host: &mut impl HostReveal,
        root_hash: &[u8; PREIMAGE_HASH_SIZE],
        key: &Key,
    ) -> Result<(), DataRevealError> {
        self.set(key, [])?;
        let mut offset = 0;
        let mut buffer = [0; MAX_PAGE_SIZE * MAX_TREE_LEVELS];
        reveal_loop(
            host,
            0,
            root_hash,
            &mut buffer,
            MAX_TREE_LEVELS,
            &mut |_, page| {
                offset += self.write(key, offset, page.as_ref())?;
                Ok::<_, KeySpaceError>(())
            },
        )?;
        Ok(())
    }
}

#[cfg(feature = "data-reveal")]
impl<KS: KeySpace> KeySpaceExtDataReveal for KS {}

/// Tests for the extension traits, over a key space minted on a mock host.
#[cfg(all(test, feature = "irmin-compat"))]
mod tests {
    use super::*;
    use crate::irmin_ds::{IrminKeySpaceRegistry, StorageV1KeySpaceCompat};
    #[cfg(feature = "data-reveal")]
    use tezos_smart_rollup_encoding::dac::pages::SlicePageError;
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

    /// A content page holding `content`.
    #[cfg(feature = "data-reveal")]
    fn content_page(content: &[u8]) -> Vec<u8> {
        [&[0][..], &(content.len() as u32).to_be_bytes(), content].concat()
    }

    /// A hash page holding `hashes`.
    #[cfg(feature = "data-reveal")]
    fn hash_page(hashes: &[[u8; PREIMAGE_HASH_SIZE]]) -> Vec<u8> {
        let hashes = hashes.concat();
        [&[1][..], &(hashes.len() as u32).to_be_bytes(), &hashes].concat()
    }

    #[test]
    fn integer_accessors_round_trip_both_endiannesses() {
        let mut host = MockHost::default();
        let mut registry = IrminKeySpaceRegistry::new();
        let mut ks = load(&mut registry, &mut host, "/ints");
        let k = key(b"/n");

        ks.store_le(&k, &0x0102_0304u32).unwrap();
        assert_eq!(ks.get_le(&k), Some(0x0102_0304u32));
        // Read the other way round, the same bytes give the swapped value.
        assert_eq!(ks.get_be(&k), Some(0x0403_0201u32));

        ks.store_be(&k, &0x0102_0304u32).unwrap();
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
        let decode_err = rlp::decode::<Vec<u8>>(&[0xc0, 0xff]).unwrap_err();
        assert_eq!(
            ks.read_rlp::<Vec<u8>>(&k).unwrap_err(),
            KeySpaceError::read(ks.name(), &k, decode_err.into())
        );
        assert!(ks.read_rlp_or(&k, vec![9u8]).is_err());
    }

    #[cfg(feature = "tezos-encoding")]
    #[test]
    fn bin_accessors_round_trip_and_reject_a_partial_value() {
        use tezos_data_encoding::types::Narith;

        let mut host = MockHost::default();
        let mut registry = IrminKeySpaceRegistry::new();
        let mut ks = load(&mut registry, &mut host, "/bin");
        let k = key(b"/value");
        let value: Narith = 300u64.into();

        ks.store_bin(&k, &value).unwrap();
        assert_eq!(ks.read_nom::<Narith>(&k).unwrap(), Some(value.clone()));

        // An absent key is `None`, or the default where there is one.
        let missing = key(b"/missing");
        let fallback: Narith = 7u64.into();
        assert_eq!(ks.read_nom::<Narith>(&missing).unwrap(), None);
        assert_eq!(
            ks.read_nom_or(&missing, fallback.clone()).unwrap(),
            fallback
        );
        assert_eq!(ks.read_nom_or(&k, fallback.clone()).unwrap(), value);

        // A value the decoder cannot finish is an error, and so is one it
        // finishes with bytes to spare. The default does not cover either.
        ks.set(&k, [0x80]).unwrap();
        let decode_err =
            <Narith as tezos_data_encoding::nom::NomReader>::nom_read_exact(&[0x80])
                .unwrap_err();
        assert_eq!(
            ks.read_nom::<Narith>(&k).unwrap_err(),
            KeySpaceError::read(ks.name(), &k, decode_err.into())
        );
        assert!(ks.read_nom_or(&k, fallback.clone()).is_err());
        ks.set(&k, [0x01, 0x01]).unwrap();
        assert!(ks.read_nom::<Narith>(&k).is_err());
    }

    #[cfg(feature = "data-reveal")]
    #[test]
    fn data_reveal_writes_the_content_pages_in_place_of_the_value() {
        let mut host = MockHost::default();
        let mut registry = IrminKeySpaceRegistry::new();
        let mut ks = load(&mut registry, &mut host, "/data-reveal");
        let k = key(b"/value");
        let first = host.set_preimage(content_page(b"first "));
        let second = host.set_preimage(content_page(b"second"));
        let root_hash = host.set_preimage(hash_page(&[first, second]));
        // Longer than the content: none of its bytes stay.
        ks.set(&k, [0xff; 64]).unwrap();

        ks.reveal_to_key(&mut host, &root_hash, &k).unwrap();

        assert_eq!(ks.get(&k), Some(b"first second".to_vec()));
    }

    #[cfg(feature = "data-reveal")]
    #[test]
    fn data_reveal_stops_on_a_preimage_that_is_not_a_page() {
        let mut host = MockHost::default();
        let mut registry = IrminKeySpaceRegistry::new();
        let mut ks = load(&mut registry, &mut host, "/data-reveal");
        let k = key(b"/value");
        let first = host.set_preimage(content_page(b"first"));
        let not_a_page = host.set_preimage(vec![0xff; 8]);
        let root_hash = host.set_preimage(hash_page(&[first, not_a_page]));

        assert_eq!(
            ks.reveal_to_key(&mut host, &root_hash, &k),
            Err(DataRevealError::Reveal(RevealLoopError::Page {
                hash: not_a_page,
                error: SlicePageError::InvalidTag(Some(0xff))
            }))
        );
        // The content written before the failure stays.
        assert_eq!(ks.get(&k), Some(b"first".to_vec()));
    }
}

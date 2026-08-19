// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! A high-level API for the durable storage.
//!
//! A [`KeySpace`] is a flat key-value store which can hold arbitrarily-sized data
//! for a given [`Key`]. Kernels can create and delete [`KeySpace`] instances as required
//! and can generate hashes representing the entire state of a [`KeySpace`].

use std::borrow::Cow;
use std::str::FromStr;

use tezos_smart_rollup_host::storage::v2;

#[cfg(feature = "irmin-compat")]
pub mod irmin_ds;
#[cfg(feature = "irmin-compat")]
mod irmin_path_validator;
pub mod wasm_nds;

/// The budget a caller building names has to stay within.
#[cfg(feature = "irmin-compat")]
pub use irmin_path_validator::{MAX_KEYSPACE_NAME_SIZE, MAX_KEY_SIZE};

/// Key creation error
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum KeyError {
    /// Attempted to create a key that exceeds the maximum allowed size.
    #[error("key exceeds the maximum allowed size")]
    KeyTooLarge,
    /// Path validation error (e.g. missing leading `/`, invalid bytes, empty step,
    /// or reserved `/readonly` prefix).
    #[cfg(feature = "irmin-compat")]
    #[error("invalid irmin-durable path: {0}")]
    PathError(tezos_smart_rollup_host::path::PathError),
}

/// Key used to access data in a [`KeySpace`].
///
/// A `Key` owns its validated bytes: borrowed for compile-time constants
/// (see [`Key::from_static`]), owned for keys built dynamically (see
/// [`Key::from_bytes`]). Being a sized, owned type, a key can be returned from
/// a builder and passed by reference wherever a `&Key` is expected.
///
/// `Key` is guaranteed compatible with `WasmNds` key, regardless of whether
/// `irmin-compat` feature is enabled.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Key(Cow<'static, [u8]>);

impl Key {
    /// `Key` must be a valid [`v2::Key`].
    #[cfg(not(feature = "irmin-compat"))]
    const fn check_bytes(bytes: &[u8]) -> Result<(), KeyError> {
        match v2::Key::new(bytes) {
            Some(_key) => Ok(()),
            None => Err(KeyError::KeyTooLarge),
        }
    }

    /// Create a key from raw bytes, taking ownership of them.
    ///
    /// Returns an error if `bytes` fails validation (see `Key::check_bytes`).
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, KeyError> {
        Self::check_bytes(bytes)?;
        Ok(Key(Cow::Owned(bytes.to_vec())))
    }

    /// Create a key from static bytes without allocating. Should only be used
    /// for constant keys.
    ///
    /// Panics if `bytes` is an invalid key.
    pub const fn from_static(bytes: &'static [u8]) -> Self {
        assert!(Self::check_bytes(bytes).is_ok(), "Invalid key");
        Key(Cow::Borrowed(bytes))
    }

    /// The raw bytes of the key.
    pub fn as_bytes(&self) -> &[u8] {
        self.0.as_ref()
    }

    /// A key made of `self` followed by `suffix`.
    ///
    /// Takes anything that reads as bytes, so a constant segment (`Key`) and a
    /// computed one (a `String` from `format!`) compose the same way.
    // TODO: https://linear.app/tezos/issue/L2-1971
    // A key builder would take over the composition rules this leaves to the
    // caller: irmin separators, prefix overlap, and bytes hex-encoded only to
    // survive path validation.
    pub fn concat(&self, suffix: impl AsRef<[u8]>) -> Result<Self, KeyError> {
        let mut bytes = self.as_bytes().to_vec();
        bytes.extend_from_slice(suffix.as_ref());
        Self::try_from(bytes)
    }
}

impl AsRef<[u8]> for Key {
    fn as_ref(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl AsRef<v2::Key> for Key {
    fn as_ref(&self) -> &v2::Key {
        v2::Key::new(self.as_bytes())
            .expect("KeySpace Key is guaranteed to always be a valid WasmNds Key")
    }
}

impl TryFrom<Vec<u8>> for Key {
    type Error = KeyError;

    /// Create a key from owned bytes without reallocating.
    fn try_from(bytes: Vec<u8>) -> Result<Self, Self::Error> {
        Self::check_bytes(&bytes)?;
        Ok(Key(Cow::Owned(bytes)))
    }
}

impl TryFrom<String> for Key {
    type Error = KeyError;

    /// Create a key from an owned string without reallocating.
    fn try_from(s: String) -> Result<Self, Self::Error> {
        Self::try_from(s.into_bytes())
    }
}

impl core::fmt::Display for Key {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        // With irmin-compat, key bytes are valid ASCII path characters;
        // otherwise we print the hex-encoded bytes.
        match core::str::from_utf8(self.as_bytes()) {
            Ok(s) => f.write_str(s),
            Err(_) => write!(f, "{:02x?}", self.as_bytes()),
        }
    }
}

/// Name creation error
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum NameError {
    /// Attempted to create a name that exceeds the maximum allowed size.
    #[error("name exceeds the maximum allowed size")]
    NameTooLong,
    /// Path validation error (e.g. missing leading `/`, trailing `/`, invalid bytes,
    /// or reserved `/readonly` prefix).
    #[cfg(feature = "irmin-compat")]
    #[error("invalid irmin-durable storage path: {0}")]
    PathError(tezos_smart_rollup_host::path::PathError),
}

/// A validated keyspace name, used as a path prefix in durable storage.
///
/// Like [`Key`], a `Name` owns its validated bytes: borrowed for constants
/// (see [`Name::from_static`]), owned when parsed at runtime (see its
/// [`FromStr`] implementation).
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct Name(Cow<'static, str>);

impl Name {
    // [`Name`] must be a valid [`v2::Key`].
    #[cfg(not(feature = "irmin-compat"))]
    const fn check_bytes(bytes: &[u8]) -> Result<(), NameError> {
        match v2::Key::new(bytes) {
            Some(_key) => Ok(()),
            None => Err(NameError::NameTooLong),
        }
    }

    /// Create a name from a static string without allocating. Should only be
    /// used for constant names.
    ///
    /// Panics if `name` is an invalid keyspace name.
    pub const fn from_static(name: &'static str) -> Self {
        assert!(Self::check_bytes(name.as_bytes()).is_ok(), "Invalid name");
        Name(Cow::Borrowed(name))
    }

    #[cfg(all(pvm_kind = "wasm", not(feature = "irmin-compat")))]
    fn as_key(&self) -> &v2::Key {
        let name: &str = self.as_ref();
        v2::Key::new(name.as_bytes())
            .expect("KeySpace Name is guaranteed to always be a valid WasmNds key")
    }
}

impl core::fmt::Display for Name {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str(self.0.as_ref())
    }
}

impl AsRef<str> for Name {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl FromStr for Name {
    type Err = NameError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::check_bytes(s.as_bytes())?;
        Ok(Name(Cow::Owned(s.to_owned())))
    }
}

impl TryFrom<String> for Name {
    type Error = NameError;

    /// Create a name from an owned string without reallocating.
    fn try_from(s: String) -> Result<Self, Self::Error> {
        Self::check_bytes(s.as_bytes())?;
        Ok(Name(Cow::Owned(s)))
    }
}

/// Errors that can occur when writing to a [`KeySpace`].
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum KeySpaceWriteError {
    /// Attempted to write more than the maximum allowed bytes at a given key.
    #[error("value size exceeded the maximum allowed")]
    ValueSizeExceeded,
    /// The write offset exceeds the current length of the stored value.
    #[error("write offset exceeds the current length of the stored value")]
    InvalidOffset,
}

/// A key space in the durable storage.
///
/// A `KeySpace` is a flat key-value store. Instances are created via
/// [`KeySpaceLoader::load_or_create`].
pub trait KeySpace {
    /// The name this key space was loaded under, i.e. the [`Name`] passed to
    /// [`KeySpaceLoader::load_or_create`] when this handle was created.
    fn name(&self) -> &Name;

    /// Read the whole value associated with the key.
    /// Returns `None` if the key does not exist.
    fn get(&self, key: &Key) -> Option<Vec<u8>>;

    /// Read the first `N` bytes of the value associated with the key and return them in a fixed-size array.
    /// Returns `None` if the key does not exist or if the value is shorter than `N` bytes.
    ///
    /// *NB* care should be taken that `N` is small - certainly smaller than a page on the current
    /// architecture. Otherwise, it may be possible to trigger a stack overflow, by allocating too
    /// much on the stack in one go - and bypassing rust's [guard page](https://doc.rust-lang.org/rustc/exploit-mitigations.html#stack-clashing-protection).
    fn get_prefix_exact<const N: usize>(&self, key: &Key) -> Option<[u8; N]> {
        self.read_exact_in(key, 0)
    }

    /// Read a portion of the value associated with the key and return the number of bytes written.
    /// Returns `None` if the key does not exist, or if the offset is invalid.
    fn read(&self, key: &Key, offset: usize, buffer: &mut [u8]) -> Option<usize>;

    /// Read the exact number of bytes from a portion of the value associated with the key and return them in a fixed-size array.
    /// Returns `None` if the key does not exist or if the value is not long enough to read N bytes from the given offset.
    ///
    /// *NB* care should be taken that `N` is small - certainly smaller than a page on the current
    /// architecture. Otherwise, it may be possible to trigger a stack overflow, by allocating too
    /// much on the stack in one go - and bypassing rust's [guard page](https://doc.rust-lang.org/rustc/exploit-mitigations.html#stack-clashing-protection).
    fn read_exact_in<const N: usize>(&self, key: &Key, offset: usize) -> Option<[u8; N]> {
        let mut buffer = [0u8; N];
        match self.read(key, offset, &mut buffer) {
            Some(bytes_read) if bytes_read == N => Some(buffer),
            _ => None,
        }
    }

    /// Write the given value to the key.
    fn set(
        &mut self,
        key: &Key,
        value: impl AsRef<[u8]>,
    ) -> Result<(), KeySpaceWriteError>;

    /// Write data to the value at the given key starting at the given offset.
    /// Returns the number of bytes written.
    fn write(
        &mut self,
        key: &Key,
        offset: usize,
        data: impl AsRef<[u8]>,
    ) -> Result<usize, KeySpaceWriteError>;

    /// Retrieve the length of the value associated with the key.
    /// Returns `None` if the key does not exist.
    fn value_length(&self, key: &Key) -> Option<usize>;

    /// Check if the given key exists in the key space.
    fn contains(&self, key: &Key) -> bool;

    /// Remove the key from the key space. Returns false if the key did not exist.
    fn delete(&mut self, key: &Key) -> bool;

    /// Remove all key-value associations from the key space.
    fn clear(&mut self);

    /// Replace the key-value associations in `self` with those of `other`.
    /// The `other` key space is not modified.
    fn copy_from(&mut self, other: &Self);

    /// Replace the key-value associations in `self` with those of `other` and
    /// remove all key-value associations from `other`.
    fn move_from(&mut self, other: &mut Self);

    /// Obtain the hash that represents the current state of the key space. This hash is
    /// sensitive to the key-value associations and the order in which they were added.
    fn hash(&self) -> Vec<u8>;
}

/// A mutable borrow of a [`KeySpace`] is itself a [`KeySpace`], forwarding to
/// the borrowed handle. A caller holding a key space can therefore lend it to a
/// structure that takes one by value, and keep it once that structure dies.
impl<KS: KeySpace> KeySpace for &mut KS {
    fn name(&self) -> &Name {
        (**self).name()
    }

    fn get(&self, key: &Key) -> Option<Vec<u8>> {
        (**self).get(key)
    }

    fn read(&self, key: &Key, offset: usize, buffer: &mut [u8]) -> Option<usize> {
        (**self).read(key, offset, buffer)
    }

    fn set(
        &mut self,
        key: &Key,
        value: impl AsRef<[u8]>,
    ) -> Result<(), KeySpaceWriteError> {
        (**self).set(key, value)
    }

    fn write(
        &mut self,
        key: &Key,
        offset: usize,
        data: impl AsRef<[u8]>,
    ) -> Result<usize, KeySpaceWriteError> {
        (**self).write(key, offset, data)
    }

    fn value_length(&self, key: &Key) -> Option<usize> {
        (**self).value_length(key)
    }

    fn contains(&self, key: &Key) -> bool {
        (**self).contains(key)
    }

    fn delete(&mut self, key: &Key) -> bool {
        (**self).delete(key)
    }

    fn clear(&mut self) {
        (**self).clear()
    }

    fn copy_from(&mut self, other: &Self) {
        (**self).copy_from(other)
    }

    fn move_from(&mut self, other: &mut Self) {
        (**self).move_from(other)
    }

    fn hash(&self) -> Vec<u8> {
        (**self).hash()
    }
}

/// Error returned by [`KeySpaceLoader::load_or_create`].
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum KeySpaceLoaderError {
    /// A key space whose name overlaps (is a prefix of, or has as prefix) the
    /// requested name is already loaded. Only meaningful when names form a
    /// hierarchical path, which is why this variant is gated on `irmin-compat`.
    #[cfg(feature = "irmin-compat")]
    #[error("key space name overlaps an already-loaded key space")]
    Overlapping,
    /// A key space with this exact name is already loaded and has not been
    /// dropped yet.
    #[error("a key space with this name is already loaded")]
    AlreadyLoaded,
    /// Invalid/Inconsistent storage detected
    #[cfg(not(feature = "irmin-compat"))]
    #[error("KeySpaceLoader encountered a malformed name mapping for {0}")]
    InconsistentNameMapping(Name),
    /// Only up to `i32::MAX` databases are supported.
    #[cfg(not(feature = "irmin-compat"))]
    #[error("Could not allocated database for the given name - ran out of db indices.")]
    TooManyDatabases,
}

/// A loader for [`KeySpace`] instances backed by a specific storage implementation.
///
/// The loader tracks which keyspace names have been used and prevents
/// overlapping names. Callers receive an owned key space handle; when the
/// handle is dropped, the same name can be reloaded, but names that overlap
/// with any previously loaded name are permanently rejected.
pub trait KeySpaceLoader {
    /// The type of key space produced by this loader.
    type KeySpace: KeySpace;

    /// Load or create a key space with the given name.
    ///
    /// Returns an owned key space handle. The caller may hold multiple
    /// key spaces simultaneously as long as their names do not overlap.
    ///
    /// Returns an error if the requested name overlaps with any
    /// previously loaded key space, or if a key space with this exact
    /// name is already loaded.
    fn load_or_create(
        &mut self,
        name: Name,
    ) -> Result<Self::KeySpace, KeySpaceLoaderError>;
}

/// Tests for [`KeySpace`] served through a mutable borrow. Keep the helpers
/// generic: called on a `&mut KS` value directly, the `&mut self` methods
/// resolve to `KS`'s own impl and skip the forwarder.
#[cfg(all(test, feature = "irmin-compat"))]
mod tests {
    use super::*;
    use crate::irmin_ds::{IrminKeySpaceRegistry, StorageV1KeySpaceCompat};
    use tezos_smart_rollup_mock::{InMemoryStore, MockHost};

    type MockKeySpace = StorageV1KeySpaceCompat<InMemoryStore>;

    fn key(s: &[u8]) -> Key {
        Key::from_bytes(s).unwrap()
    }

    /// Load `name` over `host`. Key spaces minted over one `host` share its
    /// durable tree; the registry only keeps their names from overlapping.
    fn load(
        registry: &mut IrminKeySpaceRegistry,
        host: &mut MockHost,
        name: &str,
    ) -> MockKeySpace {
        // SAFETY: this registry is the only one minting key spaces over `host`,
        // and it rejects overlapping names.
        unsafe { registry.load_or_create(host, name.parse().unwrap()) }
            .expect("name does not overlap a loaded one")
    }

    fn lent_reads_and_writes<KS: KeySpace>(mut base: KS) {
        assert_eq!(base.name(), &"/lent".parse().unwrap());
        assert_eq!(base.get(&key(b"/kept")), Some(b"before".to_vec()));
        assert_eq!(base.value_length(&key(b"/kept")), Some(6));
        assert!(base.contains(&key(b"/kept")));
        assert!(!base.contains(&key(b"/absent")));

        let mut buffer = [0u8; 3];
        assert_eq!(base.read(&key(b"/kept"), 3, &mut buffer), Some(3));
        assert_eq!(&buffer, b"ore");

        base.set(&key(b"/added"), b"hello").unwrap();
        assert_eq!(base.write(&key(b"/added"), 0, b"HELL").unwrap(), 4);
        assert!(base.delete(&key(b"/kept")));
        assert!(!base.delete(&key(b"/kept")));
    }

    fn lent_clear<KS: KeySpace>(mut base: KS) {
        base.clear()
    }

    fn lent_hash<KS: KeySpace>(base: KS) -> Vec<u8> {
        base.hash()
    }

    /// At `KS = &mut MockKeySpace`, `other: &KS` is a `&&mut MockKeySpace`,
    /// hence `&&mut src` at the call site (`&mut &mut src` for `move_from`).
    fn lent_copy_from<KS: KeySpace>(mut base: KS, other: &KS) {
        base.copy_from(other)
    }

    fn lent_move_from<KS: KeySpace>(mut base: KS, other: &mut KS) {
        base.move_from(other)
    }

    #[test]
    fn borrowed_key_space_forwards_reads_and_writes() {
        let mut host = MockHost::default();
        let mut registry = IrminKeySpaceRegistry::new();
        let mut ks = load(&mut registry, &mut host, "/lent");
        ks.set(&key(b"/kept"), b"before").unwrap();
        let hash_before = ks.hash();

        // The caller lends its key space, and keeps it.
        lent_reads_and_writes(&mut ks);
        assert_eq!(lent_hash(&mut ks), ks.hash());

        // Every mutation made through the borrow is visible on the key space,
        // and it is writable again now that the borrow is over.
        assert_eq!(ks.get(&key(b"/added")), Some(b"HELLo".to_vec()));
        assert!(!ks.contains(&key(b"/kept")));
        assert_ne!(ks.hash(), hash_before);
        ks.set(&key(b"/after"), b"again").unwrap();
        assert!(ks.contains(&key(b"/after")));
    }

    #[test]
    fn borrowed_key_space_forwards_clear() {
        let mut host = MockHost::default();
        let mut registry = IrminKeySpaceRegistry::new();
        let mut ks = load(&mut registry, &mut host, "/lent");
        ks.set(&key(b"/a"), b"1").unwrap();
        ks.set(&key(b"/b"), b"2").unwrap();

        lent_clear(&mut ks);

        assert!(!ks.contains(&key(b"/a")));
        assert!(!ks.contains(&key(b"/b")));
    }

    #[test]
    fn borrowed_key_space_forwards_copy_from() {
        let mut host = MockHost::default();
        let mut registry = IrminKeySpaceRegistry::new();
        let mut dst = load(&mut registry, &mut host, "/dst");
        let mut src = load(&mut registry, &mut host, "/src");
        dst.set(&key(b"/old"), b"dst").unwrap();
        src.set(&key(b"/new"), b"src").unwrap();

        lent_copy_from(&mut dst, &&mut src);

        // `dst` holds `src`'s associations and only them, `src` holds its own.
        assert_eq!(dst.get(&key(b"/new")), Some(b"src".to_vec()));
        assert!(!dst.contains(&key(b"/old")));
        assert_eq!(src.get(&key(b"/new")), Some(b"src".to_vec()));
    }

    #[test]
    fn borrowed_key_space_forwards_move_from() {
        let mut host = MockHost::default();
        let mut registry = IrminKeySpaceRegistry::new();
        let mut dst = load(&mut registry, &mut host, "/dst");
        let mut src = load(&mut registry, &mut host, "/src");
        dst.set(&key(b"/old"), b"dst").unwrap();
        src.set(&key(b"/new"), b"src").unwrap();

        lent_move_from(&mut dst, &mut &mut src);

        // `dst` holds `src`'s associations, and `src` is left empty.
        assert_eq!(dst.get(&key(b"/new")), Some(b"src".to_vec()));
        assert!(!dst.contains(&key(b"/old")));
        assert!(!src.contains(&key(b"/new")));
    }
}

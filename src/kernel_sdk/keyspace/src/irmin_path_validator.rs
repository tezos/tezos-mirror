// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Irmin-specific validation for [`Key`] and [`Name`] byte contents.
//!
//! Both [`Name`] and [`Key`] follow the same rules as an irmin storage path:
//! they must start with `/`, must not end with `/`, steps must contain only
//! bytes matching `[A-Za-z0-9._-]`, and no empty steps (`//`) are allowed.
//!
//! This module delegates structural validation to
//! [`tezos_smart_rollup_host::path::validate_path`] to avoid duplicating logic.
//!
//! Examples:
//! - Valid [`Name`]: `/evm/world_state`, `/kernel`
//! - Valid [`Key`]: `/my_key`, `/a/b/c`

use tezos_smart_rollup_host::path::{validate_path, Path, PATH_MAX_SIZE};

use super::{Key, Name};
use crate::{KeyError, NameError};

/// The maximum size (in bytes) of a keyspace name.
///
/// A keyspace name represents a path prefix in the irmin-backed durable storage.
/// Together with the key, the full path `{name}{key}` must fit within [`PATH_MAX_SIZE`].
pub const MAX_KEYSPACE_NAME_SIZE: usize = 64;

/// The maximum size (in bytes) that a key can have.
///
/// Derived from the irmin path limit: a full path is `{name}{key}`,
/// so `key_max = PATH_MAX_SIZE - MAX_KEYSPACE_NAME_SIZE`.
pub const MAX_KEY_SIZE: usize = match PATH_MAX_SIZE.checked_sub(MAX_KEYSPACE_NAME_SIZE) {
    Some(size) => size,
    None => panic!("PATH_MAX_SIZE must be greater than MAX_KEYSPACE_NAME_SIZE"),
};

impl Key {
    /// Validate that the given bytes form a valid irmin path for a key.
    ///
    /// Must start with `/`, must not end with `/`, must not contain `//`,
    /// and each step must contain only bytes matching `[A-Za-z0-9._-]`.
    /// Must not exceed [`MAX_KEY_SIZE`] bytes.
    /// Must not be `/readonly` (reserved by the irmin durable storage).
    pub(super) const fn check_bytes(bytes: &[u8]) -> Result<(), KeyError> {
        if bytes.len() > MAX_KEY_SIZE {
            return Err(KeyError::KeyTooLarge);
        }
        match validate_path(bytes) {
            Ok(()) => Ok(()),
            Err(e) => Err(KeyError::PathError(e)),
        }
    }
}

impl Name {
    /// Validate that this name is a valid irmin path.
    ///
    /// Must start with `/`, must not end with `/`, must not contain `//`,
    /// and each step must contain only bytes matching `[A-Za-z0-9._-]`.
    /// Must not exceed [`MAX_KEYSPACE_NAME_SIZE`] bytes.
    /// Must not be `/readonly` (reserved by the irmin durable storage).
    pub(super) const fn check_bytes(bytes: &[u8]) -> Result<(), NameError> {
        if bytes.len() > MAX_KEYSPACE_NAME_SIZE {
            return Err(NameError::NameTooLong);
        }
        match validate_path(bytes) {
            Ok(()) => Ok(()),
            Err(e) => Err(NameError::PathError(e)),
        }
    }
}

// SAFETY: Name contents are validated irmin path bytes at construction time.
unsafe impl Path for Name {
    fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }
}

// SAFETY: delegates to the `Path` impl on `Name`.
unsafe impl Path for &Name {
    fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }
}

// Also implement for references, so `&Key` can be used directly with `concat`.
// SAFETY: delegates to the `Path` impl on `Key`.
unsafe impl Path for &Key {
    fn as_bytes(&self) -> &[u8] {
        &self.0
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::{MAX_KEYSPACE_NAME_SIZE, MAX_KEY_SIZE};
    use crate::{Key, KeyError, Name, NameError};

    #[test]
    fn key_too_large() {
        let key: Vec<u8> = std::iter::once(b'/')
            .chain(std::iter::repeat_n(b'a', MAX_KEY_SIZE))
            .collect();
        assert!(matches!(Key::from_bytes(&key), Err(KeyError::KeyTooLarge)));
    }

    #[test]
    fn name_too_long() {
        let name: String = std::iter::once('/')
            .chain(std::iter::repeat_n('a', MAX_KEYSPACE_NAME_SIZE))
            .collect();
        assert!(matches!(Name::from_str(&name), Err(NameError::NameTooLong)));
    }

    #[test]
    fn readonly_name_rejected() {
        assert!(matches!(
            Name::from_str("/readonly"),
            Err(NameError::PathError(
                tezos_smart_rollup_host::path::PathError::ReadOnly
            ))
        ));
    }

    #[test]
    fn readonly_prefix_allowed() {
        // `/readonly_stuff` is not the reserved name `/readonly`
        assert!(Name::from_str("/readonly_stuff").is_ok());
    }

    #[test]
    fn readonly_key_rejected() {
        assert!(matches!(
            Key::from_bytes(b"/readonly"),
            Err(KeyError::PathError(
                tezos_smart_rollup_host::path::PathError::ReadOnly
            ))
        ));
    }

    #[test]
    fn readonly_key_prefix_rejected() {
        assert!(matches!(
            Key::from_bytes(b"/readonly/something"),
            Err(KeyError::PathError(
                tezos_smart_rollup_host::path::PathError::ReadOnly
            ))
        ));
    }

    #[test]
    fn readonly_key_prefix_allowed() {
        // `/readonly_stuff` is not the reserved `/readonly` prefix
        assert!(Key::from_bytes(b"/readonly_stuff").is_ok());
    }
}

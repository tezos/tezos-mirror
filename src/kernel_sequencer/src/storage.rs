// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_host::{
    path::{concat, OwnedPath, Path, RefPath, PATH_SEPARATOR},
    runtime::RuntimeError,
    Error,
};

const SEQUENCER_PREFIX_PATH: RefPath = RefPath::assert_from(b"/__sequencer");
const USER_PREFIX_PATH: RefPath = RefPath::assert_from(b"/u");

/// Prefix the given path by `/__sequencer`.
///
/// This function has to be used when writing/reading storage related to the sequencer kernel.
/// Then with this function, all the sequencer kernel storage should be under the path `__sequencer`.
#[allow(dead_code)]
pub fn sequencer_prefix<T: Path>(path: &T) -> Result<OwnedPath, RuntimeError> {
    concat(&SEQUENCER_PREFIX_PATH, path).map_err(|_| RuntimeError::HostErr(Error::StoreInvalidKey))
}

/// Prefix the given path by `/u`.
///
/// If the given path starts by `/kernel`, then the path is unchanged.
/// If the given path starts by `/__sdk`, then the path is unchanged.
pub fn map_user_path<T: Path>(path: &T) -> Result<OwnedPath, RuntimeError> {
    match path.as_bytes() {
        [PATH_SEPARATOR, b'k', b'e', b'r', b'n', b'e', b'l', PATH_SEPARATOR, ..]
        | [PATH_SEPARATOR, b'_', b'_', b's', b'd', b'k', PATH_SEPARATOR, ..] => Ok(path.into()),
        _ => concat(&USER_PREFIX_PATH, path)
            .map_err(|_| RuntimeError::HostErr(Error::StoreInvalidKey)),
    }
}

#[cfg(test)]
mod tests {
    use tezos_smart_rollup_host::path::{concat, OwnedPath, RefPath};

    use super::{map_user_path, sequencer_prefix, USER_PREFIX_PATH};

    #[test]
    fn test_sequencer_prefixed() {
        let path = RefPath::assert_from(b"/a/b/c");
        let prefixed = sequencer_prefix(&path).expect("prefixing the path should work");

        assert_eq!(prefixed, RefPath::assert_from(b"/__sequencer/a/b/c").into());
    }

    #[test]
    fn test_map_kernel_boot() {
        let kernel_boot_path: OwnedPath = RefPath::assert_from(b"/kernel/boot.wasm").into();
        let mapped_kernel_boot_path = map_user_path(&kernel_boot_path).unwrap();

        assert_eq!(kernel_boot_path, mapped_kernel_boot_path)
    }

    #[test]
    fn test_map_user_path() {
        let user_path = RefPath::assert_from(b"/state/defined/by/user");
        let prefixed_user_path = map_user_path(&user_path).unwrap();
        let expected = concat(&USER_PREFIX_PATH, &user_path).unwrap();
        assert_eq!(expected, prefixed_user_path);
    }
}

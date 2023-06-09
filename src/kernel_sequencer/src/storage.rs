// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use crate::state::State;
use tezos_data_encoding::enc::BinWriter;
use tezos_data_encoding::nom::NomReader;
use tezos_smart_rollup_host::{
    path::{concat, OwnedPath, Path, RefPath, PATH_SEPARATOR},
    runtime::{Runtime, RuntimeError},
    Error,
};

const SEQUENCER_PREFIX_PATH: RefPath = RefPath::assert_from(b"/__sequencer");
const USER_PREFIX_PATH: RefPath = RefPath::assert_from(b"/u");
pub const DELAYED_INBOX_PATH: RefPath = RefPath::assert_from(b"/delayed-inbox");
const STATE: RefPath = RefPath::assert_from(b"/state");

/// Prefix the given path by `/__sequencer`.
///
/// This function has to be used when writing/reading storage related to the sequencer kernel.
/// Then with this function, all the sequencer kernel storage should be under the path `__sequencer`.
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

/// Write the sequencer kernel state under /__sequencer/state
pub fn write_state<H: Runtime>(host: &mut H, state: State) -> Result<(), RuntimeError> {
    let path = sequencer_prefix(&STATE)?;
    let mut bytes = Vec::default();
    state
        .bin_write(&mut bytes)
        .map_err(|_| RuntimeError::DecodingError)?;

    host.store_write(&path, &bytes, 0)
}

/// Returns the state of the sequencer kernel.
///
/// Or the default value if it is not present in storage.
pub fn read_state<H: Runtime>(host: &mut H) -> Result<State, RuntimeError> {
    let path = sequencer_prefix(&STATE)?;

    let is_present = host.store_has(&path)?;
    match is_present {
        None => Ok(State::default()),
        Some(_) => {
            // read the size of the state
            let size = host.store_value_size(&path)?;
            // read the according bytes
            let bytes = host.store_read(&path, 0, size)?;
            // deserialize the state
            let (_, state) = State::nom_read(&bytes).map_err(|_| RuntimeError::DecodingError)?;
            Ok(state)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{map_user_path, sequencer_prefix, write_state, USER_PREFIX_PATH};
    use crate::state::State;
    use tezos_smart_rollup_host::path::{concat, OwnedPath, RefPath};
    use tezos_smart_rollup_host::runtime::Runtime;
    use tezos_smart_rollup_mock::MockHost;

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

    #[test]
    fn test_write_state() {
        let path = RefPath::assert_from(b"/__sequencer/state");
        let state = State::Fallback;
        let mut mock_host = MockHost::default();

        write_state(&mut mock_host, state).expect("write_state should succeed");

        let is_present = mock_host.store_has(&path).expect("Store has should work");
        assert!(is_present.is_some());
    }
}

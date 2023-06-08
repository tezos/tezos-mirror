// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_host::{
    path::{concat, OwnedPath, Path, RefPath},
    runtime::RuntimeError,
    Error,
};

const SEQUENCER_PREFIX_PATH: RefPath = RefPath::assert_from(b"/__sequencer");

/// Prefix the given path by `/__sequencer`.
///
/// This function has to be used when writing/reading storage related to the sequencer kernel.
/// Then with this function, all the sequencer kernel storage should be under the path `__sequencer`.
#[allow(dead_code)]
pub fn sequencer_prefix<T: Path>(path: &T) -> Result<OwnedPath, RuntimeError> {
    concat(&SEQUENCER_PREFIX_PATH, path).map_err(|_| RuntimeError::HostErr(Error::StoreInvalidKey))
}

#[cfg(test)]
mod tests {
    use tezos_smart_rollup_host::path::RefPath;

    use super::sequencer_prefix;

    #[test]
    fn test_sequencer_prefixed() {
        let path = RefPath::assert_from(b"/a/b/c");
        let prefixed = sequencer_prefix(&path).expect("prefixing the path should work");

        assert_eq!(prefixed, RefPath::assert_from(b"/__sequencer/a/b/c").into());
    }
}

// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Important kernel store keys for transaction processors

use tezos_smart_rollup_host::path::{concat, OwnedPath, PathError, RefPath};

pub(crate) const KERNEL_PRIVATE_STATE: RefPath = RefPath::assert_from(b"/kernel/state");
pub(crate) const CACHED_MESSAGES_STORE_PREFIX: RefPath =
    RefPath::assert_from(b"/kernel/state/messages/cache");
// 32-bit unsigned LE counter
pub(crate) const PROGRESS_KEY: RefPath = RefPath::assert_from(b"/kernel/state/progress");

pub(crate) const DAC_PAYLOAD_PATH_PREFIX: RefPath = RefPath::assert_from(b"/kernel/dac/payload");

pub(crate) const DAC_ITERATOR_STATE_PREFIX: RefPath = RefPath::assert_from(b"/kernel/dac/iterator");

pub(crate) const DAL_PAYLOAD_PATH: RefPath = RefPath::assert_from(b"/kernel/dal/payload");

pub(crate) fn dac_payload_path(idx: u32) -> Result<OwnedPath, PathError> {
    concat(
        &DAC_PAYLOAD_PATH_PREFIX,
        &OwnedPath::try_from(format!("/{idx}"))?,
    )
}

pub(crate) const TRANSACTIONS_PER_KERNEL_RUN: RefPath =
    RefPath::assert_from(b"/kernel/transactions.per.kernel.run");

pub(crate) fn dac_iterator_state_path(idx: u32) -> Result<OwnedPath, PathError> {
    concat(
        &DAC_ITERATOR_STATE_PREFIX,
        &OwnedPath::try_from(format!("/{idx}"))?,
    )
}

pub(crate) fn cached_message_path(idx: u32) -> Result<OwnedPath, PathError> {
    concat(
        &CACHED_MESSAGES_STORE_PREFIX,
        &OwnedPath::try_from(format!("/{idx}"))?,
    )
}

pub(crate) fn cached_message_stage_path(idx: u32) -> Result<OwnedPath, PathError> {
    concat(&cached_message_path(idx)?, &RefPath::assert_from(b"/stage"))
}

#[cfg(test)]
mod tests {
    use tezos_smart_rollup_host::path::Path;

    use super::*;

    #[test]
    fn test_cached_message_path() {
        let path = cached_message_path(42).unwrap();
        assert_eq!(path.as_bytes(), b"/kernel/state/messages/cache/42");
    }
}

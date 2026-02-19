// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

//! Supporting functions for transaction processors

use tezos_smart_rollup_core::MAX_FILE_CHUNK_SIZE;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_host::{path::Path, runtime::RuntimeError};

pub(crate) fn read_large_store_chunk<'a, Host: StorageV1>(
    host: &mut Host,
    path: &impl Path,
    buf: &'a mut [u8],
) -> Result<&'a mut [u8], RuntimeError> {
    let mut abuf = &mut *buf;
    let mut offset = 0;
    while abuf.len() > MAX_FILE_CHUNK_SIZE {
        let bytes_read = host.store_read_slice(path, offset, &mut abuf[..MAX_FILE_CHUNK_SIZE])?;
        offset += bytes_read;
        if bytes_read < MAX_FILE_CHUNK_SIZE {
            return Ok(&mut buf[..offset]);
        }
        abuf = &mut abuf[MAX_FILE_CHUNK_SIZE..];
    }
    if !abuf.is_empty() {
        let bytes_read = host.store_read_slice(path, offset, abuf)?;
        offset += bytes_read;
    }
    Ok(&mut buf[..offset])
}

#[cfg(test)]
mod tests {
    use tezos_smart_rollup_host::path::RefPath;
    use tezos_smart_rollup_host::storage::StorageV1;
    use tezos_smart_rollup_mock::MockHost;

    use crate::{transactions::utils::read_large_store_chunk, MAX_ENVELOPE_CONTENT_SIZE};

    #[test]
    fn read_write() {
        const DATA: &[u8] = &[42u8; MAX_ENVELOPE_CONTENT_SIZE];
        const PATH: RefPath = RefPath::assert_from(b"/kernel_own_store");

        let mut host = MockHost::default();
        host.store_write_all(&PATH, DATA).unwrap();
        let mut buf = [0u8; 4096];
        let result = read_large_store_chunk(&mut host, &PATH, &mut buf).unwrap();
        assert_eq!(result, DATA);
    }
}

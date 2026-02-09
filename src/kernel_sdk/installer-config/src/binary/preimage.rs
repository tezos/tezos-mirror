// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_encoding::dac::pages::reveal_loop;
use tezos_smart_rollup_encoding::dac::pages::V0SliceContentPage;
use tezos_smart_rollup_encoding::dac::pages::MAX_PAGE_SIZE;
use tezos_smart_rollup_host::path::Path;
use tezos_smart_rollup_host::runtime::Runtime;

// Support 3 levels of hashes pages, and then bottom layer of content.
const MAX_DAC_LEVELS: usize = 4;

/// Reveal root hash
///
/// This function will reveal the root hash by writing and storing the
/// hash at the appropriate and intended path.
pub fn reveal_root_hash_to_store(
    host: &mut impl Runtime,
    root_hash: &[u8; PREIMAGE_HASH_SIZE],
    reveal_to: &impl Path,
) -> Result<(), &'static str> {
    let mut reveal_buffer = [0; MAX_PAGE_SIZE * MAX_DAC_LEVELS];

    let mut write_kernel_page = write_kernel_page(reveal_to);

    reveal_loop(
        host,
        0,
        root_hash,
        reveal_buffer.as_mut_slice(),
        MAX_DAC_LEVELS,
        &mut write_kernel_page,
    )
}

/// Appends the content of the page path given.
fn write_kernel_page<Host: Runtime>(
    reveal_to: &impl Path,
) -> impl FnMut(&mut Host, V0SliceContentPage) -> Result<(), &'static str> + '_ {
    let mut kernel_size = 0;
    move |host, page| {
        let written = append_content(host, kernel_size, page, reveal_to)?;
        kernel_size += written;
        Ok(())
    }
}

fn append_content<Host: Runtime>(
    host: &mut Host,
    kernel_size: usize,
    content: V0SliceContentPage,
    reveal_to: &impl Path,
) -> Result<usize, &'static str> {
    let content = content.as_ref();

    host.store_write(reveal_to, content, kernel_size)
        .map_err(|_| "Failed to write kernel content page")?;
    Ok(content.len())
}

#[cfg(test)]
mod test {
    use super::reveal_root_hash_to_store;
    use tezos_smart_rollup_core::MAX_FILE_CHUNK_SIZE;
    use tezos_smart_rollup_encoding::dac::{prepare_preimages, PreimageHash};
    use tezos_smart_rollup_host::path::RefPath;
    use tezos_smart_rollup_host::storage::StorageV1;
    use tezos_smart_rollup_mock::MockHost;

    const TMP_REVEAL_PATH: RefPath = RefPath::assert_from(b"/__sdk/installer/reveal");

    fn preliminary_upgrade(host: &mut MockHost) -> (PreimageHash, Vec<u8>) {
        let kernel = [1_u8; 1000];

        let save_preimages = |_hash: PreimageHash, preimage: Vec<u8>| {
            host.set_preimage(preimage);
        };
        (
            prepare_preimages(&kernel, save_preimages).unwrap(),
            kernel.to_vec(),
        )
    }

    #[test]
    fn test_reveal_root_hash_to_store() {
        let mut host = MockHost::default();
        let (root_hash, original_kernel) = preliminary_upgrade(&mut host);

        reveal_root_hash_to_store(&mut host, root_hash.as_ref(), &TMP_REVEAL_PATH)
            .expect("Reveal root hash to store should succeed.");

        let stored_kernel = host
            .store_read(&TMP_REVEAL_PATH, 0, MAX_FILE_CHUNK_SIZE)
            .expect("Stored root hash should be readable.");

        assert_eq!(stored_kernel, original_kernel)
    }
}

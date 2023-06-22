// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_core::MAX_FILE_CHUNK_SIZE;
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

    let mut size_written = 0;
    while size_written < content.len() {
        let num_to_write = usize::min(MAX_FILE_CHUNK_SIZE, content.len() - size_written);
        let bytes_to_write = &content[size_written..(size_written + num_to_write)];

        Runtime::store_write(host, reveal_to, bytes_to_write, kernel_size + size_written)
            .map_err(|_| "Failed to write kernel content page")?;

        size_written += num_to_write;
    }

    Ok(size_written)
}

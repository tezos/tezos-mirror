//! Test of reveal preimage mechanism from [encoding::dac].

use tezos_smart_rollup_encoding::dac::pages::*;
use tezos_smart_rollup_host::runtime::Runtime;

#[test]
fn encode_decode_preimages() {
    // Arrange

    // produces almost 1000 hashes - forcing a tree 2 levels deep
    const TOTAL: usize = 500_000;

    let mut data = Vec::with_capacity(TOTAL * core::mem::size_of::<usize>());
    (0..TOTAL)
        .map(usize::to_le_bytes)
        .for_each(|b| data.extend_from_slice(&b));

    let mut host = tezos_smart_rollup_mock::MockHost::default();
    let root_hash = prepare_preimages(&data, |_hash, page| {
        host.set_preimage(page);
    })
    .unwrap();

    let mut revealed = Vec::with_capacity(data.len());

    // Act
    let max_dac_levels = 4;
    let mut buffer = [0; MAX_PAGE_SIZE * 4];
    let mut save_revealed = save_content(&mut revealed);

    reveal_loop(
        &mut host,
        0,
        root_hash.as_ref(),
        &mut buffer,
        max_dac_levels,
        &mut save_revealed,
    )
    .unwrap();

    // Assert
    drop(save_revealed);
    assert_eq!(data, revealed, "Revealed different contents to original")
}

fn save_content<Host: Runtime>(
    buffer: &mut Vec<u8>,
) -> impl FnMut(&mut Host, V0SliceContentPage) -> Result<(), &'static str> + '_ {
    |_, page| {
        buffer.extend_from_slice(page.as_ref());
        Ok(())
    }
}

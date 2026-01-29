// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

//! Encoding of DAC payload as a Merkle tree with an arbitrary branching
//! factor greater or equal to 2. The serialization process works as follows:
//!
//! - A large sequence of bytes, the payload, is split into several pages
//!   of fixed size, each of which is prefixed with a small sequence
//!   of bytes (also of fixed size), which is referred to as the preamble
//!   of the page. Pages obtained directly from the original payload
//!   are referred to as `Contents pages`. Contents pages constitute the
//!   leaves of the Merkle tree being built,
//!
//! - Each contents page (each of which is a sequence of bytes consisting
//!   of the preamble followed by the actual contents from the original
//!   payload) is then hashed. The size of each hash is fixed. The hashes are
//!   concatenated together, and the resulting sequence of bytes is split
//!   into pages of the same size of `Hashes pages`, each of which is
//!   prefixed with a preamble whose size is the same as in Contents pages.
//!   Hashes pages correspond to nodes of the Merkle tree being built, and
//!   the children of a hash page are the (either Payload or Hashes) pages
//!   whose hash appear into the former,
//!
//! - Hashes pages are hashed using the same process described above, leading
//!   to a smaller list of hashes pages. To guarantee that the list of hashes
//!   pages is actually smaller than the original list of pages being hashed,
//!   we require the size of pages to be large enough to contain at least two
//!   hashes.
//!
//! Merkle tree encodings of DAC pages are versioned, to allow for multiple
//! hashing schemes to be used.

use host::runtime::{Runtime, RuntimeError};

/// Maximum size of dac pages is 4Kb.
pub const MAX_PAGE_SIZE: usize = 4096;

/// Tag size to distinguish hash/contents pages.
pub(crate) const PAGE_TAG_SIZE: usize = 1;

/// Prefix of 4-bytes to define how large contents/hash page is.
pub(crate) const PAGE_SIZE_PREFIX_SIZE: usize = 4;

/// Prefix of 5-bytes for page variant plus size.
pub(crate) const PAGE_PREFIX_SIZE: usize = PAGE_TAG_SIZE + PAGE_SIZE_PREFIX_SIZE;

/// Maximum content/hashes size that can fit in a page.
pub(crate) const MAX_USABLE_PAGE_SIZE: usize = MAX_PAGE_SIZE - PAGE_PREFIX_SIZE;

#[cfg(feature = "alloc")]
pub use encoding::{prepare_preimages, Page, V0ContentPage, V0HashPage};
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
#[cfg(feature = "alloc")]
use thiserror::Error;

/// Errors encountered while constructing a preimage hash.
#[cfg(feature = "alloc")]
#[derive(Debug, Error)]
pub enum PreimageHashError {
    /// Preimages are limited to [`MAX_PAGE_SIZE`] bytes.
    #[error("Content too large to be a preimage page")]
    InputLengthTooLarge,
}

/// Hashes `content` into a preimage hash.
///
/// Content must be at most [`MAX_PAGE_SIZE`] bytes.
#[cfg(feature = "alloc")]
pub fn make_preimage_hash(
    content: &[u8],
) -> Result<[u8; PREIMAGE_HASH_SIZE], PreimageHashError> {
    if content.len() > MAX_PAGE_SIZE {
        return Err(PreimageHashError::InputLengthTooLarge);
    }

    let hash = crypto::blake2b::digest_256(content);
    let mut root_hash: [u8; PREIMAGE_HASH_SIZE] = [0; PREIMAGE_HASH_SIZE];
    root_hash[1..].copy_from_slice(&hash);
    Ok(root_hash)
}

#[cfg(feature = "alloc")]
mod encoding {
    // TODO: <https://github.com/trilitech/tezedge/issues/17>
    //       lint triggered by issue in encoding macro
    #![allow(clippy::useless_format)]

    use super::*;
    use crate::dac::PreimageHash;
    use tezos_data_encoding::enc::BinWriter;
    use tezos_data_encoding::encoding::HasEncoding;
    use tezos_data_encoding::nom::NomReader;
    use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;

    /// A Dac page: either a leaf node of contents, or node of hashes.
    #[derive(Debug, HasEncoding, NomReader, BinWriter)]
    #[encoding(tags = "u8")]
    pub enum Page {
        /// Content Page - see [`V0ContentPage`]
        #[encoding(tag = 0)]
        V0ContentPage(V0ContentPage),

        /// Preimage Hash Page - see [`V0HashPage`]
        #[encoding(tag = 1)]
        V0HashPage(V0HashPage),
    }

    /// Content page consisting of a dynamic number of bytes.
    #[derive(Debug, HasEncoding, NomReader, BinWriter)]
    pub struct V0ContentPage {
        #[encoding(dynamic, list)]
        contents: Vec<u8>,
    }

    impl V0ContentPage {
        /// Maximum size of content in each page.
        pub const MAX_CONTENT_SIZE: usize = V0SliceContentPage::MAX_CONTENT_SIZE;

        /// Serialize an input slice into a sequence of content pages.
        pub fn new_pages(input: &[u8]) -> impl Iterator<Item = V0ContentPage> + '_ {
            input
                .chunks(Self::MAX_CONTENT_SIZE)
                .map(Vec::from)
                .map(|contents| V0ContentPage { contents })
        }
    }

    impl AsRef<[u8]> for V0ContentPage {
        fn as_ref(&self) -> &[u8] {
            self.contents.as_slice()
        }
    }

    /// Hash page consisting of a dynamic number of [`PreimageHash`].
    #[derive(Debug, HasEncoding, NomReader, BinWriter)]
    pub struct V0HashPage {
        #[encoding(dynamic, list)]
        hashes: Vec<PreimageHash>,
    }

    impl V0HashPage {
        /// Maximum number of hashes able to fit into a hash page.
        pub const MAX_HASHES_PER_PAGE: usize = V0SliceHashPage::MAX_HASHES_PER_PAGE;

        /// The list of hashes.
        pub fn hashes(&self) -> &[PreimageHash] {
            self.hashes.as_slice()
        }

        /// Serialize a hashes slice into a sequence of hash pages.
        pub fn new_pages(
            hashes: &[[u8; PREIMAGE_HASH_SIZE]],
        ) -> impl Iterator<Item = V0HashPage> + '_ {
            hashes
                .chunks(Self::MAX_HASHES_PER_PAGE)
                .map(|hashes| V0HashPage {
                    hashes: hashes.iter().map(PreimageHash::from).collect(),
                })
        }
    }

    /// Generates the preimages of the given content.
    pub fn prepare_preimages(
        content: &[u8],
        mut handle: impl FnMut(PreimageHash, Vec<u8>),
    ) -> Result<PreimageHash, PreimageHashError> {
        let mut hashes = Vec::new();

        for chunk in content.chunks(V0ContentPage::MAX_CONTENT_SIZE) {
            let page = Page::V0ContentPage(V0ContentPage {
                contents: chunk.to_vec(),
            });
            let mut encoded = Vec::new();
            page.bin_write(&mut encoded).unwrap();

            let hash: Vec<u8> = make_preimage_hash(&encoded)?.into();
            let hash = PreimageHash::from(hash);

            hashes.push(hash.clone());

            handle(hash, encoded);
        }

        while hashes.len() > 1 {
            let curr_hashes = hashes;
            hashes = Vec::new();

            for hash_page in curr_hashes.chunks(V0HashPage::MAX_HASHES_PER_PAGE) {
                let hash_page = Page::V0HashPage(V0HashPage {
                    hashes: hash_page.to_vec(),
                });
                let mut encoded = Vec::new();
                hash_page.bin_write(&mut encoded).unwrap();

                let hash: Vec<u8> = make_preimage_hash(&encoded)?.into();
                let hash = PreimageHash::from(hash);

                hashes.push(hash.clone());

                handle(hash, encoded);
            }
        }

        Ok(hashes.remove(0))
    }
}

/// Errors that may occur when dealing with [SlicePage].
#[derive(Debug)]
pub enum SlicePageError {
    /// Unknown page tag.
    InvalidTag(Option<u8>),
    /// Invalid size prefix.
    InvalidSizePrefix,
}

/// A Dac [Page] that borrows the underlying buffer.
///
/// Can be used in `no_std` & `alloc`-free environments.
#[derive(Debug)]
pub enum SlicePage<'a> {
    /// Contents of borrowed bytes.
    V0ContentPage(V0SliceContentPage<'a>),
    /// Contents of borrowed hashes.
    V0HashPage(V0SliceHashPage<'a>),
}

impl<'a> TryFrom<&'a [u8]> for SlicePage<'a> {
    type Error = SlicePageError;

    fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
        match value {
            [0, rest @ ..] => {
                Ok(SlicePage::V0ContentPage(V0SliceContentPage::parse(rest)?))
            }
            [1, rest @ ..] => Ok(SlicePage::V0HashPage(V0SliceHashPage::parse(rest)?)),
            _ => Err(SlicePageError::InvalidTag(value.first().cloned())),
        }
    }
}

/// Borrowing version of [V0ContentPage].
#[derive(Debug)]
pub struct V0SliceContentPage<'a> {
    inner: &'a [u8],
}

impl<'a> V0SliceContentPage<'a> {
    /// Maximum size of content in each page.
    pub const MAX_CONTENT_SIZE: usize = MAX_USABLE_PAGE_SIZE;

    // Assumes magic byte has been discarded
    fn parse(slice: &'a [u8]) -> Result<Self, SlicePageError> {
        if slice.len() < 4 {
            return Err(SlicePageError::InvalidSizePrefix);
        }

        let size = u32::from_be_bytes([slice[0], slice[1], slice[2], slice[3]]) as usize;

        let end_offset = 4 + size;

        if slice.len() < end_offset {
            return Err(SlicePageError::InvalidSizePrefix);
        }

        Ok(Self {
            inner: &slice[4..end_offset],
        })
    }
}

impl<'a> AsRef<[u8]> for V0SliceContentPage<'a> {
    fn as_ref(&self) -> &'a [u8] {
        self.inner
    }
}

/// Borrowing version of [V0HashPage].
#[derive(Debug)]
pub struct V0SliceHashPage<'a> {
    // Guaranteed to be a multiple of PREIMAGE_HASH_SIZE
    pub(crate) inner: &'a [u8],
}

impl<'a> V0SliceHashPage<'a> {
    /// Maximum number of hashes able to fit into a hash page.
    pub const MAX_HASHES_PER_PAGE: usize = MAX_USABLE_PAGE_SIZE / PREIMAGE_HASH_SIZE;

    /// Returns an iterator over the preimage hashes contained within.
    pub fn hashes(&self) -> impl Iterator<Item = &'a [u8; PREIMAGE_HASH_SIZE]> {
        // there is a nightly(only) API called `as_chunks` that would return
        // `(&[[u8; PREIMAPREIMAGE_HASH_SIZE]], &[u8])` that we could use in
        // future
        self.inner
            .chunks_exact(PREIMAGE_HASH_SIZE)
            .map(|chunk| chunk.try_into().expect("Guaranteed to be exact."))
    }

    // Assumes magic byte has been discarded
    fn parse(slice: &'a [u8]) -> Result<Self, SlicePageError> {
        if slice.len() < 4 {
            return Err(SlicePageError::InvalidSizePrefix);
        }

        let size = u32::from_be_bytes([slice[0], slice[1], slice[2], slice[3]]) as usize;

        let end_offset = 4 + size; // for prefix bytes

        if slice.len() < end_offset || size % PREIMAGE_HASH_SIZE != 0 {
            return Err(SlicePageError::InvalidSizePrefix);
        }

        Ok(Self {
            inner: &slice[4..end_offset],
        })
    }
}

/// Fetches a page of data from file `hash` into `buffer` using [Runtime::reveal_preimage].
/// Returns a tuple of the raw page and remaining buffer.
pub fn fetch_page_raw<'a, Host: Runtime>(
    host: &Host,
    hash: &[u8; PREIMAGE_HASH_SIZE],
    buffer: &'a mut [u8],
) -> Result<(&'a [u8], &'a mut [u8]), RuntimeError> {
    let size = Runtime::reveal_preimage(host, hash, buffer)?;
    let (page, rest) = buffer.split_at_mut(size);

    Ok((page, rest))
}

/// Recursively traverses a Merkle Tree of hashes up to `max_dac_levels` depth where each hash
/// corresponds to a preimage that can be revealed via [Runtime::reveal_preimage]. The closure
/// `save_content` is applied on each content page found.
///
/// N.B `max_dac_levels`, should probably be kept under 3 (3 gives 59MB of data). You are likely,
/// however, to run into tick-limit issues while doing so, depending on what `save_content` does.
///
/// Instead, it is recommended to use `DacCertificate::reveal_to_store` where possible.
pub fn reveal_loop<Host: Runtime>(
    host: &mut Host,
    level: usize,
    hash: &[u8; PREIMAGE_HASH_SIZE],
    rest: &mut [u8],
    max_dac_levels: usize,
    save_content: &mut impl FnMut(&mut Host, V0SliceContentPage) -> Result<(), &'static str>,
) -> Result<(), &'static str> {
    if level >= max_dac_levels {
        return Err("DAC preimage tree contains too many levels.");
    }

    let (page, rest) =
        fetch_page_raw(host, hash, rest).map_err(|_| "Failed to retrieve preimage")?;

    let page = SlicePage::try_from(page)
        .map_err(|_| "Unable to decode DAC page: Decode into SlicePage failed")?;
    match page {
        SlicePage::V0HashPage(hashes) => {
            for hash in hashes.hashes() {
                reveal_loop(host, level + 1, hash, rest, max_dac_levels, save_content)?;
            }
        }
        SlicePage::V0ContentPage(content) => save_content(host, content)?,
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_data_encoding::enc::BinWriter;
    use tezos_data_encoding::nom::NomReader;

    // taken from DAC test example in tezos
    const EXAMPLE_CONTENT_PAGE: &[u8] = &[
        0, 0, 0, 0, b'A', b'L', b'o', b'r', b'e', b'm', b' ', b'i', b'p', b's', b'u',
        b'm', b' ', b'd', b'o', b'l', b'o', b'r', b' ', b's', b'i', b't', b' ', b'a',
        b'm', b'e', b't', b',', b' ', b'c', b'o', b'n', b's', b'e', b'c', b't', b'e',
        b't', b'u', b'r', b' ', b'a', b'd', b'i', b'p', b'i', b's', b'c', b'i', b'n',
        b'g', b' ', b'e', b'l', b'i', b't', b',', b' ', b's', b'e', b'd', b' ', b'd',
        b'o', b' ', b'e',
    ];

    // taken from DAC test example in tezos
    const EXAMPLE_HASH_PAGE: &[u8] = &[
        1, 0, 0, 0, b'B', 0, b'r', 180, b'a', b'2', b'Z', b'(', 220, 14, 4, 220, b'{',
        b'N', b'n', b'@', 183, b'#', b'!', 6, b'm', 204, b'p', 130, 162, 247, 246, 16,
        b'l', 239, b'7', b'"', 249, 163, 0, 155, 167, 146, 19, 175, 28, b'V', 129, 247,
        208, 31, b'F', b'd', 183, 194, 149, b'H', 163, b'|', 246, 164, 201, b'&', 195,
        129, 24, 3, b'}', b'4', b't', 11, 213,
    ];

    #[test]
    fn encode_decode_hash_page() {
        let (_, page) =
            Page::nom_read(EXAMPLE_HASH_PAGE).expect("Deserialization should work");
        let mut buffer = Vec::new();
        page.bin_write(&mut buffer)
            .expect("Serialization should work");
        assert_eq!(buffer.as_slice(), EXAMPLE_HASH_PAGE);
    }

    #[test]
    fn encode_decode_contents_page() {
        let (_, page) =
            Page::nom_read(EXAMPLE_CONTENT_PAGE).expect("Deserialization should work");
        let mut buffer = Vec::new();
        page.bin_write(&mut buffer)
            .expect("Serialization should work");
        assert_eq!(buffer.as_slice(), EXAMPLE_CONTENT_PAGE);
    }

    #[test]
    fn decoding_contents_over_slice() {
        let (_, page) =
            Page::nom_read(EXAMPLE_CONTENT_PAGE).expect("Deserialization should work");

        let slice_page =
            SlicePage::try_from(EXAMPLE_CONTENT_PAGE).expect("Should be content page");

        match (&page, &slice_page) {
            (Page::V0ContentPage(page), SlicePage::V0ContentPage(slice_page)) => {
                assert_eq!(page.as_ref(), slice_page.inner)
            }
            _ => panic!("Should be content pages, got: {page:?} & {slice_page:?}",),
        }
    }

    #[test]
    fn decoding_hash_over_slice() {
        let (_, page) =
            Page::nom_read(EXAMPLE_HASH_PAGE).expect("Deserialization should work");

        let slice_page =
            SlicePage::try_from(EXAMPLE_HASH_PAGE).expect("Should be hash page");

        match (&page, &slice_page) {
            (Page::V0HashPage(page), SlicePage::V0HashPage(slice_page)) => {
                let hashes: Vec<&[u8; PREIMAGE_HASH_SIZE]> =
                    page.hashes().iter().map(|hash| hash.as_ref()).collect();
                let slice_hashes: Vec<&[u8; PREIMAGE_HASH_SIZE]> =
                    slice_page.hashes().collect();

                assert_eq!(hashes, slice_hashes);
            }
            _ => panic!("Should be content pages, got: {page:?} & {slice_page:?}",),
        }
    }
}

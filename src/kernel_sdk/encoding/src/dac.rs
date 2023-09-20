// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Defines dac related types and functions.

#[cfg(feature = "crypto")]
pub mod certificate;
pub mod pages;

#[deprecated = "These items have moved, please import from dac::pages instead"]
pub use pages::*;

#[cfg(feature = "alloc")]
#[doc(inline)]
pub use self::alloc::PreimageHash;

#[cfg(feature = "alloc")]
mod alloc {

    use tezos_data_encoding::enc::BinWriter;
    use tezos_data_encoding::encoding::HasEncoding;
    use tezos_data_encoding::nom::NomReader;
    use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
    /// A 33-byte hash corresponding to a preimage.
    #[derive(Eq, PartialEq, Debug, HasEncoding, NomReader, BinWriter, Clone, Hash)]
    pub struct PreimageHash {
        #[encoding(sized = "PREIMAGE_HASH_SIZE", bytes)]
        hash: Vec<u8>,
    }

    impl AsRef<[u8; PREIMAGE_HASH_SIZE]> for PreimageHash {
        fn as_ref(&self) -> &[u8; PREIMAGE_HASH_SIZE] {
            self.hash
                .as_slice()
                .try_into()
                .expect("Must be PREIMAGE_HASH_SIZE")
        }
    }

    impl From<&[u8; PREIMAGE_HASH_SIZE]> for PreimageHash {
        fn from(hash: &[u8; PREIMAGE_HASH_SIZE]) -> Self {
            let hash = hash.as_slice().to_vec();

            Self { hash }
        }
    }

    impl From<Vec<u8>> for PreimageHash {
        fn from(hash: Vec<u8>) -> Self {
            Self { hash }
        }
    }

    impl From<PreimageHash> for Vec<u8> {
        fn from(val: PreimageHash) -> Self {
            val.hash
        }
    }

    impl From<PreimageHash> for [u8; PREIMAGE_HASH_SIZE] {
        fn from(value: PreimageHash) -> Self {
            value.hash.try_into().unwrap_or_else(|_| {
                unreachable!("Impossible to happen since it's been smart-constructed")
            })
        }
    }
}

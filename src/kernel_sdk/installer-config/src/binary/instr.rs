// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_host::path::RefPath;

#[derive(Debug, PartialEq, Eq)]
pub struct RefBytes<'a>(pub &'a [u8]);

impl<'a> AsRef<[u8]> for RefBytes<'a> {
    fn as_ref(&self) -> &[u8] {
        self.0
    }
}

#[allow(clippy::from_over_into)]
impl<'a> Into<[u8; PREIMAGE_HASH_SIZE]> for RefBytes<'a> {
    fn into(self) -> [u8; PREIMAGE_HASH_SIZE] {
        self.0.try_into().unwrap()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct MoveInstruction<Path> {
    pub from: Path,
    pub to: Path,
}

// Value dependent instructions start here

#[derive(Debug, PartialEq, Eq)]
pub struct RevealInstruction<Path, Bytes> {
    pub hash: Bytes,
    pub to: Path,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ConfigInstruction<Path, Bytes> {
    Reveal(RevealInstruction<Path, Bytes>),
    Move(MoveInstruction<Path>),
}

pub type RefConfigInstruction<'a> = ConfigInstruction<RefPath<'a>, RefBytes<'a>>;

#[cfg(feature = "alloc")]
pub mod owned {
    use tezos_smart_rollup_encoding::dac::PreimageHash;
    use tezos_smart_rollup_host::path::{OwnedPath, PathError};
    use thiserror::Error;

    use super::*;

    #[derive(Debug, PartialEq, Eq)]
    pub struct OwnedBytes(pub Vec<u8>);

    impl AsRef<[u8]> for OwnedBytes {
        fn as_ref(&self) -> &[u8] {
            &self.0
        }
    }

    pub type OwnedConfigInstruction = ConfigInstruction<OwnedPath, OwnedBytes>;

    #[derive(Debug, PartialEq, Eq)]
    pub struct OwnedConfigProgram(pub Vec<OwnedConfigInstruction>);

    #[derive(Debug, Error, PartialEq)]
    pub enum RevealInstrError {
        #[error("Invalid preimage hash size: {0}")]
        InvalidRevealHashSize(usize),
        #[error("Invalid reveal path: {0}")]
        PathError(PathError),
    }

    impl OwnedConfigInstruction {
        pub fn reveal_instr(hash: PreimageHash, to: OwnedPath) -> Self {
            OwnedConfigInstruction::Reveal(RevealInstruction {
                hash: OwnedBytes(hash.into()),
                to,
            })
        }

        pub fn move_instr(from: OwnedPath, to: OwnedPath) -> Self {
            OwnedConfigInstruction::Move(MoveInstruction { from, to })
        }
    }
}

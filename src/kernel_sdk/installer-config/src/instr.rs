// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_host::path::RefPath;

#[derive(Debug, PartialEq, Eq)]
pub struct RawBytes<'a>(pub &'a [u8]);

#[allow(clippy::from_over_into)]
impl<'a> Into<[u8; PREIMAGE_HASH_SIZE]> for RawBytes<'a> {
    fn into(self) -> [u8; PREIMAGE_HASH_SIZE] {
        self.0.try_into().unwrap()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct MoveInstruction<'a> {
    pub from: RefPath<'a>,
    pub to: RefPath<'a>,
}

// Value dependent instructions start here

#[derive(Debug, PartialEq, Eq)]
pub struct RevealInstruction<'a> {
    pub hash: RawBytes<'a>,
    pub to: RefPath<'a>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ConfigInstruction<'a> {
    Reveal(RevealInstruction<'a>),
    Move(MoveInstruction<'a>),
}

impl<'a> ConfigInstruction<'a> {
    pub fn reveal_instr(
        hash: &'a [u8; PREIMAGE_HASH_SIZE],
        to: RefPath<'a>,
    ) -> ConfigInstruction<'a> {
        ConfigInstruction::Reveal(RevealInstruction {
            hash: RawBytes(hash),
            to,
        })
    }

    pub fn move_instr(from: RefPath<'a>, to: RefPath<'a>) -> ConfigInstruction<'a> {
        ConfigInstruction::Move(MoveInstruction { from, to })
    }
}

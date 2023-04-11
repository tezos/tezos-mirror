// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pub const BLOCK_HASH_SIZE: usize = 32;
pub type BlockHash = [u8; BLOCK_HASH_SIZE];

pub type L2Level = u64;
pub type OwnedHash = Vec<u8>;
pub type Hash<'a> = &'a Vec<u8>;

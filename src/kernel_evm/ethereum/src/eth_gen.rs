// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pub type RawTransaction = Vec<u8>;
pub type RawTransactions = Vec<RawTransaction>;
pub type L2Level = u64;
pub type Quantity = u64;
pub type OwnedHash = Vec<u8>;
pub type Hash<'a> = &'a Vec<u8>;

// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use clarity::Transaction as EthTransaction;

pub const TRANSACTION_HASH_SIZE: usize = 32;
pub type TransactionHash = [u8; TRANSACTION_HASH_SIZE];

pub const BLOCK_HASH_SIZE: usize = 32;
pub type BlockHash = [u8; BLOCK_HASH_SIZE];

pub type RawTransaction = EthTransaction;
pub type RawTransactions = Vec<RawTransaction>;
pub type L2Level = u64;
pub type Quantity = u64;
pub type OwnedHash = Vec<u8>;
pub type Hash<'a> = &'a Vec<u8>;

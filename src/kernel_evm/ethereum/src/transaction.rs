// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use clarity::Transaction as EthTransaction;

pub const TRANSACTION_HASH_SIZE: usize = 32;
pub type TransactionHash = [u8; TRANSACTION_HASH_SIZE];

pub type RawTransaction = EthTransaction;
pub type RawTransactions = Vec<RawTransaction>;

// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::{custom, Error};
use primitive_types::{H160, H256, U256 as PU256};
use revm::primitives::{Address, U256};
#[cfg(test)]
use rlp::RlpEncodable;
use rlp::{Decodable, Rlp, RlpDecodable};

#[derive(Debug, RlpDecodable, Default)]
#[cfg_attr(test, derive(RlpEncodable))]
pub struct FaDepositWithProxy {
    pub amount: PU256,
    pub receiver: H160,
    // If proxy doesn't have code it will still be used as
    // ticket owner.
    pub proxy: H160,
    pub ticket_hash: H256,
    pub inbox_level: u32,
    pub inbox_msg_id: u32,
}

impl FaDepositWithProxy {
    pub(crate) fn from_raw(raw_deposit: Vec<u8>) -> Result<Self, Error> {
        FaDepositWithProxy::decode(&Rlp::new(&raw_deposit)).map_err(custom)
    }
}

pub fn u256_to_alloy(value: &PU256) -> U256 {
    let mut bytes = [0u8; 32];
    value.to_little_endian(&mut bytes);
    U256::from_le_bytes::<32>(bytes)
}

pub fn h160_to_alloy(value: &H160) -> Address {
    Address::from_slice(&value.to_fixed_bytes())
}

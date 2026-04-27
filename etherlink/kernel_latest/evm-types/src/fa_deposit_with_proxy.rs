// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::{H160, H256, U256 as PU256};
use rlp::{Decodable, DecoderError, Rlp, RlpDecodable, RlpEncodable};

// DEV: This struct uses deprecated types and encoding
// It is only here for backwards compatibility
// Remove when represented with revm primitives
#[derive(Debug, Eq, PartialEq, Clone, RlpEncodable, RlpDecodable, Default)]
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
    pub fn from_raw(raw_deposit: Vec<u8>) -> Result<Self, DecoderError> {
        FaDepositWithProxy::decode(&Rlp::new(&raw_deposit))
    }
}

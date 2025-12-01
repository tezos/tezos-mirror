// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::{H160, H256};
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpStream};

use crate::rlp_helpers::{decode_field, decode_list, next};

/// Access list item used to specify addresses
/// which are being accessed during a contract invocation.
/// For more information see `<https://eips.ethereum.org/EIPS/eip-2930>`.
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "evaluation", derive(serde::Deserialize))]
#[cfg_attr(feature = "evaluation", serde(rename_all = "camelCase"))]
pub struct AccessListItem {
    /// Address of the contract invoked during execution
    pub address: H160,
    /// Keys in the contract's storage accessed during contract execution
    pub storage_keys: Vec<H256>,
}

impl Encodable for AccessListItem {
    fn rlp_append(&self, s: &mut RlpStream) {
        s.begin_list(2);
        s.append(&self.address);
        s.append_list(&self.storage_keys);
    }
}

impl Decodable for AccessListItem {
    fn decode(rlp: &Rlp) -> Result<Self, DecoderError> {
        if !rlp.is_list() {
            Err(DecoderError::RlpExpectedToBeList)
        } else {
            let mut it = rlp.iter();
            let address: H160 = decode_field(&next(&mut it)?, "address")?;
            let storage_keys: Vec<H256> = decode_list(&next(&mut it)?, "storage_keys")?;
            if it.next().is_some() {
                return Err(DecoderError::RlpIncorrectListLen);
            }
            Ok(Self {
                address,
                storage_keys,
            })
        }
    }
}

pub type AccessList = Vec<AccessListItem>;

pub fn empty_access_list() -> AccessList {
    vec![]
}

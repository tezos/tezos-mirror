// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use revm::primitives::{alloy_primitives::Keccak256, Address};
use rlp::{Decodable, Encodable, Rlp};
use tezos_crypto_rs::public_key_hash::PublicKeyHash;
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_host::{
    path::{OwnedPath, RefPath},
    runtime::RuntimeError,
};

use crate::{helpers::storage::concat, Error};

// Path where is stored the correspondance between an EVM address and the native
// Tezos account it was derived from.
const ETHEREUM_ADDRESS_MAPPING_PATH: RefPath =
    RefPath::assert_from(b"/evm/world_state/eth_accounts/tezosx/native/ethereum");

pub fn ethereum_address_from_tezos(pub_key_hash: &PublicKeyHash) -> Address {
    let mut hasher = Keccak256::new();
    hasher.update(pub_key_hash.to_b58check().as_bytes());
    let hash = hasher.finalize();
    Address::from_slice(&hash[0..20])
}

const TEZOS_SRC_ADDR_TAG: u8 = 1;
#[derive(PartialEq, Debug, Clone, Eq)]
pub enum ForeignAddress {
    Tezos(PublicKeyHash),
}

impl Encodable for ForeignAddress {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        match &self {
            ForeignAddress::Tezos(pub_key_hash) => {
                stream.append(&TEZOS_SRC_ADDR_TAG);
                stream.append(&pub_key_hash.to_b58check().as_bytes());
            }
        }
    }
}

impl Decodable for ForeignAddress {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        if !decoder.is_list() {
            return Err(rlp::DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 2 {
            return Err(rlp::DecoderError::RlpIncorrectListLen);
        }
        let tag: u8 = decoder.at(0)?.as_val()?;
        let pub_key_hash_decoder = decoder.at(1)?;
        match tag {
            TEZOS_SRC_ADDR_TAG => {
                let vec: Vec<u8> = pub_key_hash_decoder.as_val()?;
                let s: String = String::from_utf8(vec).map_err(|_| {
                    rlp::DecoderError::Custom("Invalid public key hash (not a string)")
                })?;
                let pub_key_hash = PublicKeyHash::from_b58check(&s).map_err(|_| {
                    rlp::DecoderError::Custom("Invalid public key hash (b58check)")
                })?;
                Ok(Self::Tezos(pub_key_hash))
            }
            _ => Err(rlp::DecoderError::Custom("Unknown address mapping tag.")),
        }
    }
}

fn path_to_ethereum_address_mapping(address: &Address) -> Result<OwnedPath, Error> {
    let address = address.to_string().to_lowercase();
    let address_path: Vec<u8> = format!("/{address}").into();
    let address_path =
        OwnedPath::try_from(address_path).map_err(|e| Error::Custom(e.to_string()))?;
    concat(&ETHEREUM_ADDRESS_MAPPING_PATH, &address_path)
        .map_err(|e| Error::Custom(e.to_string()))
}

#[allow(dead_code)]
pub fn get_ethereum_address_mapping(
    host: &impl Runtime,
    address: &Address,
) -> Result<Option<ForeignAddress>, Error> {
    let path = path_to_ethereum_address_mapping(address)
        .map_err(|_| RuntimeError::PathNotFound)?;
    match host.store_read_all(&path) {
        Ok(bytes) => {
            let source_address = ForeignAddress::decode(&Rlp::new(&bytes))
                .map_err(|_| RuntimeError::DecodingError)?;
            Ok(Some(source_address))
        }
        Err(RuntimeError::PathNotFound) => Ok(None),
        Err(err) => Err(Error::Runtime(err)),
    }
}

#[allow(dead_code)]
pub fn set_ethereum_address_mapping(
    host: &mut impl Runtime,
    address: &Address,
    source_address: ForeignAddress,
) -> Result<(), Error> {
    let path = path_to_ethereum_address_mapping(address)
        .map_err(|_| RuntimeError::PathNotFound)?;
    let value = &source_address.rlp_bytes();
    Ok(host.store_write_all(&path, value)?)
}

// SPDX-FileCopyrightText: 2022 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

//! Representation of Ethereum addresses
//!
//! We need to support Ethereum addresses for compatibility, so that
//! we can read Ethereum transactions, etc.
//!
//! Additionally, some addresses have special meaning - for example
//! locations of precompiled contracts or contract creation.
use primitive_types::{H160, H256};
use rlp::{Decodable, DecoderError, Encodable, Rlp};

/// An address of an EVM contract
///
/// This should be compatible with the Ethereum addresses.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd)]
pub struct EthereumAddress(H160);

impl EthereumAddress {
    /// Get an address from unsigned 64-bit integer, big endian.
    pub fn from_u64_be(v: u64) -> Self {
        EthereumAddress(H160::from_low_u64_be(v))
    }

    /// Get an address from a byte array, address if 20 bytes, 0 if no bytes, else error
    pub fn from_slice(data: &[u8]) -> Result<EthereumAddress, DecoderError> {
        if data.len() == 20 {
            Ok(EthereumAddress(H160::from_slice(data)))
        } else if data.is_empty() {
            Ok(EthereumAddress(H160::zero()))
        } else {
            Err(DecoderError::Custom("Wrong size for Address"))
        }
    }
}

impl From<String> for EthereumAddress {
    /// Decode a transaction in hex format. Unsafe, to be used only in tests : panics when fails
    fn from(e: String) -> Self {
        let data = &hex::decode(e).unwrap();
        EthereumAddress::from_slice(data).unwrap()
    }
}

impl Decodable for EthereumAddress {
    fn decode(decoder: &Rlp<'_>) -> Result<EthereumAddress, DecoderError> {
        let data = decoder.data()?;
        EthereumAddress::from_slice(data)
    }
}

impl Encodable for EthereumAddress {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        if !self.0.is_zero() {
            s.append(&self.0);
        } else {
            // we should make the distinction between 0 and null
            // but we don't, null is encoded as 0
            // which is a pb, as the 0x0 address can bu used, to burn stuff
            // TODO: https://gitlab.com/tezos/tezos/-/issues/5075
            s.append_empty_data();
        }
    }
}

#[allow(clippy::from_over_into)]
impl Into<H160> for EthereumAddress {
    fn into(self) -> H160 {
        self.0
    }
}

impl From<H160> for EthereumAddress {
    fn from(addr: H160) -> Self {
        Self(addr)
    }
}
impl From<EthereumAddress> for String {
    fn from(e: EthereumAddress) -> Self {
        format!("{:x}", e.0)
    }
}

impl From<[u8; 20]> for EthereumAddress {
    fn from(v: [u8; 20]) -> Self {
        Self(v.into())
    }
}

// Use case: coerce a value from the EVM execution stack into an address. This
// is needed for eg contract self destruct (beneficiary address is at top of
// stack and encoded as H256).
impl From<H256> for EthereumAddress {
    fn from(_v: H256) -> Self {
        todo!("See issue: https://gitlab.com/tezos/tezos/-/issues/4902")
    }
}

/// The address for contract used for creating contracts
///
/// Ethereum has a set of precompiled/special contracts. Creating
/// contracts is implemented as one such contract.
#[allow(dead_code)]
const CREATE_CONTRACT: EthereumAddress = EthereumAddress(H160::zero());

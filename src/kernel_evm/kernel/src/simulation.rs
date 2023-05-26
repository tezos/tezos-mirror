// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::{H160, U256};
use rlp::{Decodable, DecoderError, Rlp};
use tezos_ethereum::rlp_helpers::{decode_field, decode_option, next};

// https://ethereum.org/en/developers/docs/apis/json-rpc/#eth_call
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Simulation {
    /// (optional) The address the transaction is sent from.
    pub from: Option<H160>,
    /// The address the transaction is directed to.
    pub to: H160,
    /// (optional) Integer of the gas provided for the transaction execution.
    /// eth_call consumes zero gas, but this parameter may be needed by some
    /// executions.
    pub gas: Option<u64>,
    /// (optional) Integer of the gasPrice used for each paid gas
    pub gas_price: Option<u64>,
    /// (optional) Integer of the value sent with this transaction (in Wei)
    pub value: Option<U256>,
    /// (optional) Hash of the method signature and encoded parameters.
    pub data: Vec<u8>,
}

impl Simulation {
    /// Unserialize bytes as RLP encoded data.
    pub fn from_rlp_bytes(bytes: &[u8]) -> Result<Simulation, DecoderError> {
        let decoder = Rlp::new(bytes);
        Simulation::decode(&decoder)
    }
}

impl Decodable for Simulation {
    fn decode(decoder: &Rlp<'_>) -> Result<Self, DecoderError> {
        if decoder.is_list() {
            if Ok(6) == decoder.item_count() {
                let mut it = decoder.iter();
                let from: Option<H160> = decode_option(&next(&mut it)?, "from")?;
                let to: H160 = decode_field(&next(&mut it)?, "to")?;
                let gas: Option<u64> = decode_option(&next(&mut it)?, "gas")?;
                let gas_price: Option<u64> = decode_option(&next(&mut it)?, "gas_price")?;
                let value: Option<U256> = decode_option(&next(&mut it)?, "value")?;
                let data: Vec<u8> = decode_field(&next(&mut it)?, "data")?;
                Ok(Self {
                    from,
                    to,
                    gas,
                    gas_price,
                    value,
                    data,
                })
            } else {
                Err(DecoderError::RlpIncorrectListLen)
            }
        } else {
            Err(DecoderError::RlpExpectedToBeList)
        }
    }
}

impl TryFrom<&[u8]> for Simulation {
    type Error = DecoderError;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        Self::from_rlp_bytes(bytes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl Simulation {
        /// Unserialize an hex string as RLP encoded data.
        pub fn from_rlp(e: String) -> Result<Simulation, DecoderError> {
            let tx = hex::decode(e)
                .or(Err(DecoderError::Custom("Couldn't parse hex value")))?;
            Self::from_rlp_bytes(&tx)
        }
    }

    fn address_of_str(s: &str) -> H160 {
        let data = &hex::decode(s).unwrap();
        H160::from_slice(data)
    }

    #[test]
    fn test_decode_empty() {
        let input_string =
            "da8094353535353535353535353535353535353535353580808080".to_string();
        let address = address_of_str("3535353535353535353535353535353535353535");
        let expected = Simulation {
            from: None,
            to: address,
            gas: None,
            gas_price: None,
            value: None,
            data: vec![],
        };

        let simulation = Simulation::from_rlp(input_string);

        assert!(simulation.is_ok(), "Simulation input should be decodable");
        assert_eq!(
            expected,
            simulation.unwrap(),
            "The decoded result is not the one expected"
        );
    }

    #[test]
    fn test_decode_non_empty() {
        let input_string =
            "f6942424242424242424242424242424242424242424943535353535353535353535353535353535353535822b678256ce828235821616".to_string();
        let to = address_of_str("3535353535353535353535353535353535353535");
        let from = Some(address_of_str("2424242424242424242424242424242424242424"));
        let data = hex::decode("1616").unwrap();
        let expected = Simulation {
            from,
            to,
            gas: Some(11111),
            gas_price: Some(22222),
            value: Some(U256::from(33333)),
            data,
        };

        let simulation = Simulation::from_rlp(input_string);

        assert!(simulation.is_ok(), "Simulation input should be decodable");
        assert_eq!(
            expected,
            simulation.unwrap(),
            "The decoded result is not the one expected"
        );
    }
}

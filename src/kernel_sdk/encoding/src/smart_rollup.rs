// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2022 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Hash of Layer1 smart rollup addresses.

use std::fmt::Display;

use tezos_data_encoding::enc::BinWriter;
use tezos_data_encoding::encoding::HasEncoding;
use tezos_data_encoding::nom::NomReader;

use crypto::base58::{FromBase58Check, FromBase58CheckError};
use crypto::hash::{HashType, SmartRollupHash};

/// Smart rollup address
#[derive(Debug, Clone, PartialEq, Eq, HasEncoding, BinWriter, NomReader)]
pub struct SmartRollupAddress {
    /// Address hash
    hash: SmartRollupHash,
}

impl Display for SmartRollupAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.hash)
    }
}

impl SmartRollupAddress {
    /// Conversion from base58-encoding string (with sr1 prefix).
    pub fn from_b58check(data: &str) -> Result<Self, FromBase58CheckError> {
        let bytes = data.from_base58check()?;

        if bytes.starts_with(HashType::SmartRollupHash.base58check_prefix()) {
            let hash = SmartRollupHash::from_base58_check(data)?;
            Ok(SmartRollupAddress { hash })
        } else {
            Err(FromBase58CheckError::InvalidBase58)
        }
    }

    /// Conversion to base58-encoding string (with sr1 prefix).
    pub fn to_b58check(&self) -> String {
        self.hash.to_base58_check()
    }

    /// Instantiate new SmartRollupAddress
    pub fn new(hash: SmartRollupHash) -> Self {
        Self { hash }
    }

    /// Returns internal `hash`
    pub fn hash(&self) -> &SmartRollupHash {
        &self.hash
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sr1_b58check() {
        let sr1 = "sr1UNDWPUYVeomgG15wn5jSw689EJ4RNnVQa";

        let smart_rollup_address = SmartRollupAddress::from_b58check(sr1);

        assert!(smart_rollup_address.is_ok());

        let sr1_from_smart_rollup_address = smart_rollup_address.unwrap().to_b58check();

        assert_eq!(sr1, &sr1_from_smart_rollup_address);
    }

    #[test]
    fn sr1_encoding() {
        let sr1 = "sr1UXY5i5Z1sF8xd8ZUyzur827MAaFWREzvj";

        let smart_rollup_address =
            SmartRollupAddress::from_b58check(sr1).expect("expected valid sr1 hash");

        let mut bin = Vec::new();
        smart_rollup_address
            .bin_write(&mut bin)
            .expect("serialization should work");

        let (remaining_input, deserde_sr1) = SmartRollupAddress::nom_read(bin.as_slice())
            .expect("deserialization should work");

        assert!(remaining_input.is_empty());
        assert_eq!(smart_rollup_address, deserde_sr1);
    }
}

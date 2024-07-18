// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Definitions relating to Layer-1 accounts, which the kernel may interact with.

use crypto::base58::{FromBase58Check, FromBase58CheckError};
use crypto::hash::{ContractKt1Hash, Hash, HashTrait, HashType};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::map;
use nom::sequence::delimited;
use nom::sequence::preceded;
use tezos_data_encoding::enc::{self, BinResult, BinWriter};
use tezos_data_encoding::encoding::{Encoding, HasEncoding};
use tezos_data_encoding::has_encoding;
use tezos_data_encoding::nom::{NomReader, NomResult};

use super::public_key_hash::PublicKeyHash;

#[cfg(feature = "testing")]
pub mod testing;

/// Contract id - of either an implicit account or originated account.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Contract {
    /// User account
    Implicit(PublicKeyHash),
    /// Smart contract account
    Originated(ContractKt1Hash),
}

impl Contract {
    /// Converts from a *base58-encoded* string, checking for the prefix.
    pub fn from_b58check(data: &str) -> Result<Self, FromBase58CheckError> {
        let bytes = data.from_base58check()?;
        match bytes {
            _ if bytes.starts_with(HashType::ContractKt1Hash.base58check_prefix()) => {
                Ok(Self::Originated(ContractKt1Hash::from_b58check(data)?))
            }
            _ => Ok(Self::Implicit(PublicKeyHash::from_b58check(data)?)),
        }
    }

    /// Converts to a *base58-encoded* string, including the prefix.
    pub fn to_b58check(&self) -> String {
        match self {
            Self::Implicit(pkh) => pkh.to_b58check(),
            Self::Originated(kt1) => kt1.to_b58check(),
        }
    }
}

impl From<Contract> for Hash {
    fn from(c: Contract) -> Self {
        match c {
            Contract::Implicit(pkh) => pkh.into(),
            Contract::Originated(ckt1) => ckt1.into(),
        }
    }
}

impl TryFrom<String> for Contract {
    type Error = FromBase58CheckError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Contract::from_b58check(value.as_str())
    }
}

#[allow(clippy::from_over_into)]
impl Into<String> for Contract {
    fn into(self) -> String {
        self.to_b58check()
    }
}

has_encoding!(Contract, CONTRACT_ENCODING, { Encoding::Custom });

impl NomReader for Contract {
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        alt((
            map(
                preceded(tag([0]), PublicKeyHash::nom_read),
                Contract::Implicit,
            ),
            map(
                delimited(tag([1]), ContractKt1Hash::nom_read, tag([0])),
                Contract::Originated,
            ),
        ))(input)
    }
}

impl BinWriter for Contract {
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        match self {
            Self::Implicit(implicit) => {
                enc::put_byte(&0, output);
                BinWriter::bin_write(implicit, output)
            }
            Self::Originated(originated) => {
                enc::put_byte(&1, output);
                let mut bytes: Hash = originated.as_ref().to_vec();
                // Originated is padded
                bytes.push(0);
                enc::bytes(&mut bytes, output)?;
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn tz1_b58check() {
        let tz1 = "tz1RjtZUVeLhADFHDL8UwDZA6vjWWhojpu5w";

        let pkh = Contract::from_b58check(tz1);

        assert!(matches!(
            pkh,
            Ok(Contract::Implicit(PublicKeyHash::Ed25519(_)))
        ));

        let tz1_from_pkh = pkh.unwrap().to_b58check();

        assert_eq!(tz1, &tz1_from_pkh);
    }

    #[test]
    fn tz2_b58check() {
        let tz2 = "tz2VGBaXuS6rnaa5hpC92qkgadRJKdEbeGwc";

        let pkh = Contract::from_b58check(tz2);

        assert!(matches!(
            pkh,
            Ok(Contract::Implicit(PublicKeyHash::Secp256k1(_)))
        ));

        let tz2_from_pkh = pkh.unwrap().to_b58check();

        assert_eq!(tz2, &tz2_from_pkh);
    }

    #[test]
    fn tz3_b58check() {
        let tz3 = "tz3WEJYwJ6pPwVbSL8FrSoAXRmFHHZTuEnMA";

        let pkh = Contract::from_b58check(tz3);

        assert!(matches!(
            pkh,
            Ok(Contract::Implicit(PublicKeyHash::P256(_)))
        ));

        let tz3_from_pkh = pkh.unwrap().to_b58check();

        assert_eq!(tz3, &tz3_from_pkh);
    }

    #[test]
    fn kt1_b58check() {
        let kt1 = "KT1BuEZtb68c1Q4yjtckcNjGELqWt56Xyesc";

        let pkh = Contract::from_b58check(kt1);

        assert!(matches!(pkh, Ok(Contract::Originated(ContractKt1Hash(_)))));

        let kt1_from_pkh = pkh.unwrap().to_b58check();

        assert_eq!(kt1, &kt1_from_pkh);
    }

    #[test]
    fn tz1_encoding() {
        let tz1 = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";

        let contract = Contract::from_b58check(tz1).expect("expected valid tz1 hash");

        let mut bin = Vec::new();
        contract
            .bin_write(&mut bin)
            .expect("serialization should work");

        let deserde_contract = NomReader::nom_read(bin.as_slice())
            .expect("deserialization should work")
            .1;

        check_implicit_serialized(&bin, tz1);

        assert_eq!(contract, deserde_contract);
    }

    #[test]
    fn tz2_encoding() {
        let tz2 = "tz2JmrN5LtfkYZFCQnWQtwpd9u7Fq3Dc4n6E";

        let contract = Contract::from_b58check(tz2).expect("expected valid tz2 hash");

        let mut bin = Vec::new();
        contract
            .bin_write(&mut bin)
            .expect("serialization should work");

        let deserde_contract = NomReader::nom_read(bin.as_slice())
            .expect("deserialization should work")
            .1;

        check_implicit_serialized(&bin, tz2);

        assert_eq!(contract, deserde_contract);
    }

    #[test]
    fn tz3_encoding() {
        let tz3 = "tz3gKfNk1UgCKXd21gBVba5Z9kqY8m6J2g1n";

        let contract = Contract::from_b58check(tz3).expect("expected valid tz3 hash");

        let mut bin = Vec::new();
        contract
            .bin_write(&mut bin)
            .expect("serialization should work");

        let deserde_contract = NomReader::nom_read(bin.as_slice())
            .expect("deserialization should work")
            .1;

        check_implicit_serialized(&bin, tz3);

        assert_eq!(contract, deserde_contract);
    }

    // Check encoding of originated contracts (aka smart-contract addresses)
    #[test]
    fn contract_encode_originated() {
        let test = "KT1BuEZtb68c1Q4yjtckcNjGELqWt56Xyesc";
        let mut expected = vec![1];
        let mut bytes = Contract::from_b58check(test).unwrap().into();
        expected.append(&mut bytes);
        expected.push(0); // padding

        let contract = Contract::from_b58check(test).unwrap();

        let mut bin = Vec::new();
        contract.bin_write(&mut bin).unwrap();

        assert_eq!(expected, bin);
    }

    // Check decoding of originated contracts (aka smart-contract addresses)
    #[test]
    fn contract_decode_originated() {
        let expected = "KT1BuEZtb68c1Q4yjtckcNjGELqWt56Xyesc";
        let mut test = vec![1];
        let mut bytes = Contract::from_b58check(expected).unwrap().into();
        test.append(&mut bytes);
        test.push(0); // padding

        let expected_contract = Contract::from_b58check(expected).unwrap();

        let (remaining_input, contract) = Contract::nom_read(test.as_slice()).unwrap();

        assert!(remaining_input.is_empty());
        assert_eq!(expected_contract, contract);
    }

    // Check that serialization of implicit PublicKeyHash is binary compatible
    // with protocol encoding of implicit contract ids.
    fn check_implicit_serialized(contract_bytes: &[u8], address: &str) {
        let mut bin_pkh = Vec::new();
        PublicKeyHash::from_b58check(address)
            .expect("expected valid implicit contract")
            .bin_write(&mut bin_pkh)
            .expect("serialization should work");

        assert!(matches!(
            contract_bytes,
            [0_u8, rest @ ..] if rest == bin_pkh));
    }
}

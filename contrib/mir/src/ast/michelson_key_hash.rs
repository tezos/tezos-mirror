/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

//! Representation for typed Michelson `key_hash` values.

use tezos_crypto_rs::hash::{
    ContractTz1Hash, ContractTz2Hash, ContractTz3Hash, ContractTz4Hash, Hash, HashTrait,
};

use super::{ByteReprError, ByteReprTrait};

macro_rules! key_hash_type_and_impls {
    ($($(#[$meta:meta])* $con:ident($ty:ident)),* $(,)*) => {
        /// Public key hash. Public key hashes are used to represent implicit
        /// Tezos addresses.
        #[derive(Debug, Clone, Eq, PartialOrd, Ord, PartialEq, Hash)]
        pub enum KeyHash {
            $($(#[$meta])* $con($ty)),*
        }

        $(impl From<$ty> for KeyHash {
            fn from(value: $ty) -> Self {
                KeyHash::$con(value)
            }
        })*

        impl AsRef<[u8]> for KeyHash {
            fn as_ref(&self) -> &[u8] {
                match self {
                    $(KeyHash::$con($ty(h)))|* => h,
                }
            }
        }

        impl From<KeyHash> for Vec<u8> {
            fn from(value: KeyHash) -> Self {
                match value {
                    $(KeyHash::$con($ty(h)))|* => h,
                }
            }
        }
    };
}

key_hash_type_and_impls! {
    /// A hash of a Ed25519 public key, `tz1...` in base58-check encoding.
    Tz1(ContractTz1Hash),
    /// A hash of a SecP256k1 public key, `tz2...` in base58-check encoding.
    Tz2(ContractTz2Hash),
    /// A hash of a P256 public key, `tz3...` in base58-check encoding.
    Tz3(ContractTz3Hash),
    /// A hash of a BLS public key, `tz4...` in base58-check encoding.
    Tz4(ContractTz4Hash),
}

impl TryFrom<&[u8]> for KeyHash {
    type Error = ByteReprError;
    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        Self::from_bytes(value)
    }
}

impl TryFrom<&str> for KeyHash {
    type Error = ByteReprError;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::from_base58_check(value)
    }
}

fn check_size(data: &[u8], expected_size: usize, name: &str) -> Result<(), ByteReprError> {
    let size = data.len();
    if size != expected_size {
        Err(ByteReprError::WrongFormat(format!(
            "key_hash must be {expected_size} {name} long, but it is {size} {name} long"
        )))
    } else {
        Ok(())
    }
}

const TAG_TZ1: u8 = 0;
const TAG_TZ2: u8 = 1;
const TAG_TZ3: u8 = 2;
const TAG_TZ4: u8 = 3;

impl KeyHash {
    /// Size of the hash in bytes.
    /// All hashes are blake2b 160-bit hashes.
    pub const HASH_SIZE: usize = 20;
    /// Size of the representation size in bytes.
    /// Corresponds to [Self::HASH_SIZE] + 1, where 1 byte is used for a tag.
    pub const BYTE_SIZE: usize = Self::HASH_SIZE + 1; // hash size + tag size
    /// Byte size of a base58-check encoded `key_hash`.
    pub const BASE58_SIZE: usize = 36;
}

impl ByteReprTrait for KeyHash {
    fn from_base58_check(data: &str) -> Result<Self, ByteReprError> {
        use KeyHash::*;

        check_size(data.as_bytes(), Self::BASE58_SIZE, "characters")?;

        Ok(match &data[0..3] {
            "tz1" => Tz1(HashTrait::from_b58check(data)?),
            "tz2" => Tz2(HashTrait::from_b58check(data)?),
            "tz3" => Tz3(HashTrait::from_b58check(data)?),
            "tz4" => Tz4(HashTrait::from_b58check(data)?),
            s => return Err(ByteReprError::UnknownPrefix(s.to_owned())),
        })
    }

    fn from_bytes(bytes: &[u8]) -> Result<Self, ByteReprError> {
        use KeyHash::*;

        check_size(bytes, Self::BYTE_SIZE, "bytes")?;
        Ok(match bytes[0] {
            TAG_TZ1 => Tz1(HashTrait::try_from_bytes(&bytes[1..])?),
            TAG_TZ2 => Tz2(HashTrait::try_from_bytes(&bytes[1..])?),
            TAG_TZ3 => Tz3(HashTrait::try_from_bytes(&bytes[1..])?),
            TAG_TZ4 => Tz4(HashTrait::try_from_bytes(&bytes[1..])?),
            _ => {
                return Err(ByteReprError::UnknownPrefix(format!(
                    "0x{}",
                    hex::encode(&bytes[..1])
                )))
            }
        })
    }

    fn to_base58_check(&self) -> String {
        use KeyHash::*;
        match self {
            Tz1(hash) => hash.to_base58_check(),
            Tz2(hash) => hash.to_base58_check(),
            Tz3(hash) => hash.to_base58_check(),
            Tz4(hash) => hash.to_base58_check(),
        }
    }

    fn to_bytes(&self, out: &mut Vec<u8>) {
        use KeyHash::*;
        fn go(out: &mut Vec<u8>, tag: u8, hash: impl AsRef<Hash>) {
            out.push(tag);
            out.extend_from_slice(hash.as_ref());
        }
        match self {
            Tz1(hash) => go(out, TAG_TZ1, hash),
            Tz2(hash) => go(out, TAG_TZ2, hash),
            Tz3(hash) => go(out, TAG_TZ3, hash),
            Tz4(hash) => go(out, TAG_TZ4, hash),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_base58_to_bin() {
        for (b58, hex) in FIXTURES {
            assert_eq!(
                KeyHash::from_base58_check(b58).unwrap().to_bytes_vec(),
                hex::decode(hex).unwrap(),
            );
        }
    }

    #[test]
    fn test_bin_to_base58() {
        // unknown tag
        assert_eq!(
            dbg!(KeyHash::from_bytes(
                &hex::decode("ff7b09f782e0bcd67739510afa819d85976119d5ef").unwrap()
            )),
            Err(ByteReprError::UnknownPrefix("0xff".to_owned())),
        );

        for (b58, hex) in FIXTURES {
            assert_eq!(
                &KeyHash::from_bytes(&hex::decode(hex).unwrap())
                    .unwrap()
                    .to_base58_check(),
                b58,
            );
        }
    }

    // binary representation produced by running
    //
    // `octez-client --mode mockup normalize data ... of type key_hash --unparsing-mode Optimized`
    const FIXTURES: &[(&str, &str)] = &[
        (
            "tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw",
            "002422090f872dfd3a39471bb23f180e6dfed030f3",
        ),
        (
            "tz1SNL5w4RFRbCWRMB4yDWvoRQrPQxZmNzeQ",
            "0049d0be8c2987e04e080f4d73cbe24d8bf83997e2",
        ),
        (
            "tz1V8fDHpHzN8RrZqiYCHaJM9EocsYZch5Cy",
            "00682343b6fe7589573e11db2b87fd206b936e2a79",
        ),
        (
            "tz1WPGZjP9eHGqD9DkiRJ1xGRU1wEMY19AAF",
            "0075deb97789e2429f2b9bb5dba1b1e4a061e832a3",
        ),
        (
            "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j",
            "007b09f782e0bcd67739510afa819d85976119d5ef",
        ),
        (
            "tz1hHGTh6Yk4k7d2PiTcBUeMvw6fJCFikedv",
            "00ed6586813c9085c8b6252ec3a654ee0e36a0f0e2",
        ),
        (
            "tz29EDhZ4D3XueHxm5RGZsJLHRtj3qSA2MzH",
            "010a053e3d8b622a993d3182e3f6cc5638ff5f12fe",
        ),
        (
            "tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r",
            "025cfa532f50de3e12befc0ad21603835dd7698d35",
        ),
        (
            "tz4J46gb6DxDFYxkex8k9sKiYZwjuiaoNSqN",
            "036342f30484dd46b6074373aa6ddca9dfb70083d6",
        ),
    ];
}

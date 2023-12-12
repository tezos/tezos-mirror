/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

pub mod address_hash;
pub mod entrypoint;

pub use self::address_hash::AddressHash;
pub use self::entrypoint::Entrypoint;

use address_hash::check_size;

use super::{ByteReprError, ByteReprTrait};

#[derive(Debug, Clone, Eq, PartialOrd, Ord, PartialEq)]
pub struct Address {
    pub hash: AddressHash,
    pub entrypoint: Entrypoint,
}

impl Address {
    pub fn is_default_ep(&self) -> bool {
        self.entrypoint.is_default()
    }
}

impl ByteReprTrait for Address {
    fn from_base58_check(data: &str) -> Result<Self, ByteReprError> {
        let (hash, ep) = if let Some(ep_sep_pos) = data.find('%') {
            (&data[..ep_sep_pos], &data[ep_sep_pos + 1..])
        } else {
            (data, "")
        };
        Ok(Address {
            hash: AddressHash::from_base58_check(hash)?,
            entrypoint: Entrypoint::try_from(ep)?,
        })
    }

    fn from_bytes(bytes: &[u8]) -> Result<Self, ByteReprError> {
        check_size(bytes, AddressHash::BYTE_SIZE, "bytes")?;

        let (hash, ep) = bytes.split_at(AddressHash::BYTE_SIZE);
        Ok(Address {
            hash: AddressHash::from_bytes(hash)?,
            entrypoint: Entrypoint::try_from(ep)?,
        })
    }

    fn to_bytes(&self, out: &mut Vec<u8>) {
        self.hash.to_bytes(out);
        if !self.is_default_ep() {
            out.extend_from_slice(self.entrypoint.as_bytes())
        }
    }

    fn to_base58_check(&self) -> String {
        if self.is_default_ep() {
            self.hash.to_base58_check()
        } else {
            format!(
                "{}%{}",
                self.hash.to_base58_check(),
                self.entrypoint.as_str()
            )
        }
    }
}

impl TryFrom<&[u8]> for Address {
    type Error = ByteReprError;
    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        Self::from_bytes(value)
    }
}

impl TryFrom<&str> for Address {
    type Error = ByteReprError;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::from_base58_check(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_base58_to_bin() {
        // address with explicit, but empty, entrypoint
        assert_eq!(
            Address::from_base58_check("tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw%")
                .unwrap()
                .to_bytes_vec(),
            hex::decode("00002422090f872dfd3a39471bb23f180e6dfed030f3").unwrap(),
        );

        // address with explicit default entrypoint
        assert_eq!(
            Address::from_base58_check("tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw%default")
                .unwrap()
                .to_bytes_vec(),
            hex::decode("00002422090f872dfd3a39471bb23f180e6dfed030f3").unwrap(),
        );

        for (b58, hex) in FIXTURES {
            assert_eq!(
                Address::from_base58_check(b58).unwrap().to_bytes_vec(),
                hex::decode(hex).unwrap(),
            );
        }
    }

    #[test]
    fn test_bin_to_base58() {
        // explicit default entrypoint is apparently forbidden in binary encoding
        assert!(matches!(
            Address::from_bytes(
                &hex::decode("00007b09f782e0bcd67739510afa819d85976119d5ef64656661756c74").unwrap()
            ),
            Err(ByteReprError::WrongFormat(_)),
        ));

        // unknown implicit tag
        assert_eq!(
            dbg!(Address::from_bytes(
                &hex::decode("00ff7b09f782e0bcd67739510afa819d85976119d5ef").unwrap()
            )),
            Err(ByteReprError::UnknownPrefix("0xff".to_owned())),
        );

        // unknown tag
        assert_eq!(
            Address::from_bytes(
                &hex::decode("ffff7b09f782e0bcd67739510afa819d85976119d5ef").unwrap()
            ),
            Err(ByteReprError::UnknownPrefix("0xff".to_owned())),
        );

        for (b58, hex) in FIXTURES {
            assert_eq!(
                Address::from_bytes(&hex::decode(hex).unwrap())
                    .unwrap()
                    .to_base58_check(),
                b58,
            );
        }
    }

    // binary representation produced by running
    //
    // `octez-client --mode mockup run script 'parameter address; storage unit;
    // code { CAR; FAILWITH }' on storage Unit and input "\"$addr\""`
    const FIXTURES: [(&str, &str); 25] = [
        (
            "tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw",
            "00002422090f872dfd3a39471bb23f180e6dfed030f3",
        ),
        (
            "tz1SNL5w4RFRbCWRMB4yDWvoRQrPQxZmNzeQ",
            "000049d0be8c2987e04e080f4d73cbe24d8bf83997e2",
        ),
        (
            "tz1V8fDHpHzN8RrZqiYCHaJM9EocsYZch5Cy",
            "0000682343b6fe7589573e11db2b87fd206b936e2a79",
        ),
        (
            "tz1WPGZjP9eHGqD9DkiRJ1xGRU1wEMY19AAF",
            "000075deb97789e2429f2b9bb5dba1b1e4a061e832a3",
        ),
        (
            "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%bar",
            "00007b09f782e0bcd67739510afa819d85976119d5ef626172",
        ),
        (
            "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%defauls",
            "00007b09f782e0bcd67739510afa819d85976119d5ef64656661756c73",
        ),
        (
            "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j",
            "00007b09f782e0bcd67739510afa819d85976119d5ef",
        ),
        (
            "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%defaulu",
            "00007b09f782e0bcd67739510afa819d85976119d5ef64656661756c75",
        ),
        (
            "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%foo",
            "00007b09f782e0bcd67739510afa819d85976119d5ef666f6f",
        ),
        (
            "tz1hHGTh6Yk4k7d2PiTcBUeMvw6fJCFikedv",
            "0000ed6586813c9085c8b6252ec3a654ee0e36a0f0e2",
        ),
        (
            "tz29EDhZ4D3XueHxm5RGZsJLHRtj3qSA2MzH%bar",
            "00010a053e3d8b622a993d3182e3f6cc5638ff5f12fe626172",
        ),
        (
            "tz29EDhZ4D3XueHxm5RGZsJLHRtj3qSA2MzH",
            "00010a053e3d8b622a993d3182e3f6cc5638ff5f12fe",
        ),
        (
            "tz29EDhZ4D3XueHxm5RGZsJLHRtj3qSA2MzH%foo",
            "00010a053e3d8b622a993d3182e3f6cc5638ff5f12fe666f6f",
        ),
        (
            "tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r%bar",
            "00025cfa532f50de3e12befc0ad21603835dd7698d35626172",
        ),
        (
            "tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r",
            "00025cfa532f50de3e12befc0ad21603835dd7698d35",
        ),
        (
            "tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r%foo",
            "00025cfa532f50de3e12befc0ad21603835dd7698d35666f6f",
        ),
        (
            "tz4J46gb6DxDFYxkex8k9sKiYZwjuiaoNSqN%bar",
            "00036342f30484dd46b6074373aa6ddca9dfb70083d6626172",
        ),
        (
            "tz4J46gb6DxDFYxkex8k9sKiYZwjuiaoNSqN",
            "00036342f30484dd46b6074373aa6ddca9dfb70083d6",
        ),
        (
            "tz4J46gb6DxDFYxkex8k9sKiYZwjuiaoNSqN%foo",
            "00036342f30484dd46b6074373aa6ddca9dfb70083d6666f6f",
        ),
        (
            "KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye%bar",
            "011f2d825fdd9da219235510335e558520235f4f5400626172",
        ),
        (
            "KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye",
            "011f2d825fdd9da219235510335e558520235f4f5400",
        ),
        (
            "KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye%foo",
            "011f2d825fdd9da219235510335e558520235f4f5400666f6f",
        ),
        (
            "sr1RYurGZtN8KNSpkMcCt9CgWeUaNkzsAfXf%bar",
            "03d601f22256d2ad1faec0c64374e527c6e62f2e5a00626172",
        ),
        (
            "sr1RYurGZtN8KNSpkMcCt9CgWeUaNkzsAfXf",
            "03d601f22256d2ad1faec0c64374e527c6e62f2e5a00",
        ),
        (
            "sr1RYurGZtN8KNSpkMcCt9CgWeUaNkzsAfXf%foo",
            "03d601f22256d2ad1faec0c64374e527c6e62f2e5a00666f6f",
        ),
    ];
}

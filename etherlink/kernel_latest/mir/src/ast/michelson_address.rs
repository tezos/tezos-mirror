// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! Representation for typed Michelson `address` values.

pub mod address_hash;

pub use self::address_hash::AddressHash;
pub use tezos_protocol::entrypoint::Entrypoint;

use address_hash::check_size;

use super::{ByteReprError, ByteReprTrait};

/// Tezos address with an entrypoint, e.g.
/// `KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi%foo`.
#[derive(Debug, Clone, Eq, PartialOrd, Ord, PartialEq)]
pub struct Address {
    /// The hash part of the address, i.e. the part before `%`.
    pub hash: AddressHash,
    /// The entrypoint part of the address, i.e. the part after `%`.
    pub entrypoint: Entrypoint,
}

/// Cheap placeholder address (default hash, default entrypoint), used when
/// moving an `Address` out of a `&mut` field without cloning.
impl Default for Address {
    fn default() -> Self {
        Address {
            hash: AddressHash::default(),
            entrypoint: Entrypoint::default(),
        }
    }
}

impl Address {
    /// Returns `true` if the address uses the default entrypoint. Note that
    /// addresses that don't explicitly specify the entrypoint, e.g.
    /// `KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi`, implicitly use the default one.
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
        // NB: an `address` literal's entrypoint suffix is *not* a script-source
        // entrypoint annotation; it must follow L1's address-value rule
        // (length <= 31, not `default`) — not the Michelson annotation charset
        // regex. See `Entrypoint::from_address_str` and L1's `parse_address`
        // / `Entrypoint.of_string_strict` (script_ir_translator.ml,
        // entrypoint_repr.ml).
        Ok(Address {
            hash: AddressHash::from_base58_check(hash)?,
            entrypoint: Entrypoint::from_address_str(ep)?,
        })
    }

    fn from_bytes(bytes: &[u8]) -> Result<Self, ByteReprError> {
        check_size(bytes, AddressHash::BYTE_SIZE, "bytes")?;

        let (hash, ep) = bytes.split_at(AddressHash::BYTE_SIZE);
        // NB: the trailing bytes of an optimized `address` value are *not* a
        // script-source entrypoint annotation; they must follow L1's
        // address-value rule (length <= 31, not `default`) — not the Michelson
        // annotation charset regex. See `Entrypoint::from_address_bytes` and
        // L1's `parse_address` / `Entrypoint.value_encoding`.
        Ok(Address {
            hash: AddressHash::from_bytes(hash)?,
            entrypoint: Entrypoint::from_address_bytes(ep)?,
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
            // The entrypoint name is raw bytes (see `Entrypoint`); it is valid
            // UTF-8 for every entrypoint expressible as a Michelson string
            // literal. A non-UTF-8 entrypoint can only be produced by `UNPACK
            // address` on crafted bytes and has no readable Michelson form, so
            // the lossy conversion here is never observed for a printable
            // value.
            format!(
                "{}%{}",
                self.hash.to_base58_check(),
                String::from_utf8_lossy(self.entrypoint.as_bytes())
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

        // L2-1377: address with explicit "default" entrypoint is now
        // rejected (matches L1's `parse_address` /
        // `Entrypoint.of_string_strict`). The implicit form (no `%`) and
        // the empty-suffix form (`%`) still decode to the default
        // entrypoint, as above and below.
        assert!(matches!(
            Address::from_base58_check("tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw%default"),
            Err(ByteReprError::WrongFormat(_)),
        ));

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
                &hex::decode(
                    "00007b09f782e0bcd67739510afa819d85976119d5ef64656661756c74"
                )
                .unwrap()
            ),
            Err(ByteReprError::WrongFormat(_)),
        ));

        // unknown implicit tag
        assert_eq!(
            dbg!(Address::from_bytes(
                &hex::decode("00ff7b09f782e0bcd67739510afa819d85976119d5ef").unwrap()
            )),
            Err(ByteReprError::WrongFormat(
                "public key hash : Error decoding bytes [1..21] caused by invalid tag `0xFF`"
                    .into()
            )),
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

    /// L2-1377: an `address` literal whose entrypoint suffix uses bytes
    /// outside the Michelson script-source entrypoint annotation charset
    /// must still be accepted on the address-value path (both readable
    /// and binary), matching Tezos L1's `parse_address`.
    #[test]
    fn test_l2_1377_relaxed_entrypoint_charset() {
        // -- Readable path (PUSH address "KT1...%!"). --

        // Single non-charset ASCII byte ("!", 0x21).
        let addr = Address::from_base58_check("tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%!")
            .expect("L1 accepts %!, MIR must too");
        assert_eq!(addr.entrypoint.as_str(), Some("!"));

        // Forbidden-as-first-char byte (".") used as first char.
        let addr =
            Address::from_base58_check("tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%.foo")
                .expect("L1 accepts %.foo, MIR must too");
        assert_eq!(addr.entrypoint.as_str(), Some(".foo"));

        // Trailing non-charset byte ("foo!").
        let addr =
            Address::from_base58_check("tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%foo!")
                .expect("L1 accepts %foo!, MIR must too");
        assert_eq!(addr.entrypoint.as_str(), Some("foo!"));

        // -- Binary path (UNPACK address). --

        // address hash (22 bytes) + "!" (0x21).
        assert_eq!(
            Address::from_bytes(
                &hex::decode("00007b09f782e0bcd67739510afa819d85976119d5ef21").unwrap()
            )
            .expect("L1 accepts 0x21 as ep, MIR must too")
            .entrypoint
            .as_str(),
            Some("!"),
        );

        // address hash + ".foo".
        assert_eq!(
            Address::from_bytes(
                &hex::decode("00007b09f782e0bcd67739510afa819d85976119d5ef2e666f6f")
                    .unwrap()
            )
            .expect("L1 accepts .foo as ep, MIR must too")
            .entrypoint
            .as_str(),
            Some(".foo"),
        );

        // address hash + 0xff: a non-UTF-8 entrypoint byte (`ep_nonascii`
        // from the issue). L1 stores the entrypoint name as a raw byte
        // string and accepts this; MIR must too. There is no readable
        // Michelson form, so it is only reachable on the binary path.
        let addr = Address::from_bytes(
            &hex::decode("00007b09f782e0bcd67739510afa819d85976119d5efff").unwrap(),
        )
        .expect("L1 accepts a 0xff entrypoint byte, MIR must too");
        assert_eq!(addr.entrypoint.as_bytes(), &[0xff_u8]);
        assert_eq!(addr.entrypoint.as_str(), None);

        // Max-length (31) "weird" entrypoint round-trips on both paths.
        let mut hex_with_bang =
            String::from("00007b09f782e0bcd67739510afa819d85976119d5ef");
        for _ in 0..31 {
            hex_with_bang.push_str("21"); // '!' x 31
        }
        let addr = Address::from_bytes(&hex::decode(&hex_with_bang).unwrap())
            .expect("31 bytes of '!' is at the length bound and must be accepted");
        assert_eq!(addr.entrypoint.as_str(), Some("!".repeat(31).as_str()));
    }

    /// L2-1377: the address-value path must keep enforcing the L1 rules
    /// it shares — length <= 31 and not-default-by-name.
    #[test]
    fn test_l2_1377_kept_restrictions() {
        // length > 31 is still rejected on both paths (matches L1's
        // shared length bound).
        assert!(matches!(
            Address::from_base58_check(
                format!("tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%{}", "q".repeat(32))
                    .as_str()
            ),
            Err(ByteReprError::WrongFormat(_))
        ));
        let mut hex_too_long =
            String::from("00007b09f782e0bcd67739510afa819d85976119d5ef");
        for _ in 0..32 {
            hex_too_long.push_str("71");
        }
        assert!(matches!(
            Address::from_bytes(&hex::decode(&hex_too_long).unwrap()),
            Err(ByteReprError::WrongFormat(_))
        ));

        // Explicit "default" remains forbidden on the binary path.
        assert!(matches!(
            Address::from_bytes(
                &hex::decode(
                    "00007b09f782e0bcd67739510afa819d85976119d5ef64656661756c74"
                )
                .unwrap()
            ),
            Err(ByteReprError::WrongFormat(_))
        ));
        // Explicit "default" remains forbidden on the readable path.
        assert!(matches!(
            Address::from_base58_check("tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%default"),
            Err(ByteReprError::WrongFormat(_))
        ));
    }

    /// L2-1377: an empty entrypoint suffix and a missing `%` separator
    /// must both map to the implicit default entrypoint, on both paths.
    #[test]
    fn test_l2_1377_default_canonicalization() {
        // Readable: no `%` -> default.
        assert!(
            Address::from_base58_check("tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j")
                .unwrap()
                .is_default_ep()
        );
        // Readable: trailing `%` with empty suffix -> default.
        assert!(
            Address::from_base58_check("tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%")
                .unwrap()
                .is_default_ep()
        );
        // Binary: no trailing entrypoint bytes -> default.
        assert!(Address::from_bytes(
            &hex::decode("00007b09f782e0bcd67739510afa819d85976119d5ef").unwrap()
        )
        .unwrap()
        .is_default_ep());
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

// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::Error;
/*
Parameters of exported functions need to implement the `Lift<UniFfiTag>` trait.
Only uniffi primitive types and a few other types implement it:
- `Option<T>` and `Vec<T>` implement it if `T` also implements it.
- `HashMap<K, V>` implements it if `K` and `V` also implement it.
- `Arc<T>` implements it if `T` implements the `FfiConverterArc<UniFfiTag>` trait.
- enums deriving `uniffi::Enum` and structs deriving `uniffi::Record` implement it.

struct deriving `uniffi::Object` does not implement the `Lift<UniFfiTag>` trait.
But since it implements `FfiConverterArc<UniFfiTag>` trait, using Arc<> allows it to be derived.
So to pass a struct deriving `uniffi::Object` as optional in an exported function parameter, an Arc<T> is required:
```
#[derive(uniffi::Object)]
pub struct MyStruct;

#[uniffi::export]
pub fn my_function(my_struct: Option<Arc<MyStruct>>) -> ...
```
Notice that `&MyStruct` can also be passed as parameters of exported functions because it implements the `LiftRef<UniFfiTag>` trait.
This does not help for optional struct deriving `uniffi::Object` because `Option<T>` implements this trait only if `T` implements the `Lift<UniFfiTag>` trait.
*/
use std::sync::Arc;
use tezos_data_encoding::enc;

#[derive(Debug, uniffi::Error, thiserror::Error)]
#[uniffi(flat_error)]
pub enum ForgingError {
    #[error("Binary conversion failure: {0:?}")]
    ToBytes(#[from] enc::BinError),
}

/// Forge an arbitrary message into a payload that can be signed.
///
/// The input message should be of a particular format - see
/// <https://taquito.io/docs/signing/#generating-a-signature-with-beacon-sdk>.
///
/// The forged message is represented as a Michelson payload - specifically a Michelson String.
///
/// This method will fail if `msg.len() > u32::MAX`. For all reasonable use-cases, this method should
/// always succeed.
// TODO: https://linear.app/tezos/issue/SDK-68
//       MIR already defines features for forging
#[uniffi::export]
pub fn forge_message(msg: &str) -> Result<Vec<u8>, Error> {
    let mut out = Vec::<u8>::new();
    enc::put_byte(&0x05, &mut out); // Tag for Packed Micheline
    enc::put_byte(&0x01, &mut out); // Tag for String Micheline
    enc::string(msg, &mut out).map_err(|err| Error::Forge(ForgingError::ToBytes(err)))?;
    Ok(out)
}

pub mod operation {
    use super::*;
    use crate::keys::{BlsSignature, PublicKey, PublicKeyHash};
    use crate::Error;
    use tezos_data_encoding::enc::BinWriter;
    use tezos_protocol::operation::{
        DelegationContent, ManagerOperationContent, OperationContent, OriginationContent,
        RevealContent, Script,
    };

    #[derive(uniffi::Object, Debug)]
    pub struct Reveal {
        pub source: PublicKeyHash,
        pub fee: u64,
        pub counter: u64,
        pub gas_limit: u64,
        pub storage_limit: u64,
        pub public_key: PublicKey,
        pub proof: Option<BlsSignature>,
    }

    #[uniffi::export]
    impl Reveal {
        #[doc = "Build a reveal operation."]
        #[uniffi::constructor(default(proof = None))]
        pub fn new(
            source: &PublicKeyHash,
            fee: u64,
            counter: u64,
            gas_limit: u64,
            storage_limit: u64,
            public_key: &PublicKey,
            proof: Option<Arc<BlsSignature>>,
        ) -> Self {
            Self {
                source: source.clone(),
                fee,
                counter,
                gas_limit,
                storage_limit,
                public_key: public_key.clone(),
                proof: proof.map(|proof| proof.as_ref().clone()),
            }
        }

        #[doc = "Forge the operation."]
        pub fn forge(&self) -> Result<Vec<u8>, Error> {
            let reveal = OperationContent::Reveal(ManagerOperationContent {
                source: self.source.0.clone(),
                fee: self.fee.into(),
                counter: self.counter.into(),
                gas_limit: self.gas_limit.into(),
                storage_limit: self.storage_limit.into(),
                operation: RevealContent {
                    pk: self.public_key.0.clone(),
                    proof: self.proof.clone().map(|proof| proof.0),
                },
            });
            reveal
                .to_bytes()
                .map_err(|err| Error::Forge(ForgingError::ToBytes(err)))
        }
    }

    #[derive(uniffi::Object, Debug)]
    pub struct Origination {
        pub source: PublicKeyHash,
        pub fee: u64,
        pub counter: u64,
        pub gas_limit: u64,
        pub storage_limit: u64,
        pub balance: u64,
        pub delegate: Option<PublicKeyHash>,
        // Its script is always as follows:
        // "script": {
        //   "code": [],
        //   "storage": {
        //     "prim": "unit"
        //   }
        // }
    }

    #[uniffi::export]
    impl Origination {
        #[doc = "Build a origination operation."]
        #[uniffi::constructor]
        pub fn new(
            source: &PublicKeyHash,
            fee: u64,
            counter: u64,
            gas_limit: u64,
            storage_limit: u64,
            balance: u64,
            delegate: Option<Arc<PublicKeyHash>>,
        ) -> Self {
            Self {
                source: source.clone(),
                fee,
                counter,
                gas_limit,
                storage_limit,
                balance,
                delegate: delegate.map(|delegate| delegate.as_ref().clone()),
            }
        }

        #[doc = "Forge the operation."]
        pub fn forge(&self) -> Result<Vec<u8>, Error> {
            let origination = OperationContent::Origination(ManagerOperationContent {
                source: self.source.0.clone(),
                fee: self.fee.into(),
                counter: self.counter.into(),
                gas_limit: self.gas_limit.into(),
                storage_limit: self.storage_limit.into(),
                operation: OriginationContent {
                    balance: self.balance.into(),
                    delegate: self.delegate.clone().map(|delegate| delegate.0),
                    script: Script {
                        // Seq(&[]).encode(),
                        code: vec![0x02, 0x00, 0x00, 0x00, 0x00],
                        // Micheline::App(Prim::unit, &[], NO_ANNS).encode(),
                        storage: vec![0x03, 0x6c],
                    },
                },
            });
            origination
                .to_bytes()
                .map_err(|err| Error::Forge(ForgingError::ToBytes(err)))
        }
    }

    #[derive(uniffi::Object, Debug)]
    pub struct Delegation {
        pub source: PublicKeyHash,
        pub fee: u64,
        pub counter: u64,
        pub gas_limit: u64,
        pub storage_limit: u64,
        pub delegate: Option<PublicKeyHash>,
    }

    #[uniffi::export]
    impl Delegation {
        #[doc = "Build a delegation operation."]
        #[uniffi::constructor]
        pub fn new(
            source: &PublicKeyHash,
            fee: u64,
            counter: u64,
            gas_limit: u64,
            storage_limit: u64,
            delegate: Option<Arc<PublicKeyHash>>,
        ) -> Self {
            Self {
                source: source.clone(),
                fee,
                counter,
                gas_limit,
                storage_limit,
                delegate: delegate.map(|delegate| delegate.as_ref().clone()),
            }
        }

        #[doc = "Forge the operation."]
        pub fn forge(&self) -> Result<Vec<u8>, Error> {
            let mut out = Vec::<u8>::new();
            let delegation = OperationContent::Delegation(ManagerOperationContent {
                source: self.source.0.clone(),
                fee: self.fee.into(),
                counter: self.counter.into(),
                gas_limit: self.gas_limit.into(),
                storage_limit: self.storage_limit.into(),
                operation: DelegationContent {
                    delegate: self.delegate.clone().map(|delegate| delegate.0),
                },
            });
            delegation
                .bin_write(&mut out)
                .map_err(|err| Error::Forge(ForgingError::ToBytes(err)))?;
            Ok(out)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::keys::{BlsSignature, PublicKey, PublicKeyHash};
    use operation::*;

    // All messages bytes were generated using `octez-codec encode "alpha.script.expr" from '{ "string": "$MSG" }'`

    #[test]
    fn empty_message_forging() {
        let msg = "";
        let raw_msg = forge_message(msg).expect(&format!("Forging message {} should succeed", msg));
        let expected_bytes: Vec<u8> = vec![0x05, 0x01, 0x00, 0x00, 0x00, 0x00];
        assert_eq!(
            raw_msg, expected_bytes,
            "Message must be forged into the expected bytes"
        );
    }

    #[test]
    fn message_forging() {
        let msg = "message";
        let raw_msg = forge_message(msg).expect(&format!("Forging message {} should succeed", msg));
        let expected_bytes: Vec<u8> = vec![
            0x05, 0x01, 0x00, 0x00, 0x00, 0x07, b'm', b'e', b's', b's', b'a', b'g', b'e',
        ];
        assert_eq!(
            raw_msg, expected_bytes,
            "Message must be forged into the expected bytes"
        );
    }

    #[test]
    fn large_message_forging() {
        let msg = "a".repeat(256);
        let raw_msg =
            forge_message(&msg).expect(&format!("Forging message {} should succeed", msg));
        let expected_bytes: Vec<u8> =
            [vec![0x05, 0x01, 0x00, 0x00, 0x01, 0x00], [b'a'].repeat(256)].concat();
        assert_eq!(
            raw_msg, expected_bytes,
            "Message must be forged into the expected bytes"
        );
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "reveal",
      "source": "tz1QFD9WqLWZmmAuqnnTPPUjfauitYEWdshv",
      "fee": "274",
      "counter": "43947",
      "gas_limit": "169",
      "storage_limit": "0",
      "public_key": "edpkuDMUm7Y53wp4gxeLBXuiAhXZrLn8XB1R83ksvvesH8Lp8bmCfK"
    }'
    */
    #[test]
    fn reveal_forging() {
        let public_key =
            PublicKey::from_b58check("edpkuDMUm7Y53wp4gxeLBXuiAhXZrLn8XB1R83ksvvesH8Lp8bmCfK")
                .unwrap();
        let source = PublicKeyHash::from_b58check("tz1QFD9WqLWZmmAuqnnTPPUjfauitYEWdshv").unwrap();
        let (fee, counter, gas_limit, storage_limit) = (274, 43947, 169, 0);
        let reveal = Reveal::new(
            &source,
            fee,
            counter,
            gas_limit,
            storage_limit,
            &public_key,
            None,
        );
        let raw_reveal = reveal
            .forge()
            .expect(&format!("Forging operation {:?} should succeed", reveal));

        let bytes = hex::decode("6b003287ca0e2768be954c0142783bad9ae1b3dae2009202abd702a90100004b39cf3680892b9fcf6da83ec1f84907c0251b7b470d1911d92981364d1a0c1300").unwrap();
        assert_eq!(
            bytes, raw_reveal,
            "Reveal must be forged into the expected bytes"
        );
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "reveal",
      "source": "tz4AebqGUAFk8rTwiS7EpbY8dA3zHmmxiFPT",
      "fee": "469",
      "counter": "3",
      "gas_limit": "2169",
      "storage_limit": "277",
      "public_key": "BLpk1xS8uUKcZ7QwEnasnpE3XezJapXQzdkhh7ecjAqGPvDQf1HGLC1cHCnc7oq4PqdGAMy2oSBc",
      "proof": "BLsigAw3bp2QZL8YCWptYYYE1zQr4vidqE7WT4jctre5HpAdjVYmXnGeqYfASAzPB2pWaDCdiRBApWY5Y4V7WJapj3GFHBHnTZh6eoxCduu2FfXuNmSKN1NQLiztAt6nBWytrR1xAzZDg9"
    }'
    */
    #[test]
    fn reveal_with_proof_forging() {
        let public_key = PublicKey::from_b58check(
            "BLpk1xS8uUKcZ7QwEnasnpE3XezJapXQzdkhh7ecjAqGPvDQf1HGLC1cHCnc7oq4PqdGAMy2oSBc",
        )
        .unwrap();
        let source = PublicKeyHash::from_b58check("tz4AebqGUAFk8rTwiS7EpbY8dA3zHmmxiFPT").unwrap();
        let proof = BlsSignature::from_b58check("BLsigAw3bp2QZL8YCWptYYYE1zQr4vidqE7WT4jctre5HpAdjVYmXnGeqYfASAzPB2pWaDCdiRBApWY5Y4V7WJapj3GFHBHnTZh6eoxCduu2FfXuNmSKN1NQLiztAt6nBWytrR1xAzZDg9").unwrap();
        let (fee, counter, gas_limit, storage_limit) = (469, 3, 2169, 277);
        let reveal = Reveal::new(
            &source,
            fee,
            counter,
            gas_limit,
            storage_limit,
            &public_key,
            Some(Arc::new(proof)),
        );
        let raw_reveal = reveal
            .forge()
            .expect(&format!("Forging operation {:?} should succeed", reveal));

        let bytes = hex::decode("6b0312086b2a01d14b324153774b428bcb98eeda5c0bd50303f910950203b11e34b8846d52fbc19a188006b1d085a3c7ed90063b221edb607c538e8ef076d9a6a1c37cfda431a18c97f1c5a31b6eff00000060a4b4274fb5de18c213faa255991b18893e5c43cb1ef8f748fd51f05ab473466a945696eb06852de9b89dc26037b907b7080f192c3324f0919f3a3326efcd3719219aaa822ac73ff3eb3a1d9f85285ef842eba9454404d81d25f2852da046eade").unwrap();
        assert_eq!(
            bytes, raw_reveal,
            "Reveal must be forged into the expected bytes"
        );
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "delegation",
      "source": "tz1SMHZCpzRUoaoz9gA18pNUghpyYY4N6fif",
      "fee": "13",
      "counter": "66",
      "gas_limit": "271",
      "storage_limit": "128",
      "delegate": "tz3NGKzVp8ezNnu5qx8mY3iioSUXKfC1d8Yc"
    }'
    */
    #[test]
    fn delegation_forging() {
        let delegate =
            PublicKeyHash::from_b58check("tz3NGKzVp8ezNnu5qx8mY3iioSUXKfC1d8Yc").unwrap();
        let source = PublicKeyHash::from_b58check("tz1SMHZCpzRUoaoz9gA18pNUghpyYY4N6fif").unwrap();
        let (fee, counter, gas_limit, storage_limit) = (13, 66, 271, 128);
        let delegation = Delegation::new(
            &source,
            fee,
            counter,
            gas_limit,
            storage_limit,
            Some(Arc::new(delegate)),
        );
        let raw_delegation = delegation.forge().expect(&format!(
            "Forging operation {:?} should succeed",
            delegation
        ));

        let bytes = hex::decode("6e00499e3772f0fd50e62d325ad12ca9e54fde8b4f1f0d428f028001ff02153c42139fbbe509e9023bb85eac281709766070").unwrap();
        assert_eq!(
            bytes, raw_delegation,
            "Delegation must be forged into the expected bytes"
        );
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "delegation",
      "source": "tz2GNQB7rXjNXBX6msePzQ2nBWYUUGutYy5p",
      "fee": "609",
      "counter": "7",
      "gas_limit": "12",
      "storage_limit": "11"
    }'
    */
    #[test]
    fn remove_delegate_forging() {
        let source = PublicKeyHash::from_b58check("tz2GNQB7rXjNXBX6msePzQ2nBWYUUGutYy5p").unwrap();
        let (fee, counter, gas_limit, storage_limit) = (609, 7, 12, 11);
        let delegation = Delegation::new(&source, fee, counter, gas_limit, storage_limit, None);
        let raw_delegation = delegation.forge().expect(&format!(
            "Forging operation {:?} should succeed",
            delegation
        ));

        let bytes =
            hex::decode("6e01585a321426fbb1303d16b569e571109eb68d8c1be104070c0b00").unwrap();
        assert_eq!(
            bytes, raw_delegation,
            "Delegation must be forged into the expected bytes"
        );
    }
}

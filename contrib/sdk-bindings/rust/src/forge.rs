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
#[uniffi::export]
pub fn forge_message(msg: &str) -> Vec<u8> {
    mir::ast::Micheline::String(msg.to_owned()).encode_for_pack()
}

#[uniffi::export]
pub fn forge_operation(
    branch: &crate::hash::BlockHash,
    operations: Vec<Arc<operation::Operation>>,
) -> Result<Vec<u8>, Error> {
    use enc::BinWriter;
    use tezos_protocol::operation::{OperationContentList, UnsignedOperation};
    let unsigned_operation = UnsignedOperation {
        branch: branch.0.clone(),
        content_list: OperationContentList {
            contents: operations.into_iter().map(|op| op.0.clone()).collect(),
        },
    };
    unsigned_operation
        .to_bytes()
        .map_err(|err| Error::Forge(ForgingError::ToBytes(err)))
}

pub mod operation {
    use super::*;
    use crate::entrypoint::Entrypoint;
    use crate::hash::{BlsSignature, Contract, PublicKey, PublicKeyHash};
    use crate::Error;
    use tezos_data_encoding::enc::BinWriter;
    use tezos_protocol::operation::{
        DelegationContent, ManagerOperationContent, OperationContent, OriginationContent,
        Parameters, RevealContent, Script, TransactionContent,
    };

    #[derive(uniffi::Object, Debug)]
    pub struct Operation(pub(crate) OperationContent);

    #[uniffi::export]
    impl Operation {
        /// Build a reveal operation.
        #[uniffi::constructor(default(proof = None))]
        pub fn reveal(
            source: &PublicKeyHash,
            fee: u64,
            counter: u64,
            gas_limit: u64,
            storage_limit: u64,
            public_key: &PublicKey,
            proof: Option<Arc<BlsSignature>>,
        ) -> Self {
            Self(OperationContent::Reveal(ManagerOperationContent {
                source: source.0.clone(),
                fee: fee.into(),
                counter: counter.into(),
                gas_limit: gas_limit.into(),
                storage_limit: storage_limit.into(),
                operation: RevealContent {
                    pk: public_key.0.clone(),
                    proof: proof.map(|proof| proof.0.clone()),
                },
            }))
        }

        #[allow(clippy::too_many_arguments)]
        /// Build a transaction operation.
        #[uniffi::constructor(default(entrypoint = None, value = None))]
        pub fn transaction(
            source: &PublicKeyHash,
            fee: u64,
            counter: u64,
            gas_limit: u64,
            storage_limit: u64,
            amount: u64,
            destination: &Contract,
            entrypoint: Option<Arc<Entrypoint>>,
            value: Option<Vec<u8>>,
        ) -> Self {
            Self(OperationContent::Transaction(ManagerOperationContent {
                source: source.0.clone(),
                fee: fee.into(),
                counter: counter.into(),
                gas_limit: gas_limit.into(),
                storage_limit: storage_limit.into(),
                operation: TransactionContent {
                    amount: amount.into(),
                    destination: destination.0.clone(),
                    parameters: Parameters {
                        entrypoint: entrypoint.map_or_else(
                            tezos_protocol::entrypoint::Entrypoint::default,
                            |entrypoint| entrypoint.0.clone(),
                        ),
                        // octez-client convert data "Unit" from Michelson to binary
                        value: value.unwrap_or(vec![0x03, 0x0b]).clone(),
                    }
                    .into(),
                },
            }))
        }

        /// Build a origination operation.
        ///
        /// Its script is always as follows:
        ///   'script': {
        ///     'code': [],
        ///     'storage': {
        ///       'prim': 'unit'
        ///     }
        ///   }
        #[uniffi::constructor]
        pub fn origination(
            source: &PublicKeyHash,
            fee: u64,
            counter: u64,
            gas_limit: u64,
            storage_limit: u64,
            balance: u64,
            delegate: Option<Arc<PublicKeyHash>>,
        ) -> Self {
            Self(OperationContent::Origination(ManagerOperationContent {
                source: source.0.clone(),
                fee: fee.into(),
                counter: counter.into(),
                gas_limit: gas_limit.into(),
                storage_limit: storage_limit.into(),
                operation: OriginationContent {
                    balance: balance.into(),
                    delegate: delegate.map(|delegate| delegate.0.clone()),
                    script: Script {
                        // Seq(&[]).encode(),
                        code: vec![0x02, 0x00, 0x00, 0x00, 0x00],
                        // Micheline::App(Prim::unit, &[], NO_ANNS).encode(),
                        storage: vec![0x03, 0x6c],
                    },
                },
            }))
        }

        /// Build a delegation operation.
        #[uniffi::constructor]
        pub fn delegation(
            source: &PublicKeyHash,
            fee: u64,
            counter: u64,
            gas_limit: u64,
            storage_limit: u64,
            delegate: Option<Arc<PublicKeyHash>>,
        ) -> Self {
            Self(OperationContent::Delegation(ManagerOperationContent {
                source: source.0.clone(),
                fee: fee.into(),
                counter: counter.into(),
                gas_limit: gas_limit.into(),
                storage_limit: storage_limit.into(),
                operation: DelegationContent {
                    delegate: delegate.map(|delegate| delegate.0.clone()),
                },
            }))
        }

        #[doc = "Forge the operation."]
        pub fn forge(&self) -> Result<Vec<u8>, Error> {
            self.0
                .to_bytes()
                .map_err(|err| Error::Forge(ForgingError::ToBytes(err)))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entrypoint::Entrypoint;
    use crate::hash::{BlockHash, BlsSignature, Contract, PublicKey, PublicKeyHash};
    use operation::*;

    // All messages bytes were generated using `octez-codec encode "alpha.script.expr" from '{ "string": "$MSG" }'`

    #[test]
    fn empty_message_forging() {
        let msg = "";
        let raw_msg = forge_message(msg);
        let expected_bytes: Vec<u8> = vec![0x05, 0x01, 0x00, 0x00, 0x00, 0x00];
        assert_eq!(
            raw_msg, expected_bytes,
            "Message must be forged into the expected bytes"
        );
    }

    #[test]
    fn message_forging() {
        let msg = "message";
        let raw_msg = forge_message(msg);
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
        let raw_msg = forge_message(&msg);
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
        let reveal = Operation::reveal(
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
        let reveal = Operation::reveal(
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
      "kind": "transaction",
      "source": "tz1WQGXKa2h5edfELL1XMBrDmcb4qLHQye4U",
      "fee": "8318",
      "counter": "72014",
      "gas_limit": "145000",
      "storage_limit": "1204",
      "amount": "34",
      "destination": "KT1G4D3W9cf86dzAmZBN9nwUn7sYh4DYMRb4",
      "parameters": {
        "entrypoint": "foo",
        "value": {
          "prim": "Pair", "args": [
            { "int": "1" },
            { "string": "a" }
          ]
        }
      }
    }'
    */
    #[test]
    fn transaction_forging() {
        let source = PublicKeyHash::from_b58check("tz1WQGXKa2h5edfELL1XMBrDmcb4qLHQye4U").unwrap();
        let (fee, counter, gas_limit, storage_limit, amount) = (8318, 72014, 145000, 1204, 34);
        let destination = Contract::from_b58check("KT1G4D3W9cf86dzAmZBN9nwUn7sYh4DYMRb4").unwrap();
        let entrypoint = Entrypoint::new("foo").unwrap();
        let value = hex::decode("07070001010000000161").unwrap();
        let transaction = Operation::transaction(
            &source,
            fee,
            counter,
            gas_limit,
            storage_limit,
            amount,
            &destination,
            Some(Arc::new(entrypoint)),
            // octez-client convert data 'Pair 1 "a"' from Michelson to binary
            Some(value),
        );
        let raw_transaction = transaction.forge().expect(&format!(
            "Forging operation {:?} should succeed",
            transaction
        ));

        let bytes = hex::decode("6c00760f1b125362fdf15f0e1093b1c6555b2dfdb8e4fe40ceb204e8ec08b409220151f9612eb937431bcb785af89f3aae78d4e03f8200ffff03666f6f0000000a07070001010000000161").unwrap();
        assert_eq!(
            bytes, raw_transaction,
            "Transaction must be forged into the expected bytes"
        );
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "transaction",
      "source": "tz2WU9XW86EdgVQZrbPphjUZiRfXXssY9wEP",
      "fee": "332",
      "counter": "24",
      "gas_limit": "4631",
      "storage_limit": "11",
      "amount": "0",
      "destination": "KT1S5cQmS4wXjG7JubRUCWzH3DaU7S2XfeFT",
      "parameters": {
        "entrypoint": "stake",
        "value": { "prim": "Unit" }
      }
    }'
    */
    #[test]
    fn transaction_stake_forging() {
        let source = PublicKeyHash::from_b58check("tz2WU9XW86EdgVQZrbPphjUZiRfXXssY9wEP").unwrap();
        let (fee, counter, gas_limit, storage_limit, amount) = (332, 24, 4631, 11, 0);
        let destination = Contract::from_b58check("KT1S5cQmS4wXjG7JubRUCWzH3DaU7S2XfeFT").unwrap();
        let entrypoint = Entrypoint::new("stake").unwrap();
        let transaction = Operation::transaction(
            &source,
            fee,
            counter,
            gas_limit,
            storage_limit,
            amount,
            &destination,
            Some(Arc::new(entrypoint)),
            None,
        );
        let raw_transaction = transaction.forge().expect(&format!(
            "Forging operation {:?} should succeed",
            transaction
        ));

        let bytes = hex::decode("6c01f3023970264e14502daa1db4324527bc464fe0fdcc021897240b0001bfee8f130c31aeab88080bf2265b2a7980da5d6800ff0600000002030b").unwrap();
        assert_eq!(
            bytes, raw_transaction,
            "Transaction must be forged into the expected bytes"
        );
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "transaction",
      "source": "tz1SMHZCpzRUoaoz9gA18pNUghpyYY4N6fif",
      "fee": "99",
      "counter": "1401",
      "gas_limit": "1",
      "storage_limit": "0",
      "amount": "200",
      "destination": "tz4Quq6VcCeJVmCknjzTX5kcrhUzcMruoavF"
    }'
    */
    #[test]
    fn transaction_transfer_forging() {
        let source = PublicKeyHash::from_b58check("tz1SMHZCpzRUoaoz9gA18pNUghpyYY4N6fif").unwrap();
        let (fee, counter, gas_limit, storage_limit, amount) = (99, 1401, 1, 0, 200);
        let destination = Contract::from_b58check("tz4Quq6VcCeJVmCknjzTX5kcrhUzcMruoavF").unwrap();
        let transaction = Operation::transaction(
            &source,
            fee,
            counter,
            gas_limit,
            storage_limit,
            amount,
            &destination,
            None,
            None,
        );
        let raw_transaction = transaction.forge().expect(&format!(
            "Forging operation {:?} should succeed",
            transaction
        ));

        let bytes = hex::decode("6c00499e3772f0fd50e62d325ad12ca9e54fde8b4f1f63f90a0100c8010003ae7b7d713977a27ec643969f0c2e665ba9ad9aa100").unwrap();
        assert_eq!(
            bytes, raw_transaction,
            "Transaction must be forged into the expected bytes"
        );
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "origination",
      "source": "tz4Quq6VcCeJVmCknjzTX5kcrhUzcMruoavF",
      "fee": "0",
      "counter": "812",
      "gas_limit": "74",
      "storage_limit": "98",
      "balance": "0",
      "script": {
        "code": [],
        "storage": {
          "prim": "unit"
        }
      }
    }'
    */
    #[test]
    fn origination_forging() {
        let source = PublicKeyHash::from_b58check("tz4Quq6VcCeJVmCknjzTX5kcrhUzcMruoavF").unwrap();
        let (balance, fee, counter, gas_limit, storage_limit) = (0, 0, 812, 74, 98);
        let origination = Operation::origination(
            &source,
            fee,
            counter,
            gas_limit,
            storage_limit,
            balance,
            None,
        );
        let raw_origination = origination.forge().expect(&format!(
            "Forging operation {:?} should succeed",
            origination
        ));

        let bytes = hex::decode("6d03ae7b7d713977a27ec643969f0c2e665ba9ad9aa100ac064a62000000000005020000000000000002036c").unwrap();
        assert_eq!(
            bytes, raw_origination,
            "Origination must be forged into the expected bytes"
        );
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "origination",
      "source": "tz1SUWNMC3hUdBRzzrbTbiuGPH1KFVifTQw7",
      "fee": "1722",
      "counter": "461",
      "gas_limit": "67000",
      "storage_limit": "1",
      "balance": "1000000",
      "delegate": "tz1SUWNMC3hUdBRzzrbTbiuGPH1KFVifTQw7",
      "script": {
        "code": [],
        "storage": {
          "prim": "unit"
        }
      }
    }'
    */
    #[test]
    fn origination_delegated_contract_forging() {
        let source = PublicKeyHash::from_b58check("tz1SUWNMC3hUdBRzzrbTbiuGPH1KFVifTQw7").unwrap();
        let (balance, fee, counter, gas_limit, storage_limit) = (1_000_000, 1722, 461, 67_000, 1);
        let origination = Operation::origination(
            &source,
            fee,
            counter,
            gas_limit,
            storage_limit,
            balance,
            Some(Arc::new(source.clone())),
        );
        let raw_origination = origination.forge().expect(&format!(
            "Forging operation {:?} should succeed",
            origination
        ));

        let bytes =
            hex::decode("6d004afbd2ede908e6700eb9b54352c1d2dceee8d0feba0dcd03b88b0401c0843dff004afbd2ede908e6700eb9b54352c1d2dceee8d0fe00000005020000000000000002036c").unwrap();
        assert_eq!(
            bytes, raw_origination,
            "Origination must be forged into the expected bytes"
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
        let delegation = Operation::delegation(
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
        let delegation =
            Operation::delegation(&source, fee, counter, gas_limit, storage_limit, None);
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

    /*
    octez-codec encode "023-PtSeouLo.operation.unsigned" from '{
      "branch": "BL2fRnPdMqU3z6iEvEYNofAwYoTTLhaMLsvMES2Wf4Zj7Lf69qy",
      "contents": [
        {
          "kind": "reveal",
          "source": "tz1dPdVboerXfFsZ5L4ga2y9ZJKuKT5x391x",
          "fee": "114",
          "counter": "43209",
          "gas_limit": "785",
          "storage_limit": "231",
          "public_key": "edpkvXZ4cwEip8QxeoNZPjxPUKCpVJNQXio1k6Dp16PsSh6ugNjhw9"
        },
        {
          "kind": "transaction",
          "source": "tz1dPdVboerXfFsZ5L4ga2y9ZJKuKT5x391x",
          "fee": "437",
          "counter": "43210",
          "gas_limit": "835",
          "storage_limit": "0",
          "amount": "753",
          "destination": "KT1EHorBhV6bEJH7wsyxSuDXsQ5dw4VWVwL4",
          "parameters": {
            "entrypoint": "unstake",
            "value": { "prim": "Unit" }
          }
        }
      ]
    }'
    */
    #[test]
    fn operation_forging() {
        let pkh = PublicKeyHash::from_b58check("tz1dPdVboerXfFsZ5L4ga2y9ZJKuKT5x391x").unwrap();
        let pk = PublicKey::from_b58check("edpkvXZ4cwEip8QxeoNZPjxPUKCpVJNQXio1k6Dp16PsSh6ugNjhw9")
            .unwrap();

        let reveal = Operation::reveal(&pkh, 114, 43209, 785, 231, &pk, None);
        let transaction = Operation::transaction(
            &pkh,
            437,
            43210,
            835,
            0,
            753,
            &Contract::from_b58check("KT1EHorBhV6bEJH7wsyxSuDXsQ5dw4VWVwL4").unwrap(),
            Some(Arc::new(Entrypoint::new("unstake").unwrap())),
            // octez-client convert data 'Unit' from Michelson to binary
            Some(hex::decode("030b").unwrap()),
        );

        let branch =
            BlockHash::from_b58check("BL2fRnPdMqU3z6iEvEYNofAwYoTTLhaMLsvMES2Wf4Zj7Lf69qy")
                .unwrap();
        let operations = vec![Arc::new(reveal), Arc::new(transaction)];

        let forged_operation =
            forge_operation(&branch, operations).expect("Forging operations should succeed");

        let bytes = hex::decode("29bc70a0d87890e8c45b9653b65385638d1d829e053591a21812999c69d305286b00c2b9134fa5b947b44f9a9c9a75f83d7f2e8524f672c9d1029106e70100f83f217c459a46b88b0fb2b099a4b25d6ffa97b55201132b28091287842a6739006c00c2b9134fa5b947b44f9a9c9a75f83d7f2e8524f6b503cad102c30600f105013e9b96d5173312baacd650285dfe748d2337b39d00ff0700000002030b").unwrap();
        assert_eq!(
            bytes, forged_operation,
            "Operations must be forged into the expected bytes"
        );
    }
}

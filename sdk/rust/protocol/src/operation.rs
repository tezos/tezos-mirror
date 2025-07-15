// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Tezos operations
/// The whole module is inspired of `src/proto_alpha/lib_protocol/operation_repr.ml` to represent the operation
use crate::contract::Contract;
use crate::entrypoint::Entrypoint;
use tezos_crypto_rs::{hash::BlsSignature, public_key::PublicKey, public_key_hash::PublicKeyHash};
use tezos_data_encoding::{enc::BinWriter, nom::NomReader, types::Narith};

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
#[encoding(tags = "u8")]
pub enum OperationContent {
    #[encoding(tag = 107)]
    Reveal(ManagerOperationContent<RevealContent>),
    #[encoding(tag = 108)]
    Transaction(ManagerOperationContent<TransactionContent>),
    #[encoding(tag = 110)]
    Delegation(ManagerOperationContent<DelegationContent>),
}

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct ManagerOperationContent<Op> {
    pub source: PublicKeyHash,
    pub fee: Narith,
    pub counter: Narith,
    pub gas_limit: Narith,
    pub storage_limit: Narith,
    pub operation: Op,
}

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct RevealContent {
    pub pk: PublicKey,
    #[encoding(dynamic)]
    pub proof: Option<BlsSignature>,
}

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct TransactionContent {
    pub amount: Narith,
    pub destination: Contract,
    pub parameters: Option<Parameter>,
}

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct Parameter {
    pub entrypoint: Entrypoint,
    #[encoding(dynamic, bytes)]
    pub value: Vec<u8>,
}

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct DelegationContent {
    pub delegate: Option<PublicKeyHash>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_crypto_rs::hash::HashTrait;

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "reveal",
      "source": "tz2WU9XW86EdgVQZrbPphjUZiRfXXssY9wEP",
      "fee": "31",
      "counter": "1005",
      "gas_limit": "89",
      "storage_limit": "7",
      "public_key": "sppk7bo7kcRyjajZaAqEfqdtCNx3wgizhJPFqaEuisncbDFMgn6v4iP"
    }'
    */
    #[test]
    fn reveal_encoding() {
        let pk =
            PublicKey::from_b58check("sppk7bo7kcRyjajZaAqEfqdtCNx3wgizhJPFqaEuisncbDFMgn6v4iP")
                .unwrap();
        let pkh = PublicKeyHash::from_b58check("tz2WU9XW86EdgVQZrbPphjUZiRfXXssY9wEP").unwrap();
        let fee = 31.into();
        let counter = 1005.into();
        let gas_limit = 89.into();
        let storage_limit = 7.into();
        let operation = OperationContent::Reveal(ManagerOperationContent {
            source: pkh,
            fee,
            counter,
            gas_limit,
            storage_limit,
            operation: RevealContent { pk, proof: None },
        });

        let encoded_operation = operation.to_bytes().unwrap();

        let bytes = hex::decode("6b01f3023970264e14502daa1db4324527bc464fe0fd1fed0759070103480fcf4241d5903bd5b9a71db63fc6784dc9e686acf0dac9b4305d54cb64294600").unwrap();
        assert_eq!(bytes, encoded_operation);

        let (bytes, decoded_operation) = OperationContent::nom_read(&encoded_operation).unwrap();
        assert_eq!(operation, decoded_operation);
        assert!(bytes.is_empty());
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "reveal",
      "source": "tz4N581kB4vkDANA7WTN6tcnBeCDawv1k4Cy",
      "fee": "127",
      "counter": "91",
      "gas_limit": "3250",
      "storage_limit": "0",
      "public_key": "BLpk1qUhA8WwcwKavKJb3nkPVeKj5FGCsruWQXyRpwypxe7ocbq6Npe4cjA2TLfqzgBKfRAtBajV",
      "proof": "BLsigAKrfg6QLipEvWQrhPRHbWo7x8YLFvteATg65gbvpfo9qKSabb5j92nozhecDkxvnvocv6q2WUUFvoKxkZuCaTNUUGpJPbc7igdf7MUi8fSovaLbWzZEDVGLjon7ZNPZPDpwf6h9jT"
    }'
    */
    #[test]
    fn reveal_with_proof_encoding() {
        let pk = PublicKey::from_b58check(
            "BLpk1qUhA8WwcwKavKJb3nkPVeKj5FGCsruWQXyRpwypxe7ocbq6Npe4cjA2TLfqzgBKfRAtBajV",
        )
        .unwrap();
        let pkh = PublicKeyHash::from_b58check("tz4N581kB4vkDANA7WTN6tcnBeCDawv1k4Cy").unwrap();
        let proof = BlsSignature::from_b58check("BLsigAKrfg6QLipEvWQrhPRHbWo7x8YLFvteATg65gbvpfo9qKSabb5j92nozhecDkxvnvocv6q2WUUFvoKxkZuCaTNUUGpJPbc7igdf7MUi8fSovaLbWzZEDVGLjon7ZNPZPDpwf6h9jT").unwrap();
        let fee = 127.into();
        let counter = 91.into();
        let gas_limit = 3250.into();
        let storage_limit = 0.into();
        let operation = OperationContent::Reveal(ManagerOperationContent {
            source: pkh,
            fee,
            counter,
            gas_limit,
            storage_limit,
            operation: RevealContent {
                pk,
                proof: Some(proof),
            },
        });

        let encoded_operation = operation.to_bytes().unwrap();

        let bytes = hex::decode("6b038f54fa9607db6d164b42bd23f3bfedc99460d4bc7f5bb21900039423b322ccff9b945501d28f743a0369583c1552d47dc69fa5d624bde1034df571103c62a969d5998d4682be60ca3e83ff000000609517ce0faabbf099b46a3b786f3ae0ef81cfdf4d89a8b0d1ff9216bf092a8e90987549cbc3234f21af4b29fe0a5f217c16d56336cdc26d5c364c961ca1f7ac44e9a778f9bab5695dd88f0a7cfbfa0582cb8cd7b5ef158b6b59abd576eb181ddc").unwrap();
        assert_eq!(bytes, encoded_operation);

        let (bytes, decoded_operation) = OperationContent::nom_read(&encoded_operation).unwrap();
        assert_eq!(operation, decoded_operation);
        assert!(bytes.is_empty());
    }

    /*
    octez-codec encode "022-PsRiotum.operation.contents" from '{
      "kind": "transaction",
      "source": "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
      "fee": "405",
      "counter": "2",
      "gas_limit": "1380",
      "storage_limit": "0",
      "amount": "1000000",
      "destination": "KT1EY9XA4Z5tybQN5zmVUL5cntku1zTCBLTv",
      "parameters": {
        "entrypoint": "B",
        "value": {
          "string": "Hello"
        }
      }
    }'
    */
    #[test]
    fn transaction_encoding() {
        let operation = OperationContent::Transaction(ManagerOperationContent {
            source: PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap(),
            fee: 405_u64.into(),
            counter: 2_u64.into(),
            operation: TransactionContent {
                amount: 1_000_000_u64.into(),
                destination: Contract::from_b58check("KT1EY9XA4Z5tybQN5zmVUL5cntku1zTCBLTv")
                    .unwrap(),
                parameters: Some(Parameter {
                    entrypoint: Entrypoint::try_from("B").unwrap(),
                    // mir::ast::Micheline::String("Hello".into()).encode(),
                    value: vec![0x01, 0x00, 0x00, 0x00, 0x05, 0x48, 0x65, 0x6c, 0x6c, 0x6f],
                }),
            },
            gas_limit: 1380_u64.into(),
            storage_limit: 0_u64.into(),
        });

        let mut encoded_operation = Vec::new();
        operation.bin_write(&mut encoded_operation).unwrap();

        let bytes = hex::decode("6c0002298c03ed7d454a101eb7022bc95f7e5f41ac78950302e40a00c0843d014151d57ddff98da8cd49f0f2cbf89465bcf267a400ffff01420000000a010000000548656c6c6f").unwrap();
        assert_eq!(bytes, encoded_operation);

        let (bytes, decoded_operation) = OperationContent::nom_read(&encoded_operation).unwrap();
        assert_eq!(operation, decoded_operation);
        assert!(bytes.is_empty());
    }

    /*
    octez-codec encode "022-PsRiotum.operation.contents" from '{
      "kind": "transaction",
      "source": "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
      "fee": "987",
      "counter": "456",
      "gas_limit": "0",
      "storage_limit": "1405",
      "amount": "10",
      "destination": "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"
    }'
    */
    #[test]
    fn transaction_transfer_encoding() {
        let operation = OperationContent::Transaction(ManagerOperationContent {
            source: PublicKeyHash::from_b58check("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN").unwrap(),
            fee: 987.into(),
            counter: 456.into(),
            operation: TransactionContent {
                amount: 10.into(),
                destination: Contract::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
                    .unwrap(),
                parameters: None,
            },
            gas_limit: 0.into(),
            storage_limit: 1405.into(),
        });

        let mut encoded_operation = Vec::new();
        operation.bin_write(&mut encoded_operation).unwrap();

        let bytes = hex::decode("6c00e7670f32038107a59a2b9cfefae36ea21f5aa63cdb07c80300fd0a0a000002298c03ed7d454a101eb7022bc95f7e5f41ac7800").unwrap();
        assert_eq!(bytes, encoded_operation);

        let (bytes, decoded_operation) = OperationContent::nom_read(&encoded_operation).unwrap();
        assert_eq!(operation, decoded_operation);
        assert!(bytes.is_empty());
    }

    /*
    octez-codec encode "022-PsRiotum.operation.contents" from '{
      "kind": "transaction",
      "source": "tz3hqqamVC1G22LACFoMgcJeFKZgoGMFSfSn",
      "fee": "7",
      "counter": "4223",
      "gas_limit": "0",
      "storage_limit": "0",
      "amount": "0",
      "destination": "tz4Uzyxg26DJyM4pc1V2pUvLpdsR5jdyzYsZ",
      "parameters": {
        "entrypoint": "remove_delegate",
        "value": {
          "prim": "Unit"
        }
      }
    }'
    */
    #[test]
    fn transaction_fixed_entrypoint_encoding() {
        let operation = OperationContent::Transaction(ManagerOperationContent {
            source: PublicKeyHash::from_b58check("tz3hqqamVC1G22LACFoMgcJeFKZgoGMFSfSn").unwrap(),
            fee: 7.into(),
            counter: 4223.into(),
            operation: TransactionContent {
                amount: 0.into(),
                destination: Contract::from_b58check("tz4Uzyxg26DJyM4pc1V2pUvLpdsR5jdyzYsZ")
                    .unwrap(),
                parameters: Some(Parameter {
                    entrypoint: Entrypoint::try_from("remove_delegate").unwrap(),
                    // Micheline::App(Prim::Unit, &[], NO_ANNS).encode(),
                    value: vec![0x03, 0x0b],
                }),
            },
            gas_limit: 0.into(),
            storage_limit: 0.into(),
        });

        let mut encoded_operation = Vec::new();
        operation.bin_write(&mut encoded_operation).unwrap();

        let bytes = hex::decode("6c02ebfd1371b542831b4be730161d08885c5312e44207ff200000000003db557924e5a295652eff2c1f141d5a5b72b9cc91ff0400000002030b").unwrap();
        assert_eq!(bytes, encoded_operation);

        let (bytes, decoded_operation) = OperationContent::nom_read(&encoded_operation).unwrap();
        assert_eq!(operation, decoded_operation);
        assert!(bytes.is_empty());
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "delegation",
      "source": "tz4Quq6VcCeJVmCknjzTX5kcrhUzcMruoavF",
      "fee": "9000",
      "counter": "59",
      "gas_limit": "302",
      "storage_limit": "1024",
      "delegate": "tz1SUWNMC3hUdBRzzrbTbiuGPH1KFVifTQw7"
    }'
    */
    #[test]
    fn delegation_encoding() {
        let delegate =
            Some(PublicKeyHash::from_b58check("tz1SUWNMC3hUdBRzzrbTbiuGPH1KFVifTQw7").unwrap());
        let pkh = PublicKeyHash::from_b58check("tz4Quq6VcCeJVmCknjzTX5kcrhUzcMruoavF").unwrap();
        let fee = 9000.into();
        let counter = 59.into();
        let gas_limit = 302.into();
        let storage_limit = 1024.into();
        let operation = OperationContent::Delegation(ManagerOperationContent {
            source: pkh,
            fee,
            counter,
            gas_limit,
            storage_limit,
            operation: DelegationContent { delegate },
        });

        let mut encoded_operation = Vec::new();
        operation.bin_write(&mut encoded_operation).unwrap();

        let bytes = hex::decode("6e03ae7b7d713977a27ec643969f0c2e665ba9ad9aa1a8463bae028008ff004afbd2ede908e6700eb9b54352c1d2dceee8d0fe").unwrap();
        assert_eq!(bytes, encoded_operation);

        let (bytes, decoded_operation) = OperationContent::nom_read(&encoded_operation).unwrap();
        assert_eq!(operation, decoded_operation);
        assert!(bytes.is_empty());
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "delegation",
      "source": "tz3S6P2LccJNrejt27KvJRb3BcuS5vGghkP8",
      "fee": "0",
      "counter": "1",
      "gas_limit": "0",
      "storage_limit": "0"
    }'
    */
    #[test]
    fn delegation_without_delegate_encoding() {
        let delegate = None;
        let pkh = PublicKeyHash::from_b58check("tz3S6P2LccJNrejt27KvJRb3BcuS5vGghkP8").unwrap();
        let fee = 0.into();
        let counter = 1.into();
        let gas_limit = 0.into();
        let storage_limit = 0.into();
        let operation = OperationContent::Delegation(ManagerOperationContent {
            source: pkh,
            fee,
            counter,
            gas_limit,
            storage_limit,
            operation: DelegationContent { delegate },
        });

        let mut encoded_operation = Vec::new();
        operation.bin_write(&mut encoded_operation).unwrap();

        let bytes = hex::decode("6e023f3b21e54e98db3845a2a5033d96877f7f9cea870001000000").unwrap();
        assert_eq!(bytes, encoded_operation);

        let (bytes, decoded_operation) = OperationContent::nom_read(&encoded_operation).unwrap();
        assert_eq!(operation, decoded_operation);
        assert!(bytes.is_empty());
    }
}

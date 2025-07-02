// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::Error;
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
    use crate::keys::PublicKeyHash;
    use crate::Error;
    use tezos_data_encoding::enc::BinWriter;
    use tezos_protocol::operation::{DelegationContent, ManagerOperationContent, OperationContent};

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
    use crate::keys::PublicKeyHash;
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

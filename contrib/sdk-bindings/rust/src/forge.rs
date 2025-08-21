// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::Error;
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

#[cfg(test)]
mod tests {
    use super::*;

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
}

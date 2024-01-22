// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Sendable/serializable versions of `Parsed*` structs.

use super::{Operation, ToBytesError};
use crypto::hash::SecretKeyEd25519;
use tezos_crypto_rs::blake2b::digest_256;
use tezos_data_encoding::enc::{self, BinError, BinWriter};
use tezos_data_encoding::encoding::{Encoding, HasEncoding};

#[derive(Debug, PartialEq)]
/// A batch of operations, for serialization as an external inbox message.
pub struct Batch {
    ops_with_keys: Vec<(Operation, SecretKeyEd25519)>,
}

impl Batch {
    /// Create a new batch from a list of operations.
    pub fn new(ops_with_keys: Vec<(Operation, SecretKeyEd25519)>) -> Self {
        Self { ops_with_keys }
    }
}

impl BinWriter for Batch {
    /// Serialize a batch of transactions, including the aggregate signature.
    ///
    /// Compatible with [ParsedBatch::parse].
    ///
    /// [ParsedBatch::parse]: super::ParsedBatch::parse
    fn bin_write(&self, output: &mut Vec<u8>) -> enc::BinResult {
        let msgs = self
            .ops_with_keys
            .iter()
            .map(|(op, sk)| {
                let mut bytes = Vec::new();
                op.bin_write(&mut bytes)?;
                // TODO: https://github.com/trilitech/tezedge/issues/44
                // Consider moving the hashing logic into `sk.sign`.
                let hash = digest_256(&bytes)?;
                let mut sig = sk.sign(hash.as_slice())?;
                bytes.append(&mut sig.0);

                Ok(bytes)
            })
            .map(|r| r.map_err(|e: ToBytesError| BinError::custom(e.to_string())));

        enc::dynamic(enc::list(|msg, output: &mut Vec<u8>| {
            output.extend(msg?);
            Ok(())
        }))(msgs, output)?;

        Ok(())
    }
}

impl HasEncoding for Batch {
    fn encoding() -> Encoding {
        Encoding::Custom
    }
}

#[cfg(test)]
mod test {

    use proptest::collection;
    use proptest::prelude::*;
    use tezos_data_encoding::enc::BinWriter;

    use super::Batch;
    use crate::inbox::v1::Operation;
    use crate::inbox::v1::ParsedBatch;

    proptest! {
        #[test]
        fn sendable_batch_encode_decode(
            operations in collection::vec(Operation::arb_with_signer(), 0..1),
            remaining_input in any::<Vec<u8>>(),
        ) {
            // Arrange
            let batch = Batch::new(operations);

            let mut encoded = Vec::new();
            batch.bin_write(&mut encoded).expect("Failed to encode batch");

            encoded.extend_from_slice(remaining_input.as_slice());

            let (remaining, parsed_batch) = ParsedBatch::parse(encoded.as_slice())
                .expect("Parsing of encoded batch failed");

            assert_eq!(remaining_input, remaining);

            let original = batch.ops_with_keys;
            assert_eq!(original.len(), parsed_batch.operations.len());

            for ((original, _), parsed) in original.into_iter()
                .zip(parsed_batch.operations.into_iter()) {
                assert_eq!(original, parsed.operation);
            }
        }
    }
}

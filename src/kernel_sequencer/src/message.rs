// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{all_consuming, map},
    sequence::preceded,
};
#[cfg(test)]
use tezos_crypto_rs::hash::SecretKeyEd25519;
use tezos_crypto_rs::PublicKeySignatureVerifier;
use tezos_crypto_rs::{blake2b, hash::Signature};
use tezos_data_encoding::{
    enc::{self, BinResult, BinWriter},
    nom::{NomReader, NomResult},
};
use tezos_smart_rollup_encoding::public_key::PublicKey;
use tezos_smart_rollup_encoding::smart_rollup::SmartRollupAddress;
use tezos_smart_rollup_host::runtime::RuntimeError;

#[derive(Debug, Clone, PartialEq, Eq, BinWriter, NomReader)]
pub struct UnverifiedSigned<A>
where
    A: NomReader + BinWriter,
{
    body: A,
    signature: Signature,
}

impl<A> UnverifiedSigned<A>
where
    A: NomReader + BinWriter,
{
    /// Returns the hash of the body.
    fn hash_body(body: &A) -> Result<Vec<u8>, RuntimeError> {
        // Get the bytes of the body.
        let mut bytes = Vec::new();
        body.bin_write(&mut bytes)
            .map_err(|_| RuntimeError::DecodingError)?;

        // Returns the hash of the body.
        blake2b::digest_256(&bytes).map_err(|_| RuntimeError::DecodingError)
    }

    /// Returns the hash of the signed message.
    ///
    /// It's equivalent of the body's hash.
    pub fn hash(&self) -> Result<Vec<u8>, RuntimeError> {
        UnverifiedSigned::hash_body(&self.body)
    }

    /// Returns the body of the message and verifies the signature.
    pub fn body(self, public_key: &PublicKey) -> Result<A, RuntimeError> {
        let hash = &self.hash()?;
        let is_correct = public_key.verify_signature(&self.signature, hash);
        match is_correct {
            Ok(true) => Ok(self.body),
            _ => Err(RuntimeError::DecodingError),
        }
    }

    /// Return a signed body
    #[cfg(test)]
    fn sign_ed25519(body: A, secret: &SecretKeyEd25519) -> Result<Self, RuntimeError> {
        use tezos_smart_rollup_host::Error;

        let hash = UnverifiedSigned::hash_body(&body)?;
        let signature = secret
            .sign(hash)
            .map_err(|_| RuntimeError::HostErr(Error::GenericInvalidAccess))?;
        Ok(UnverifiedSigned { body, signature })
    }
}

/// Framing protocol v0
///
/// The framing protocol starts with a 0, then the address of the rollup, then the message
/// The message should start by a tag, provided by the Tag trait
///
/// [0x00, smart rollup address, tag, message]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Framed<P> {
    pub destination: SmartRollupAddress,
    pub payload: P,
}

/// Messages sent by the user to the sequencer
#[derive(NomReader, BinWriter, Clone, Debug, PartialEq, Eq)]
pub struct Bytes {
    #[encoding(dynamic, list)]
    inner: Vec<u8>,
}

/// Sequence of messages sent by the sequencer
///
/// The sequence contains the number of messages
/// that should be processed from the delayed inbox
/// and the messages from the sequencer
///
/// The number of messages that should be processed
/// from the delayed inbox is divided into two parts
/// delayed_messages_prefix and delayed_messages_suffix
///
/// Then the messages are processed in the following order:
/// First, the number specified by `delayed_messages_prefix` will be
/// processed from the delayed inbox.
/// Then the `l2_messages` will be processed
/// Finally, the number specified by `delayed delayed_messages_suffix`
/// will be processed at the end
#[derive(NomReader, BinWriter, Clone, Debug, PartialEq, Eq)]
pub struct Sequence {
    nonce: u32,
    delayed_messages_prefix: u32,
    delayed_messages_suffix: u32,
    #[encoding(dynamic, list)]
    messages: Vec<Bytes>,
}

/// Message to set the appropriate sequencer
///
/// This message should be sent by the admin public key
/// This admin key should sign the new sequencer public key
#[derive(NomReader, BinWriter, Clone, Debug, PartialEq, Eq)]
pub struct SetSequencer {
    nonce: u32,
    admin_public_key: PublicKey,
    sequencer_public_key: PublicKey,
}

#[derive(NomReader, BinWriter, Debug, Clone, Eq, PartialEq)]
pub enum SequencerMsg {
    Sequence(Sequence),
    SetSequencer(SetSequencer),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KernelMessage {
    Sequencer(UnverifiedSigned<Framed<SequencerMsg>>),
    DelayedMessage(Vec<u8>),
}

impl<P> NomReader for Framed<P>
where
    P: NomReader,
{
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        // Extract the rollup address from the framing protocol
        // 0x00 is the version of the framing protocol
        let (input, destination) = preceded(tag([0]), SmartRollupAddress::nom_read)(input)?;

        // Extract the payload
        let (remaining, payload) = P::nom_read(input)?;

        Ok((
            remaining,
            Framed {
                destination,
                payload,
            },
        ))
    }
}

impl<P> BinWriter for Framed<P>
where
    P: BinWriter,
{
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        // bytes of the framing protocol
        // 0x00 is the version of the framing protocol
        enc::put_byte(&0x00, output);

        // bytes of the rollup address
        self.destination.bin_write(output)?;

        // bytes of the payload
        self.payload.bin_write(output)
    }
}

impl NomReader for KernelMessage {
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        all_consuming(alt((
            all_consuming(map(
                preceded(tag([1]), UnverifiedSigned::<Framed<SequencerMsg>>::nom_read),
                KernelMessage::Sequencer,
            )),
            map(
                |bytes: &[u8]| Ok(([].as_slice(), bytes.to_vec())),
                KernelMessage::DelayedMessage,
            ),
        )))(input)
    }
}

impl BinWriter for KernelMessage {
    fn bin_write(&self, output: &mut Vec<u8>) -> enc::BinResult {
        match self {
            KernelMessage::Sequencer(sequencer_framed_msg) => {
                // external message tag
                enc::put_byte(&0x01, output);
                sequencer_framed_msg.bin_write(output)?;
            }
            KernelMessage::DelayedMessage(message) => enc::put_bytes(message, output),
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::message::{Bytes, Framed, SequencerMsg, UnverifiedSigned};

    use super::{KernelMessage, Sequence};
    use crate::message::SetSequencer;
    use tezos_crypto_rs::hash::{PublicKeyEd25519, SecretKeyEd25519, SeedEd25519, Signature};
    use tezos_crypto_rs::PublicKeySignatureVerifier;
    use tezos_data_encoding::enc::{self, BinWriter};
    use tezos_data_encoding::nom::NomReader;
    use tezos_smart_rollup_encoding::public_key::PublicKey;
    use tezos_smart_rollup_encoding::smart_rollup::SmartRollupAddress;

    /// Generate a public key and a secret key
    fn key_pair(seed: &str) -> (PublicKey, SecretKeyEd25519) {
        let (public_key, secret) = SeedEd25519::from_base58_check(seed)
            .expect("seed parsing should work")
            .keypair()
            .expect("make key pair should work");

        let public_key = PublicKey::Ed25519(public_key);
        (public_key, secret)
    }

    #[test]
    fn test_sequence_serialization() {
        let (_, secret) = key_pair("edsk3a5SDDdMWw3Q5hPiJwDXUosmZMTuKQkriPqY6UqtSfdLifpZbB");
        let signature = secret.sign([0x0]).expect("sign should work");

        let body = Framed {
            destination: SmartRollupAddress::from_b58check("sr1EzLeJYWrvch2Mhvrk1nUVYrnjGQ8A4qdb")
                .expect("decoding should work"),
            payload: SequencerMsg::Sequence(Sequence {
                nonce: 0,
                delayed_messages_prefix: 0,
                delayed_messages_suffix: 0,
                messages: Vec::default(),
            }),
        };

        let sequence = KernelMessage::Sequencer(UnverifiedSigned { body, signature });

        // Serializing
        let mut bin: Vec<u8> = Vec::new();
        sequence.bin_write(&mut bin).unwrap();

        // Deserializing
        let (_, msg_read) = KernelMessage::nom_read(&bin).expect("deserialization should work");

        assert_eq!(msg_read, sequence);
    }

    #[test]
    fn test_set_sequencer_serialization() {
        let (public_key, secret) =
            key_pair("edsk3a5SDDdMWw3Q5hPiJwDXUosmZMTuKQkriPqY6UqtSfdLifpZbB");
        let signature = secret.sign([0x0]).expect("sign should work");

        let body = Framed {
            destination: SmartRollupAddress::from_b58check("sr1EzLeJYWrvch2Mhvrk1nUVYrnjGQ8A4qdb")
                .expect("decoding should work"),
            payload: SequencerMsg::SetSequencer(SetSequencer {
                nonce: 0,
                admin_public_key: public_key.clone(),
                sequencer_public_key: public_key,
            }),
        };

        let sequence = KernelMessage::Sequencer(UnverifiedSigned { body, signature });

        // Serializing
        let mut bin: Vec<u8> = Vec::new();
        sequence.bin_write(&mut bin).unwrap();

        // Deserializing
        let (_, msg_read) = KernelMessage::nom_read(&bin).expect("deserialization should work");

        assert_eq!(msg_read, sequence);
    }

    #[test]
    fn test_user_message_serialization() {
        let sequence = KernelMessage::DelayedMessage(vec![0x01, 0x0, 0x01, 0x02, 0x02]);

        // Serializing
        let mut bin: Vec<u8> = Vec::new();
        sequence.bin_write(&mut bin).unwrap();

        // Deserializing
        let (_, msg_read) = KernelMessage::nom_read(&bin).expect("deserialization should work");

        assert_eq!(msg_read, sequence);
    }

    #[test]
    fn test_message_default() {
        let (_, secret) = key_pair("edsk3a5SDDdMWw3Q5hPiJwDXUosmZMTuKQkriPqY6UqtSfdLifpZbB");
        let signature = secret.sign([0x0]).expect("sign should work");

        let body = Framed {
            destination: SmartRollupAddress::from_b58check("sr1EzLeJYWrvch2Mhvrk1nUVYrnjGQ8A4qdb")
                .expect("decoding should work"),
            payload: SequencerMsg::Sequence(Sequence {
                nonce: 0,
                delayed_messages_prefix: 5,
                delayed_messages_suffix: 5,
                messages: Vec::default(),
            }),
        };

        let sequence = KernelMessage::Sequencer(UnverifiedSigned { body, signature });

        // Serializing
        let mut bin: Vec<u8> = Vec::new();
        sequence.bin_write(&mut bin).unwrap();
        enc::put_bytes(&[0x01, 0x02, 0x03, 0x04], &mut bin);

        // Deserializing
        let (remaining, msg_read) =
            KernelMessage::nom_read(&bin).expect("deserialization should work");

        assert!(remaining.is_empty());
        assert_eq!(msg_read, KernelMessage::DelayedMessage(bin))
    }

    /// Deserialize a string to a KernelMessage
    fn from_string(hex: &str) -> KernelMessage {
        let hex = hex::decode(hex).expect("valid hexadecimal");
        let (_, msg) = KernelMessage::nom_read(&hex).expect("deserialization should work");
        msg
    }

    #[test]
    fn test_deserialization_empty_sequence() {
        let kernel_message = from_string("01006227a8721213bd7ddb9b56227e3acb01161b1e67000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
        let rollup_address =
            SmartRollupAddress::from_b58check("sr1EzLeJYWrvch2Mhvrk1nUVYrnjGQ8A4qdb").unwrap();
        let sign = Signature::from_base58_check("edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQrUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q").unwrap();

        match kernel_message {
            KernelMessage::Sequencer(UnverifiedSigned {
                body:
                    Framed {
                        destination,
                        payload:
                            SequencerMsg::Sequence(Sequence {
                                nonce,
                                delayed_messages_prefix,
                                delayed_messages_suffix,
                                messages,
                            }),
                    },
                signature,
            }) => {
                assert_eq!(destination, rollup_address);
                assert_eq!(nonce, 0);
                assert_eq!(delayed_messages_prefix, 0);
                assert_eq!(delayed_messages_suffix, 0);
                assert_eq!(messages, vec![]);
                assert_eq!(signature, sign)
            }
            _ => panic!("Wrong message encoding"),
        }
    }

    #[test]
    fn test_deserialization_one_empty_message_sequence() {
        let kernel_message = from_string("01006227a8721213bd7ddb9b56227e3acb01161b1e6700000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
        let destination =
            SmartRollupAddress::from_b58check("sr1EzLeJYWrvch2Mhvrk1nUVYrnjGQ8A4qdb").unwrap();
        let signature = Signature::from_base58_check("edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQrUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q").unwrap();

        let expected = KernelMessage::Sequencer(UnverifiedSigned {
            body: Framed {
                destination,
                payload: SequencerMsg::Sequence(Sequence {
                    nonce: 0,
                    delayed_messages_prefix: 0,
                    delayed_messages_suffix: 0,
                    messages: vec![Bytes {
                        inner: b"".to_vec(),
                    }],
                }),
            },
            signature,
        });

        assert_eq!(kernel_message, expected);
    }

    #[test]
    fn test_deserialization_one_message_sequence() {
        let kernel_message = from_string("01006227a8721213bd7ddb9b56227e3acb01161b1e6700000000000000000000000000000000090000000568656c6c6f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
        let destination =
            SmartRollupAddress::from_b58check("sr1EzLeJYWrvch2Mhvrk1nUVYrnjGQ8A4qdb").unwrap();
        let signature = Signature::from_base58_check("edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQrUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q").unwrap();

        let expected = KernelMessage::Sequencer(UnverifiedSigned {
            body: Framed {
                destination,
                payload: SequencerMsg::Sequence(Sequence {
                    nonce: 0,
                    delayed_messages_prefix: 0,
                    delayed_messages_suffix: 0,
                    messages: vec![Bytes {
                        inner: b"hello".to_vec(),
                    }],
                }),
            },
            signature,
        });

        assert_eq!(kernel_message, expected);
    }

    #[test]
    fn test_deserialization_two_messages_sequence() {
        let kernel_message = from_string("01006227a8721213bd7ddb9b56227e3acb01161b1e6700000000000000000000000000000000120000000568656c6c6f00000005776f726c6400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
        let destination =
            SmartRollupAddress::from_b58check("sr1EzLeJYWrvch2Mhvrk1nUVYrnjGQ8A4qdb").unwrap();
        let signature = Signature::from_base58_check("edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQrUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q").unwrap();

        let expected = KernelMessage::Sequencer(UnverifiedSigned {
            body: Framed {
                destination,
                payload: SequencerMsg::Sequence(Sequence {
                    nonce: 0,
                    delayed_messages_prefix: 0,
                    delayed_messages_suffix: 0,
                    messages: vec![
                        Bytes {
                            inner: b"hello".to_vec(),
                        },
                        Bytes {
                            inner: b"world".to_vec(),
                        },
                    ],
                }),
            },
            signature,
        });

        assert_eq!(kernel_message, expected);
    }

    #[test]
    fn test_incorrect_sequence_signature() {
        let destination =
            SmartRollupAddress::from_b58check("sr1EzLeJYWrvch2Mhvrk1nUVYrnjGQ8A4qdb").unwrap();
        let nonce = 0;
        let delayed_messages_prefix = 0;
        let delayed_messages_suffix = 0;
        let messages = vec![];
        let signature = Signature::from_base58_check("edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQrUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q").unwrap();

        let message = UnverifiedSigned {
            body: Framed {
                destination,
                payload: SequencerMsg::Sequence(Sequence {
                    nonce,
                    delayed_messages_prefix,
                    delayed_messages_suffix,
                    messages,
                }),
            },
            signature: signature.clone(),
        };

        let hash = message.hash().expect("hash failure");

        let public_key = PublicKey::Ed25519(
            PublicKeyEd25519::from_base58_check(
                "edpkuDMUm7Y53wp4gxeLBXuiAhXZrLn8XB1R83ksvvesH8Lp8bmCfK",
            )
            .unwrap(),
        );

        let is_correct = public_key.verify_signature(&signature, &hash);

        assert!(matches!(is_correct, Ok(false) | Err(_)));
    }

    #[test]
    fn test_correct_sequence_signature() {
        let destination =
            SmartRollupAddress::from_b58check("sr1EzLeJYWrvch2Mhvrk1nUVYrnjGQ8A4qdb").unwrap();

        let seed = SeedEd25519::from_base58_check(
            "edsk3a5SDDdMWw3Q5hPiJwDXUosmZMTuKQkriPqY6UqtSfdLifpZbB",
        )
        .unwrap();

        let (public_key, secret_key) = seed.keypair().unwrap();

        let body = Framed {
            destination,
            payload: SequencerMsg::Sequence(Sequence {
                nonce: 0,
                delayed_messages_prefix: 0,
                delayed_messages_suffix: 0,
                messages: vec![],
            }),
        };

        let sequence =
            UnverifiedSigned::sign_ed25519(body, &secret_key).expect("error when signing body");

        let hash = sequence.hash().unwrap();
        let is_correct = public_key.verify_signature(&sequence.signature, &hash);
        assert!(matches!(is_correct, Ok(true)));
    }

    #[test]
    fn test_correct_non_empty_sequence_signature() {
        let destination =
            SmartRollupAddress::from_b58check("sr1EzLeJYWrvch2Mhvrk1nUVYrnjGQ8A4qdb").unwrap();

        let seed = SeedEd25519::from_base58_check(
            "edsk3a5SDDdMWw3Q5hPiJwDXUosmZMTuKQkriPqY6UqtSfdLifpZbB",
        )
        .unwrap();

        let (public_key, secret_key) = seed.keypair().unwrap();

        let body = Framed {
            destination,
            payload: SequencerMsg::Sequence(Sequence {
                nonce: 0,
                delayed_messages_prefix: 0,
                delayed_messages_suffix: 0,
                messages: vec![Bytes {
                    inner: b"hello".to_vec(),
                }],
            }),
        };

        let sequence =
            UnverifiedSigned::sign_ed25519(body, &secret_key).expect("error when signing body");

        let hash = sequence.hash().unwrap();
        let is_correct = public_key.verify_signature(&sequence.signature, &hash);
        assert!(matches!(is_correct, Ok(true)));
    }

    #[test]
    fn test_correct_signature_encoding() {
        let signed_msg = hex::decode("006227a8721213bd7ddb9b56227e3acb01161b1e67000000000000000000000000000000000f0000000b68656c6c6f20776f726c64bc682e5a009f3ee1dc1280d6b538aa53c626c29c5d5c528c8dd92092915aa24d414cc4f7ac81be8a4eb6a6046f9beea09c3c5a54374d68e6bfbad6cb51edc306").unwrap();
        let (_, signed_msg): (&[u8], UnverifiedSigned<Framed<SequencerMsg>>) =
            UnverifiedSigned::nom_read(&signed_msg).unwrap();

        let destination =
            SmartRollupAddress::from_b58check("sr1EzLeJYWrvch2Mhvrk1nUVYrnjGQ8A4qdb").unwrap();
        let seed = SeedEd25519::from_base58_check(
            "edsk3a5SDDdMWw3Q5hPiJwDXUosmZMTuKQkriPqY6UqtSfdLifpZbB",
        )
        .unwrap();
        let (public_key, _) = seed.keypair().unwrap();

        let body = Framed {
            destination,
            payload: SequencerMsg::Sequence(Sequence {
                nonce: 0,
                delayed_messages_prefix: 0,
                delayed_messages_suffix: 0,
                messages: vec![Bytes {
                    inner: b"hello world".to_vec(),
                }],
            }),
        };
        let hash = UnverifiedSigned::hash_body(&body).unwrap();

        let is_correct = public_key.verify_signature(&signed_msg.signature, &hash);

        assert!(matches!(is_correct, Ok(true)));
    }
}

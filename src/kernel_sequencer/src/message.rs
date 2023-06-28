// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{all_consuming, map},
    sequence::preceded,
};
use tezos_crypto_rs::hash::Signature;
use tezos_data_encoding::{
    enc::{self, BinResult, BinWriter},
    nom::{NomReader, NomResult},
};
use tezos_smart_rollup_encoding::public_key::PublicKey;
use tezos_smart_rollup_encoding::smart_rollup::SmartRollupAddress;

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
    signature: Signature,
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
    signature: Signature,
}

#[derive(NomReader, BinWriter, Debug, Clone, Eq, PartialEq)]
pub enum SequencerMsg {
    Sequence(Sequence),
    SetSequencer(SetSequencer),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KernelMessage {
    Sequencer(Framed<SequencerMsg>),
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
                preceded(tag([1]), Framed::<SequencerMsg>::nom_read),
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
    use crate::message::{Framed, SequencerMsg};

    use super::{KernelMessage, Sequence};
    use crate::message::SetSequencer;
    use tezos_crypto_rs::hash::{SecretKeyEd25519, SeedEd25519};
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

        let sequence = KernelMessage::Sequencer(Framed {
            destination: SmartRollupAddress::from_b58check("sr1EzLeJYWrvch2Mhvrk1nUVYrnjGQ8A4qdb")
                .expect("decoding should work"),
            payload: SequencerMsg::Sequence(Sequence {
                nonce: 0,
                delayed_messages_prefix: 0,
                delayed_messages_suffix: 0,
                messages: Vec::default(),
                signature,
            }),
        });

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

        let sequence = KernelMessage::Sequencer(Framed {
            destination: SmartRollupAddress::from_b58check("sr1EzLeJYWrvch2Mhvrk1nUVYrnjGQ8A4qdb")
                .expect("decoding should work"),
            payload: SequencerMsg::SetSequencer(SetSequencer {
                nonce: 0,
                admin_public_key: public_key.clone(),
                sequencer_public_key: public_key,
                signature,
            }),
        });

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

        let sequence = KernelMessage::Sequencer(Framed {
            destination: SmartRollupAddress::from_b58check("sr1EzLeJYWrvch2Mhvrk1nUVYrnjGQ8A4qdb")
                .expect("decoding should work"),
            payload: SequencerMsg::Sequence(Sequence {
                nonce: 0,
                signature,
                delayed_messages_prefix: 5,
                delayed_messages_suffix: 5,
                messages: Vec::default(),
            }),
        });

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
}

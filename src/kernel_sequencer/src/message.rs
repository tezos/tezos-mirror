// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use nom::{bytes::complete::tag, sequence::preceded};
use tezos_data_encoding::{
    enc::{self, BinResult, BinWriter},
    nom::{NomReader, NomResult},
};
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

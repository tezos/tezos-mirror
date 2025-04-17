// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use nom::bytes::complete::take;
use nom::combinator::map;
use nom::error::ParseError;
use nom::Finish;
use primitive_types::{H256, U256};
use tezos_crypto_rs::blake2b::digest_256;
use tezos_data_encoding::enc as tezos_enc;
use tezos_data_encoding::nom as tezos_nom;
use tezos_data_encoding::nom::error::DecodeError;
use tezos_data_encoding::nom::NomError;
use tezos_enc::{BinError, BinWriter};
use tezos_nom::{NomReader, NomResult};
use tezos_smart_rollup::types::Timestamp;

// WIP: This structure will evolve to look like Tezos block
#[derive(PartialEq, Debug)]
pub struct TezBlock {
    pub hash: H256,
    pub number: U256,
    pub timestamp: Timestamp,
    pub previous_hash: H256,
}

impl NomReader<'_> for TezBlock {
    fn nom_read(input: &'_ [u8]) -> NomResult<'_, Self> {
        let (remaining, hash) =
            map(take::<usize, &[u8], NomError>(32_usize), H256::from_slice)(input)?;

        let (remaining, number) = nom::number::complete::be_u32(remaining)?;
        let number = U256::from(number);

        let (remaining, previous_hash) =
            map(take::<usize, &[u8], NomError>(32_usize), H256::from_slice)(remaining)?;

        // Decode the timestamp
        let (remaining, timestamp) = nom::number::complete::be_i64(remaining)?;
        let timestamp = Timestamp::from(timestamp);

        Ok((
            remaining,
            Self {
                hash,
                number,
                timestamp,
                previous_hash,
            },
        ))
    }
}

impl BinWriter for TezBlock {
    // Encoded size for parameter were taken from this command:
    // `octez-codec describe block_header binary schema`
    fn bin_write(&self, output: &mut Vec<u8>) -> Result<(), BinError> {
        let Self {
            hash,
            number,
            timestamp,
            previous_hash,
        } = self;
        // Encode all block fields
        tezos_enc::put_bytes(&hash.to_fixed_bytes(), output);
        tezos_enc::u32(&number.as_u32(), output)?;
        tezos_enc::put_bytes(&previous_hash.to_fixed_bytes(), output);
        tezos_enc::i64(&timestamp.i64(), output)?;
        Ok(())
    }
}

impl TezBlock {
    pub fn genesis_block_hash() -> H256 {
        // This H256 comes from this b58 hash 'BLockGenesisGenesisGenesisGenesisGenesis1db77eJNeJ9'
        // That is the ghostnet genesis hash according to 'devtools/get_contracts/config.ml'
        H256::from_slice(
            &hex::decode(
                "8fcf233671b6a04fcf679d2a381c2544ea6c1ea29ba6157776ed8423e7c02934",
            )
            .unwrap(),
        )
    }

    // This function must be used on a TezBlock whose hash field is H256::zero()
    fn hash(&self) -> Result<H256, BinError> {
        let mut encoded_data = vec![];
        self.bin_write(&mut encoded_data)?;
        let hashed_data = digest_256(&encoded_data);
        Ok(H256::from_slice(&hashed_data))
    }

    pub fn new(
        number: U256,
        timestamp: Timestamp,
        previous_hash: H256,
    ) -> Result<Self, BinError> {
        let block = Self {
            hash: H256::zero(),
            number,
            timestamp,
            previous_hash,
        };
        Ok(Self {
            hash: block.hash()?,
            ..block
        })
    }

    pub fn to_bytes(&self) -> Result<Vec<u8>, BinError> {
        let mut output = vec![];
        self.bin_write(&mut output)?;
        Ok(output)
    }

    pub fn try_from_bytes(bytes: &[u8]) -> Result<Self, DecodeError<&[u8]>> {
        let (remaining, block) = Self::nom_read(bytes).finish()?;
        if !remaining.is_empty() {
            return Err(DecodeError::from_error_kind(
                remaining,
                nom::error::ErrorKind::NonEmpty,
            ));
        }
        Ok(block)
    }
}

#[cfg(test)]
mod tests {
    use primitive_types::U256;
    use tezos_smart_rollup::types::Timestamp;

    use super::TezBlock;

    pub fn block_roundtrip(block: TezBlock) {
        let bytes = block
            .to_bytes()
            .expect("Block encoding should have succeeded");
        let decoded_block =
            TezBlock::try_from_bytes(&bytes).expect("Block should be decodable");
        assert_eq!(block, decoded_block, "Roundtrip failed on {:?}", block)
    }

    fn dummy_tezblock() -> TezBlock {
        let number = U256::one();
        let timestamp = Timestamp::from(0);
        let previous_hash = TezBlock::genesis_block_hash();
        TezBlock::new(number, timestamp, previous_hash)
            .expect("Block creation should have succeeded")
    }

    #[test]
    fn test_block_rlp_roundtrip() {
        block_roundtrip(dummy_tezblock());
    }
}

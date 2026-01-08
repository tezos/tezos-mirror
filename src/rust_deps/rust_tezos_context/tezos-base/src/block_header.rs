use crate::{
    fitness::Fitness,
    limits::{BLOCK_HEADER_MAX_SIZE, BLOCK_HEADER_PROTOCOL_DATA_MAX_SIZE},
    timestamp::Timestamp,
};
use derive_builder::Builder;
use getset::{CopyGetters, Getters};
use serde::{Deserialize, Serialize};
use tezos_crypto_rs::hash::{BlockHash, BlockPayloadHash, ContextHash, OperationListListHash};
use tezos_crypto_rs::signature::Signature;
use tezos_data_encoding::{
    enc::BinWriter, encoding::HasEncoding, nom::NomReader, types::Bytes, types::SizedBytes,
};

pub type Level = i32;

#[derive(Clone, Debug, Getters, Serialize, Deserialize)]
pub struct Head {
    // TODO: TE-369 - Arc refactor
    /// BlockHash of head.
    #[get = "pub"]
    block_hash: BlockHash,
    /// Level of the head.
    #[get = "pub"]
    level: Level,
    /// Fitness of block
    #[get = "pub"]
    fitness: Fitness,
}

pub fn display_fitness(fitness: &Fitness) -> String {
    fitness.to_string()
}

impl Head {
    pub fn new(block_hash: BlockHash, level: Level, fitness: Fitness) -> Self {
        Self {
            block_hash,
            level,
            fitness,
        }
    }

    pub fn to_debug_info(&self) -> (String, Level, String) {
        (
            self.block_hash.to_base58_check(),
            self.level,
            display_fitness(&self.fitness),
        )
    }
}

impl From<Head> for BlockHash {
    fn from(h: Head) -> Self {
        h.block_hash
    }
}

impl From<Head> for Level {
    fn from(h: Head) -> Self {
        h.level
    }
}

// -----------------------------------------------------------------------------------------------
#[derive(
    Serialize,
    Deserialize,
    Eq,
    PartialEq,
    Clone,
    Builder,
    Getters,
    CopyGetters,
    HasEncoding,
    NomReader,
    BinWriter,
)]
#[encoding(bounded = "BLOCK_HEADER_MAX_SIZE")]
pub struct BlockHeader {
    #[get_copy = "pub"]
    #[encoding(builtin = "Int32")]
    level: Level,
    #[get_copy = "pub"]
    proto: u8,
    #[get = "pub"]
    predecessor: BlockHash,
    #[get_copy = "pub"]
    timestamp: Timestamp,
    #[get_copy = "pub"]
    validation_pass: u8,
    #[get = "pub"]
    operations_hash: OperationListListHash,
    #[get = "pub"]
    fitness: Fitness,
    #[get = "pub"]
    context: ContextHash,

    #[get = "pub"]
    #[encoding(bounded = "BLOCK_HEADER_PROTOCOL_DATA_MAX_SIZE")]
    protocol_data: Bytes,

    #[get = "pub"]
    #[serde(skip)]
    #[builder(default)]
    #[encoding(hash)]
    hash: EncodingHash,
}

impl BlockHeader {
    pub fn payload_hash(&self) -> Option<BlockPayloadHash> {
        let len = 32;
        let data: &[u8] = self.protocol_data.as_ref();
        if data.len() < 32 {
            return None;
        }
        (&data[0..len]).try_into().ok()
    }

    pub fn payload_round(&self) -> Option<i32> {
        let pos = 32;
        let data: &[u8] = self.protocol_data.as_ref();
        if data.len() < pos + 4 {
            return None;
        }
        Some(i32::from_be_bytes(data[pos..(pos + 4)].try_into().unwrap()))
    }

    pub fn set_proof_of_work_nonce(&mut self, nonce: &SizedBytes<8>) {
        let mut data: Vec<_> = std::mem::take(&mut self.protocol_data).into();
        let pos = 36;
        data[pos..(pos + 8)].clone_from_slice(nonce.as_ref());
        self.protocol_data = data.into();
    }

    pub fn set_signature(&mut self, signature: &Signature) {
        let mut data: Vec<_> = std::mem::take(&mut self.protocol_data).into();
        // remove old signature bytes.
        let len = data.len().saturating_sub(64);
        data.resize(len, 0);
        data.extend_from_slice(signature.as_ref());
        self.protocol_data = data.into();
    }
}

impl std::fmt::Debug for BlockHeader {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BlockHeader")
            .field("level", &self.level)
            .field("proto", &self.proto)
            .field("predecessor", &self.predecessor)
            .field("timestamp", &self.timestamp)
            .field("validation_pass", &self.validation_pass)
            .field("operations_hash", &self.operations_hash)
            .field("fitness", &self.fitness)
            .field("context", &self.context)
            .field("protocol_data", &self.protocol_data)
            .finish()
    }
}

/// Optional 256-bit digest of encoded data
/// TODO https://viablesystems.atlassian.net/browse/TE-675
#[derive(Clone, Debug, Eq, Default)]
pub struct EncodingHash(pub Option<Vec<u8>>);

impl PartialEq for EncodingHash {
    fn eq(&self, other: &Self) -> bool {
        match (self.0.as_ref(), other.0.as_ref()) {
            (Some(v1), Some(v2)) => v1 == v2,
            _ => true,
        }
    }
}

impl EncodingHash {
    pub fn as_ref(&self) -> Option<&Vec<u8>> {
        self.0.as_ref()
    }
}

impl From<Vec<u8>> for EncodingHash {
    fn from(hash: Vec<u8>) -> Self {
        Self(Some(hash))
    }
}

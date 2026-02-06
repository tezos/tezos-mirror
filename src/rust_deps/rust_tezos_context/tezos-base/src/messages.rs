use getset::{CopyGetters, Getters};

use nom::{
    Finish,
    branch::alt,
    bytes::complete::{tag, take},
    combinator::{all_consuming, complete, flat_map, into, map, success, verify},
    multi::many_till,
    sequence::preceded,
};

use serde::{Deserialize, Serialize};

use tezos_crypto_rs::{
    blake2b::{self, Blake2bError},
    hash::{BlockHash, HashType},
};

use tezos_data_encoding::{
    binary_reader::BinaryReaderError,
    binary_writer::BinaryWriterError,
    enc::{BinError, BinWriter},
    encoding::{Encoding, HasEncoding},
    has_encoding,
    nom::{NomError, NomInput, NomReader, NomResult, error::convert_error},
    types::Bytes,
};

use thiserror::Error;

/// Trait for reading a binary message.
pub trait BinaryRead: Sized {
    /// Create new struct from bytes.
    fn from_bytes<B: AsRef<[u8]>>(buf: B) -> Result<Self, BinaryReaderError>;
}

/// Trait for writing a binary message.
pub trait BinaryWrite {
    /// Produce bytes from the struct.
    fn as_bytes(&self) -> Result<Vec<u8>, BinaryWriterError>;
}

use super::limits::{OPERATION_LIST_MAX_SIZE, OPERATION_MAX_SIZE};

pub const MAX_PASS_MERKLE_DEPTH: usize = 3;

/// Message that can be both encoded and decoded into binary format.
pub trait BinaryMessage: BinaryRead + BinaryWrite {}

impl<T: BinaryRead + BinaryWrite> BinaryMessage for T {}

impl<T> BinaryWrite for T
where
    T: BinWriter,
{
    #[inline]
    fn as_bytes(&self) -> Result<Vec<u8>, BinaryWriterError> {
        let mut res = Vec::new();
        self.bin_write(&mut res)?;
        Ok(res)
    }
}

impl<T> BinaryRead for T
where
    T: for<'a> tezos_data_encoding::nom::NomReader<'a> + Sized + 'static,
{
    #[inline]
    fn from_bytes<B: AsRef<[u8]>>(buf: B) -> Result<Self, BinaryReaderError> {
        let input = buf.as_ref();
        all_consuming_complete_input(T::nom_read, input)
    }
}

/// Applies nom parser `parser` to the input, assuming that input is complete and
/// ensuring that it is fully consumed.
pub fn all_consuming_complete_input<'a, T: 'static>(
    parser: impl FnMut(NomInput<'a>) -> NomResult<'a, T>,
    input: NomInput<'a>,
) -> Result<T, BinaryReaderError> {
    // - `all_consuming` combinator ensures that all input is consumed,
    //   reporting error otherwise.
    // - `complete` combinator assumes that underlying parsing has complete input,
    //   converting ``Incomplete` into `Error`.
    // - `finish` flattens `nom::Err` into underlying error for easier processing.
    all_consuming(complete(parser))(input)
        .finish()
        .map(|(bytes, output)| {
            debug_assert!(
                bytes.is_empty(),
                "Successful parsing should consume all bytes"
            );
            output
        })
        .map_err(|error| map_nom_error(input, error))
}

/// Maps input and nom error into printable version.
pub(crate) fn map_nom_error(input: NomInput, error: NomError) -> BinaryReaderError {
    if let Some(unknown_tag) = error.get_unknown_tag() {
        BinaryReaderError::UnknownTag(unknown_tag.clone())
    } else {
        BinaryReaderError::Error(convert_error(input, error))
    }
}

/// Message hash error
#[derive(Debug, Error)]
pub enum MessageHashError {
    #[error("Message serialization error: {error}")]
    SerializationError { error: BinaryWriterError },
    #[error("Error constructing hash")]
    FromBytesError {
        error: tezos_crypto_rs::hash::FromBytesError,
    },
    #[error("Blake2b digest error")]
    Blake2bError,
}

impl From<BinaryWriterError> for MessageHashError {
    fn from(error: BinaryWriterError) -> Self {
        MessageHashError::SerializationError { error }
    }
}

impl From<tezos_crypto_rs::hash::FromBytesError> for MessageHashError {
    fn from(error: tezos_crypto_rs::hash::FromBytesError) -> Self {
        MessageHashError::FromBytesError { error }
    }
}

impl From<Blake2bError> for MessageHashError {
    fn from(_: Blake2bError) -> Self {
        MessageHashError::Blake2bError
    }
}

/// Trait for getting hash of the message.
pub trait MessageHash {
    fn message_hash(&self) -> Result<[u8; 32], MessageHashError>;
    fn message_typed_hash<H>(&self) -> Result<H, MessageHashError>
    where
        H: tezos_crypto_rs::hash::HashTrait<32>;
}

impl<T: BinaryMessage> MessageHash for T {
    #[inline]
    fn message_hash(&self) -> Result<[u8; 32], MessageHashError> {
        let bytes = self.as_bytes()?;
        Ok(blake2b::digest_256(&bytes))
    }

    #[inline]
    fn message_typed_hash<H>(&self) -> Result<H, MessageHashError>
    where
        H: tezos_crypto_rs::hash::HashTrait<32>,
    {
        let bytes = self.as_bytes()?;
        Ok(H::from(blake2b::digest_256(&bytes)))
    }
}

#[derive(
    Clone,
    Serialize,
    Deserialize,
    Debug,
    Eq,
    PartialEq,
    CopyGetters,
    Getters,
    HasEncoding,
    NomReader,
    BinWriter,
)]
pub struct OperationsForBlock {
    #[get = "pub"]
    hash: BlockHash,
    #[get_copy = "pub"]
    validation_pass: i8,
}

impl OperationsForBlock {
    pub fn new(hash: BlockHash, validation_pass: i8) -> Self {
        OperationsForBlock {
            hash,
            validation_pass,
        }
    }

    /// alternative getter because .hash() causes problem with hash() method from Hash trait
    #[inline(always)]
    pub fn block_hash(&self) -> &BlockHash {
        &self.hash
    }
}

#[derive(
    Clone, Serialize, Deserialize, Debug, Eq, PartialEq, Getters, HasEncoding, NomReader, BinWriter,
)]
pub struct OperationsForBlocksMessage {
    #[get = "pub"]
    operations_for_block: OperationsForBlock,
    #[get = "pub"]
    operation_hashes_path: Path,
    #[get = "pub"]
    #[encoding(bounded = "OPERATION_LIST_MAX_SIZE", list, dynamic)]
    operations: Vec<Operation>,
}

impl OperationsForBlocksMessage {
    pub fn new(
        operations_for_block: OperationsForBlock,
        operation_hashes_path: Path,
        operations: Vec<Operation>,
    ) -> Self {
        OperationsForBlocksMessage {
            operations_for_block,
            operation_hashes_path,
            operations,
        }
    }

    pub fn as_operations(self) -> Vec<Operation> {
        self.operations
    }
}

impl From<OperationsForBlocksMessage> for Vec<Operation> {
    fn from(msg: OperationsForBlocksMessage) -> Self {
        msg.operations
    }
}

// -----------------------------------------------------------------------------------------------
#[derive(Clone, Serialize, Deserialize, Eq, PartialEq, Debug, Getters)]
pub struct PathRight {
    #[get = "pub"]
    left: [u8; 32],
}

impl PathRight {
    pub fn new(left: [u8; 32]) -> Self {
        Self { left }
    }
}

// -----------------------------------------------------------------------------------------------
#[derive(Clone, Serialize, Deserialize, Eq, PartialEq, Debug, Getters)]
pub struct PathLeft {
    #[get = "pub"]
    right: [u8; 32],
}

impl PathLeft {
    pub fn new(right: [u8; 32]) -> Self {
        Self { right }
    }
}

// -----------------------------------------------------------------------------------------------
#[derive(Clone, Serialize, Deserialize, Eq, PartialEq, Debug)]
pub enum PathItem {
    Right(PathRight),
    Left(PathLeft),
}

impl PathItem {
    pub fn right(left: [u8; 32]) -> PathItem {
        PathItem::Right(PathRight::new(left))
    }
    pub fn left(right: [u8; 32]) -> PathItem {
        PathItem::Left(PathLeft::new(right))
    }
}

// -----------------------------------------------------------------------------------------------
#[derive(Clone, Eq, PartialEq, Debug, Deserialize)]
pub struct Path(pub Vec<PathItem>);

#[derive(
    Serialize, Deserialize, Eq, PartialEq, Debug, Getters, Clone, HasEncoding, NomReader, BinWriter,
)]
pub struct OperationMessage {
    #[get = "pub"]
    operation: Operation,
}

impl From<Operation> for OperationMessage {
    fn from(operation: Operation) -> Self {
        Self { operation }
    }
}

impl From<OperationMessage> for Operation {
    fn from(msg: OperationMessage) -> Self {
        msg.operation
    }
}

#[derive(
    Clone, Serialize, Deserialize, Eq, PartialEq, Debug, HasEncoding, NomReader, BinWriter, Getters,
)]
pub struct Operation {
    #[get = "pub"]
    branch: BlockHash,
    #[encoding(bounded = "OPERATION_MAX_SIZE")]
    #[get = "pub"]
    data: Bytes,
}

impl Path {
    pub fn op() -> Self {
        Path(Vec::new())
    }
}

/// Manual serializization ensures that path depth does not exceed max value
impl Serialize for Path {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if self.0.len() > MAX_PASS_MERKLE_DEPTH {
            use serde::ser::Error;
            return Err(Error::custom(format!(
                "Path size exceedes its boundary {} for encoding",
                MAX_PASS_MERKLE_DEPTH
            )));
        }
        use serde::ser::SerializeSeq;
        let mut seq = serializer.serialize_seq(Some(self.0.len()))?;
        self.0.iter().try_for_each(|i| seq.serialize_element(i))?;
        seq.end()
    }
}

has_encoding!(Path, PATH_ENCODING, { Encoding::Custom });

#[derive(Clone)]
enum DecodePathNode {
    Left,
    Right([u8; 32]),
}

impl From<[u8; 32]> for DecodePathNode {
    fn from(bytes: [u8; 32]) -> Self {
        DecodePathNode::Right(bytes)
    }
}

fn hash(input: &[u8]) -> NomResult<[u8; 32]> {
    map(
        take(HashType::OperationListListHash.size()),
        |slice: &[u8]| slice.try_into().expect("correct hash size"),
    )(input)
}

fn path_left(input: &[u8]) -> NomResult<DecodePathNode> {
    preceded(tag(0xf0u8.to_be_bytes()), success(DecodePathNode::Left))(input)
}

fn path_right(input: &[u8]) -> NomResult<DecodePathNode> {
    preceded(tag(0x0fu8.to_be_bytes()), into(hash))(input)
}

fn path_op(input: &[u8]) -> NomResult<()> {
    preceded(tag(0x00u8.to_be_bytes()), success(()))(input)
}

fn path_complete(nodes: Vec<DecodePathNode>) -> impl FnMut(&[u8]) -> NomResult<Path> {
    move |mut input| {
        let mut res = Vec::new();
        for node in nodes.clone().into_iter().rev() {
            match node {
                DecodePathNode::Left => {
                    let (i, h) = hash(input)?;
                    res.push(PathItem::left(h));
                    input = i;
                }
                DecodePathNode::Right(h) => res.push(PathItem::right(h)),
            }
        }
        res.reverse();
        Ok((input, Path(res)))
    }
}

impl NomReader<'_> for Path {
    fn nom_read(bytes: &[u8]) -> tezos_data_encoding::nom::NomResult<Self> {
        flat_map(
            verify(
                map(many_till(alt((path_left, path_right)), path_op), |(v, _)| v),
                |nodes: &Vec<DecodePathNode>| MAX_PASS_MERKLE_DEPTH >= nodes.len(),
            ),
            path_complete,
        )(bytes)
    }
}

fn bin_write_path_items(
    path_items: &[PathItem],
    out: &mut Vec<u8>,
) -> tezos_data_encoding::enc::BinResult {
    match path_items.split_first() {
        None => {
            tezos_data_encoding::enc::u8(&0x00, out)?;
        }
        Some((PathItem::Left(left), rest)) => {
            tezos_data_encoding::enc::u8(&0xf0, out)?;
            bin_write_path_items(rest, out)?;
            tezos_data_encoding::enc::put_bytes(&left.right, out);
        }
        Some((PathItem::Right(right), rest)) => {
            tezos_data_encoding::enc::u8(&0x0f, out)?;
            tezos_data_encoding::enc::put_bytes(&right.left, out);
            bin_write_path_items(rest, out)?;
        }
    }
    Ok(())
}

impl BinWriter for Path {
    fn bin_write(&self, out: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        if self.0.len() > MAX_PASS_MERKLE_DEPTH {
            return Err(BinError::custom(format!(
                "Merkle path size {} is greater than max {}",
                self.0.len(),
                MAX_PASS_MERKLE_DEPTH
            )));
        }
        bin_write_path_items(self.0.as_slice(), out)
    }
}

impl Operation {
    pub fn new(branch: BlockHash, data: Bytes) -> Self {
        Self { branch, data }
    }
}

// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024-2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! ## Module for handling proofs
//!
//! - Serialise & Deserialise a [`Proof`]
//!
//!   Structure of serialisation:
//!   * Final hash state
//!   * Tags which dictate the shape of the proof (a partial Merkle tree)
//!   * Leaf contents
//!
//! - Convert [`super::merkle::MerkleTree`] to [`MerkleProof`]

use itertools::Itertools;

use super::tree::ModifyResult;
use super::tree::Tree;
use super::tree::impl_modify_map_collect;
use crate::state_backend::hash::Hash;
use crate::storage::DIGEST_SIZE;
use crate::storage::HashError;

pub mod deserialise_owned;
pub mod deserialiser;

/// Structure of a proof transitioning from state A to state B.
///
/// The proof needs to be able to:
/// - Contain enough information to be able to run a single step on A
/// - Obtain the hash of the state after the step
#[derive(Clone, Debug, PartialEq)]
pub struct Proof {
    /// State of the final state B
    final_state_hash: Hash,
    /// Partial Merkle tree representation of the initial state
    partial_tree: MerkleProof,
}

impl Proof {
    /// Create a proof from a partial Merkle tree and the final state hash.
    /// The initial state hash is the root hash of the partial Merkle tree so it is not included separately.
    pub fn new(proof: MerkleProof, final_state_hash: Hash) -> Self {
        Self {
            final_state_hash,
            partial_tree: proof,
        }
    }

    /// Get the proof tree.
    pub fn tree(&self) -> &MerkleProof {
        &self.partial_tree
    }

    /// Get the initial state hash of the proof.
    pub fn initial_state_hash(&self) -> Hash {
        self.partial_tree
            .root_hash()
            .expect("Computing the root hash of the Merkle proof should not fail")
    }

    /// Get the final state hash of the proof.
    pub fn final_state_hash(&self) -> Hash {
        self.final_state_hash
    }
}

/// Tag of a node
const TAG_NODE: u8 = 0b00;
/// Tag of a blind leaf
const TAG_BLIND: u8 = 0b10;
/// Tag of a read leaf
const TAG_READ: u8 = 0b11;
/// Bitmask for tags
const TAG_MASK: u8 = 0b11;

/// Merkle proof tree structure.
///
/// Leaves can be read and/or written to.
/// If a read was done, the content will be stored in the proof.
/// If a write was done, the written content is not necessary since the semantics of running the step will
/// deduce the written contents.
///
/// A proof will have the shape of a subtree of a [`MerkleTree`].
/// The structure of the full [`MerkleTree`] is known statically (since it represents the whole state of the PVM)
/// so the number of children of a node and the sizes of the leaves
/// do not need to be stored in either the proof or its encoding.
///
/// [`MerkleTree`]: super::merkle::MerkleTree
pub type MerkleProof = Tree<MerkleProofLeaf>;

/// Type used to describe the leaves of a [`MerkleProof`].
/// For more details see the documentation of [`MerkleProof`].
#[derive(Clone, Debug, PartialEq)]
pub enum MerkleProofLeaf {
    /// A leaf that is not read. It may be written.
    /// Contains the hash of the contents from initial state.
    ///
    /// Note: a blinded leaf can correspond to a blinded subtree
    /// in a [`super::merkle::MerkleTree`] due to compression.
    Blind(Hash),
    /// A leaf that is read. It may also be written.
    /// Contains the read data from the initial state.
    /// The initial hash can be deduced based on the read data.
    Read(Vec<u8>),
}

impl MerkleProof {
    /// Return a 2-bit tag for the variant of the node in the proof.
    pub fn to_raw_tag(&self) -> Tag {
        match self {
            MerkleProof::Node(_) => Tag::Node,
            MerkleProof::Leaf(MerkleProofLeaf::Blind(_)) => Tag::Leaf(LeafTag::Blind),
            MerkleProof::Leaf(MerkleProofLeaf::Read(_)) => Tag::Leaf(LeafTag::Read),
        }
    }

    /// Compute the root hash of the Merkle proof.
    pub fn root_hash(&self) -> Result<Hash, HashError> {
        impl_modify_map_collect(
            self,
            |subtree| {
                Ok(match subtree {
                    Tree::Node(vec) => ModifyResult::NodeContinue((), vec.iter().collect()),
                    Tree::Leaf(data) => ModifyResult::LeafStop(data),
                })
            },
            |leaf| match leaf {
                MerkleProofLeaf::Blind(hash) => Ok(*hash),
                MerkleProofLeaf::Read(data) => Hash::blake2b_hash_bytes(data.as_slice()),
            },
            |(), leaves| Hash::combine(&leaves),
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Tag {
    Node,
    Leaf(LeafTag),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LeafTag {
    Blind,
    Read,
}

impl From<&MerkleProof> for Tag {
    fn from(value: &MerkleProof) -> Self {
        match value {
            MerkleProof::Node(_) => Tag::Node,
            MerkleProof::Leaf(MerkleProofLeaf::Blind(_)) => Tag::Leaf(LeafTag::Blind),
            MerkleProof::Leaf(MerkleProofLeaf::Read(_)) => Tag::Leaf(LeafTag::Read),
        }
    }
}

impl TryFrom<u8> for Tag {
    type Error = DeserialiseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            TAG_NODE => Ok(Self::Node),
            TAG_BLIND => Ok(Self::Leaf(LeafTag::Blind)),
            TAG_READ => Ok(Self::Leaf(LeafTag::Read)),
            _ => Err(DeserialiseError::InvalidTag),
        }
    }
}

impl From<Tag> for u8 {
    fn from(value: Tag) -> Self {
        match value {
            Tag::Node => TAG_NODE,
            Tag::Leaf(leaf_tag) => match leaf_tag {
                LeafTag::Blind => TAG_BLIND,
                LeafTag::Read => TAG_READ,
            },
        }
    }
}

impl From<LeafTag> for Tag {
    fn from(value: LeafTag) -> Self {
        Tag::Leaf(value)
    }
}

fn serialise_raw_tags(raw_tags: impl Iterator<Item = Tag>) -> Vec<u8> {
    // Tag serialisation to bytes depends on the number of bits required to hold a raw tag
    // Here, a raw tag is 2 bits wide, hence we use 8 / 2 = 4 chunks
    raw_tags
        .chunks(4)
        .into_iter()
        .map(|chunk| {
            chunk.zip([6, 4, 2, 0]).fold(0, |acc: u8, (tag, offset)| {
                let bits = u8::from(tag) << offset;
                acc | bits
            })
        })
        .collect()
}

fn iter_raw_tags(proof: &MerkleProof) -> impl Iterator<Item = Tag> + '_ {
    proof.subtree_iterator().map(|subtree| subtree.to_raw_tag())
}

fn serialise_proof_values(proof: &MerkleProof) -> impl Iterator<Item = u8> + '_ {
    proof
        .subtree_iterator()
        .flat_map(move |subtree| match subtree {
            MerkleProof::Leaf(MerkleProofLeaf::Read(vec)) => vec,
            MerkleProof::Leaf(MerkleProofLeaf::Blind(hash)) => hash.as_ref(),
            MerkleProof::Node(_) => &[],
        })
        .copied()
}

/// Serialise a Merkle proof to an array of bytes.
///
/// In the encoding, lengths are not necessary, but tags are,
/// since the tags depend on runtime information and events
pub fn serialise_proof(proof: &Proof) -> impl Iterator<Item = u8> + '_ {
    // Here we collect the `iter_raw_tags` iterator to be able to chunkify it and transform it
    // by compressing the tags to a byte-array, fully utilising the bytes capacity.
    let final_hash_encoding = proof.final_state_hash.as_ref().iter().copied();
    let tags_encoding = serialise_raw_tags(iter_raw_tags(&proof.partial_tree)).into_iter();
    let nodes_encoding = serialise_proof_values(&proof.partial_tree);

    final_hash_encoding
        .chain(tags_encoding)
        .chain(nodes_encoding)
}

#[derive(Debug, PartialEq)]
pub enum DeserialiseError {
    ExpectedLeaf,
    InvalidTag,
    NotEnoughBytes,
    TooManyBytes,
}

/// Tree structure specifying the shape of a Merkle tree.
///
/// Leaves contain a [`usize`] representing the amount of bytes
/// held in the Merkle tree leaf as raw data.
pub type Shape = Tree<usize>;

impl Shape {
    /// Get total amount of bytes in the whole [`Shape`] held in leaves.
    fn total_leaf_bytes(&self) -> usize {
        match self {
            Tree::Node(vec) => vec.iter().map(Shape::total_leaf_bytes).sum(),
            Tree::Leaf(size) => *size,
        }
    }
}

fn deserialise_final_hash_encoding(
    bytes: &mut impl Iterator<Item = u8>,
) -> Result<Hash, DeserialiseError> {
    let mut digest = [0; DIGEST_SIZE];
    for b in digest.iter_mut() {
        match bytes.next() {
            None => return Err(DeserialiseError::NotEnoughBytes),
            Some(byte) => *b = byte,
        }
    }
    Ok(Hash::from(digest))
}

fn deserialise_merkle_proof_tags(
    bytes: &mut impl Iterator<Item = u8>,
    full_shape: Shape,
) -> Result<Tree<(usize, LeafTag)>, DeserialiseError> {
    let mut buffered_raw_tags = vec![];
    let mut get_next_tag = || -> Result<Tag, DeserialiseError> {
        if buffered_raw_tags.is_empty() {
            let byte = bytes.next().ok_or(DeserialiseError::NotEnoughBytes)?;
            buffered_raw_tags = [0, 2, 4, 6]
                .map(|offset| (byte >> offset) & TAG_MASK)
                .to_vec();
        }

        debug_assert!(!buffered_raw_tags.is_empty());

        let raw_tag = buffered_raw_tags.pop().unwrap();
        Tag::try_from(raw_tag)
    };

    full_shape.modify_shape(&mut |subtree| {
        let tag = get_next_tag()?;

        match tag {
            Tag::Leaf(leaf_tag) => match subtree {
                Tree::Node(_) => Ok(ModifyResult::LeafStop((
                    subtree.total_leaf_bytes(),
                    leaf_tag,
                ))),
                Tree::Leaf(size) => Ok(ModifyResult::LeafStop((size, leaf_tag))),
            },
            Tag::Node => match subtree {
                Tree::Node(children) => Ok(ModifyResult::NodeContinue((), children)),
                Tree::Leaf(_) => Err(DeserialiseError::ExpectedLeaf),
            },
        }
    })
}

fn deserialise_merkle_proof_values(
    bytes: &mut impl Iterator<Item = u8>,
    shape: Tree<(usize, LeafTag)>,
) -> Result<MerkleProof, DeserialiseError> {
    shape.map(&mut |(size, tag)| {
        Ok(match tag {
            LeafTag::Blind => MerkleProofLeaf::Blind(deserialise_final_hash_encoding(bytes)?),
            LeafTag::Read => {
                let mut data = Vec::with_capacity(size);
                for _ in 0..size {
                    data.push(bytes.next().ok_or(DeserialiseError::NotEnoughBytes)?);
                }
                MerkleProofLeaf::Read(data)
            }
        })
    })
}

fn deserialise_merkle_proof(
    bytes: &mut impl Iterator<Item = u8>,
    full_shape: Shape,
) -> Result<MerkleProof, DeserialiseError> {
    let proof_shape = deserialise_merkle_proof_tags(bytes, full_shape)?;
    deserialise_merkle_proof_values(bytes, proof_shape)
}

/// Given a [`Iterator<Item = u8>`], parse the contents into a [`Proof`].
///
/// A [`Shape`] of the full Merkle tree has to be provided to account for the proof being
/// only a subtree of the actual full Merkle tree.
/// Knowing the initial shape statically helps improve size of the encoding by
/// not needing to know arity of different subtrees / different leaf sizes.
///
/// Ideally, the shape should be a compile time type/argument.
/// However, for ease of implementation currently it is given in the form of a [`Shape`] argument
pub fn deserialise_proof(
    mut bytes: impl Iterator<Item = u8>,
    full_merkle_tree_shape: Shape,
) -> Result<Proof, DeserialiseError> {
    let final_hash = deserialise_final_hash_encoding(&mut bytes)?;
    let merkle_proof = deserialise_merkle_proof(&mut bytes, full_merkle_tree_shape)?;

    // we expect the iterator to be fully consumed when deserialising a proof
    if bytes.next().is_some() {
        return Err(DeserialiseError::TooManyBytes);
    }

    Ok(Proof::new(merkle_proof, final_hash))
}

#[cfg(test)]
mod tests {
    use proptest::proptest;
    use rand::Fill;
    use rand::thread_rng;

    use super::DeserialiseError;
    use super::MerkleProof;
    use super::MerkleProofLeaf;
    use super::Shape;
    use super::TAG_BLIND;
    use super::TAG_NODE;
    use super::TAG_READ;
    use super::serialise_proof;
    use crate::state_backend::proof_backend::proof::Proof;
    use crate::state_backend::proof_backend::proof::deserialise_proof;
    use crate::storage::DIGEST_SIZE;
    use crate::storage::Hash;

    /// Utility struct that computes the bounds of a [`MerkleProof`] serialisation
    /// based on total number of nodes in the tree and total size of raw data in the leafs.
    struct SerialisationBound {
        nodes_count: u64,
        content_size: u64,
    }

    impl SerialisationBound {
        /// Compute equivalent bound of a node having the children given from the argument.
        fn node_combine(children: Vec<SerialisationBound>) -> SerialisationBound {
            children
                .into_iter()
                // The base fold value has node count 1 to account for the node combining the children
                .fold(
                    SerialisationBound {
                        nodes_count: 1,
                        content_size: 0,
                    },
                    |a, b| SerialisationBound {
                        nodes_count: a.nodes_count + b.nodes_count,
                        content_size: a.content_size + b.content_size,
                    },
                )
        }

        fn expected_serialisation_length(&self) -> usize {
            let hashes_size = 2 * DIGEST_SIZE as u64;
            // div_ceil
            let tags_size = self.nodes_count / 4 + (self.nodes_count % 4 != 0) as u64;
            (hashes_size + tags_size + self.content_size) as usize
        }

        /// Generate the serialisation bound for a [`MerkleProof`] leaf.
        fn from_merkle_leaf(leaf: &MerkleProof) -> Self {
            match leaf {
                MerkleProof::Node(_) => panic!("Expected a Merkle proof leaf"),
                MerkleProof::Leaf(MerkleProofLeaf::Blind(hash)) => Self {
                    nodes_count: 1,
                    content_size: hash.as_ref().len() as u64,
                },
                MerkleProof::Leaf(MerkleProofLeaf::Read(data)) => Self {
                    nodes_count: 1,
                    content_size: data.len() as u64,
                },
            }
        }
    }

    fn generate_rand_leaf() -> MerkleProof {
        let is_leaf_read = rand::random::<bool>();
        let length: usize = rand::random::<usize>() % 100 + 1;

        let mut raw_array = vec![0; length];
        raw_array.try_fill(&mut rand::thread_rng()).unwrap();
        let blind_hash: Hash = Hash::blake2b_hash_bytes(&raw_array).unwrap();

        match is_leaf_read {
            true => MerkleProof::Leaf(MerkleProofLeaf::Read(raw_array)),
            false => MerkleProof::Leaf(MerkleProofLeaf::Blind(blind_hash)),
        }
    }

    fn check_serialisation(tree: MerkleProof, tree_correct_bytes: &[u8]) {
        let final_state_hash = Hash::blake2b_hash_bytes(&rand::random::<[u8; 10]>()).unwrap();
        let proof = Proof::new(tree, final_state_hash);

        let ser_bytes: Vec<u8> = serialise_proof(&proof).collect();
        assert_eq!(
            ser_bytes.as_slice(),
            &[final_state_hash.as_ref(), tree_correct_bytes].concat()
        );
    }

    #[test]
    fn serialise_leaf_trees() {
        // Check serialisation of all leaf variants

        let raw_array: [u8; 10] = rand::random();

        let rleaf = MerkleProof::Leaf(MerkleProofLeaf::Read(raw_array.to_vec()));
        check_serialisation(rleaf, &[&[TAG_READ << 6], raw_array.as_slice()].concat());

        let hash = Hash::blake2b_hash_bytes(&raw_array).unwrap();
        check_serialisation(
            MerkleProof::Leaf(MerkleProofLeaf::Blind(hash)),
            &[&[TAG_BLIND << 6], hash.as_ref()].concat(),
        );
    }

    #[test]
    fn serialise_1_level() {
        // Check serialisation of a node containing some leaves.

        // To allow more readable arrays of bit manipulated values
        #![allow(clippy::identity_op)]

        let h1 = Hash::blake2b_hash_bytes(&[1, 2, 3]).unwrap();
        let h2 = Hash::blake2b_hash_bytes(&[20, 30, 1, 5, 6]).unwrap();

        let n1 = MerkleProof::Leaf(MerkleProofLeaf::Read(vec![12, 15, 30, 40]));
        let n2 = MerkleProof::Leaf(MerkleProofLeaf::Blind(h1));
        let n3 = MerkleProof::Leaf(MerkleProofLeaf::Blind(h2));
        let n4 = MerkleProof::Leaf(MerkleProofLeaf::Read(vec![123, 234, 42, 1, 2, 3]));

        let root = MerkleProof::Node(vec![n1.clone()]);
        check_serialisation(root, &[(TAG_NODE << 6) | (TAG_READ << 4), 12, 15, 30, 40]);

        let root = MerkleProof::Node(vec![n1.clone(), n2.clone()]);
        check_serialisation(
            root,
            &[
                [(TAG_NODE << 6) | (TAG_READ << 4) | (TAG_BLIND << 2)].as_ref(),
                &[12, 15, 30, 40],
                h1.as_ref(),
            ]
            .concat(),
        );

        let root = MerkleProof::Node(vec![n1.clone(), n2.clone(), n3.clone()]);
        check_serialisation(
            root,
            &[
                [(TAG_NODE << 6) | (TAG_READ << 4) | (TAG_BLIND << 2) | TAG_BLIND].as_ref(),
                &[12, 15, 30, 40],
                h1.as_ref(),
                h2.as_ref(),
            ]
            .concat(),
        );

        let root = MerkleProof::Node(vec![n1.clone(), n2.clone(), n4.clone(), n3.clone()]);
        check_serialisation(
            root,
            &[
                [
                    (TAG_NODE << 6) | (TAG_READ << 4) | (TAG_BLIND << 2) | TAG_READ,
                    TAG_BLIND << 6,
                ]
                .as_ref(),
                &[12, 15, 30, 40],
                h1.as_ref(),
                &[123, 234, 42, 1, 2, 3],
                h2.as_ref(),
            ]
            .concat(),
        )
    }

    fn check_bounds(tree: MerkleProof, bound: &SerialisationBound) {
        let final_state_hash = Hash::blake2b_hash_bytes(&rand::random::<[u8; 10]>()).unwrap();
        let proof = Proof::new(tree, final_state_hash);

        let serialisation: Vec<_> = super::serialise_proof(&proof).collect();
        assert!(serialisation.len() <= bound.expected_serialisation_length());
    }

    #[test]
    fn bounds_1_level() {
        // Check size of the serialisations for a node having a number of leaves.

        for i in 1..20 {
            let children: Vec<_> = (0..i).map(|_| generate_rand_leaf()).collect();
            let bounds = children
                .iter()
                .map(SerialisationBound::from_merkle_leaf)
                .collect();

            let root = MerkleProof::Node(children);
            let bound = SerialisationBound::node_combine(bounds);

            check_bounds(root, &bound);
        }
    }

    #[test]
    fn bounds_n_levels() {
        // Check size of serialisation of a randomly generated Merkle tree.

        // Starting from an array of leaves, combine a randomly generated number of consecutive nodes
        // into a node for the level above, This will continue until only a node is left, which is the root.

        proptest!(|(total_length in 100..300)| {
            let mut nodes: Vec<_> = (0..total_length).map(|_| {
                let leaf = generate_rand_leaf();
                let bound = SerialisationBound::from_merkle_leaf(&leaf);
                (leaf, bound)
            }).collect();

            while nodes.len() > 1 {
                let mut new_nodes = vec![];
                let mut iter = nodes.into_iter();

                let mut nothing_taken = false;
                while !nothing_taken {
                    // wanted number of children of a node is between 2 and 10.
                    // (Note, if only a node is left, then there will be only one child)
                    let nr_children = rand::random::<usize>() % 8 + 2;

                    let mut new_children = vec![];
                    let mut new_bounds = vec![];
                    for _ in 0..nr_children {
                        if let Some((child, bound)) = iter.next() {
                            new_children.push(child);
                            new_bounds.push(bound);
                        }
                    }

                    nothing_taken = new_bounds.is_empty();

                    if !nothing_taken {
                        let node = MerkleProof::Node(new_children);
                        let bound = SerialisationBound::node_combine(new_bounds);

                        new_nodes.push((node, bound));
                    }
                }

                nodes = new_nodes;
            }

            let (root, bound) = &nodes[0];
            check_bounds(root.clone(), bound);
        });
    }

    fn check_deserialisation(raw_bytes: &[u8], full_shape: Shape, expected_proof: Proof) {
        let proof = deserialise_proof(raw_bytes.iter().copied(), full_shape).unwrap();
        assert_eq!(proof, expected_proof);
    }

    fn check_bad_deserialisation(
        raw_bytes: &[u8],
        full_shape: Shape,
        expected_error: DeserialiseError,
    ) {
        let proof = deserialise_proof(raw_bytes.iter().copied(), full_shape);
        assert_eq!(proof, Err(expected_error));
    }

    #[test]
    fn deserialise_leaf_trees() {
        // Serialisation represents a leaf
        // Original shape can be a leaf or a tree with multiple children.

        let gen_hash_data = |length| {
            let mut data = vec![0; length];
            data.try_fill(&mut thread_rng()).unwrap();
            let hash = Hash::blake2b_hash_bytes(&data).unwrap();
            (data, hash)
        };

        // Blind leaf, given shape is exact / bigger
        let final_hash_bytes = rand::random::<[u8; DIGEST_SIZE]>();
        let (data, hash) = gen_hash_data(12);
        let bytes = [final_hash_bytes.as_ref(), &[TAG_BLIND << 6], hash.as_ref()].concat();
        // When a leaf / subtree is blinded, the size is disregarded
        let shape = Shape::Leaf(40);
        let bigger_shape = Shape::Node(vec![
            Shape::Leaf(30),
            Shape::Node(vec![Shape::Leaf(1), Shape::Leaf(10)]),
        ]);
        let exp_proof = Proof::new(
            MerkleProof::Leaf(MerkleProofLeaf::Blind(hash)),
            final_hash_bytes.into(),
        );

        check_deserialisation(&bytes, shape, exp_proof.clone());
        check_deserialisation(&bytes, bigger_shape, exp_proof);

        // Read leaf
        let bytes = [final_hash_bytes.as_ref(), &[TAG_READ << 6], &data].concat();
        let shape = Shape::Leaf(12);
        let bigger_shape = Shape::Node(vec![
            Shape::Leaf(5),
            Shape::Node(vec![Shape::Leaf(4), Shape::Leaf(3)]),
        ]);
        let exp_proof = Proof::new(
            MerkleProof::Leaf(MerkleProofLeaf::Read(data.clone())),
            final_hash_bytes.into(),
        );

        check_deserialisation(&bytes, shape, exp_proof.clone());
        check_deserialisation(&bytes, bigger_shape, exp_proof);
    }

    #[test]
    fn deserialise_trees_exact_shape() {
        // Serialisation represents (((B),(R-40)), ((R-32),(B),(R-16)), (B))
        //                             1     2         3    4     5      6
        // Traversal will be:    root (12) 1 2 (345) 3 4 5 6
        // Originial shapes

        // For readability (tag << 0 operations)
        #![allow(clippy::identity_op)]

        let gen_hash_data = |length| {
            let mut data = vec![0; length];
            data.try_fill(&mut thread_rng()).unwrap();
            let hash = Hash::blake2b_hash_bytes(&data).unwrap();
            (data, hash)
        };

        let (_, fh) = gen_hash_data(32);
        let [(_, h0), (d1, _), (d2, _), (_, h3), (d4, _), (_, h5)] =
            [16, 40, 32, 100, 16, 200].map(gen_hash_data);
        let tag_bytes = [
            (TAG_NODE << 6) | (TAG_NODE << 4) | (TAG_BLIND << 2) | (TAG_READ << 0),
            (TAG_NODE << 6) | (TAG_READ << 4) | (TAG_BLIND << 2) | (TAG_READ << 0),
            TAG_BLIND << 6,
        ];
        let val_bytes = [h0.as_ref(), &d1, &d2, h3.as_ref(), &d4, h5.as_ref()].concat();

        let n1 = Shape::Node(vec![Shape::Leaf(32), Shape::Leaf(40)]);
        let n2 = Shape::Node(vec![Shape::Leaf(32), Shape::Leaf(10), Shape::Leaf(16)]);
        let n3 = Shape::Leaf(100);
        let root = Shape::Node(vec![n1, n2, n3]);

        let merkle_proof = MerkleProof::Node(vec![
            MerkleProof::Node(vec![
                MerkleProof::Leaf(MerkleProofLeaf::Blind(h0)),
                MerkleProof::Leaf(MerkleProofLeaf::Read(d1)),
            ]),
            MerkleProof::Node(vec![
                MerkleProof::Leaf(MerkleProofLeaf::Read(d2)),
                MerkleProof::Leaf(MerkleProofLeaf::Blind(h3)),
                MerkleProof::Leaf(MerkleProofLeaf::Read(d4)),
            ]),
            MerkleProof::Leaf(MerkleProofLeaf::Blind(h5)),
        ]);

        let raw_bytes = [fh.as_ref(), &tag_bytes, &val_bytes].concat();
        check_deserialisation(&raw_bytes, root.clone(), Proof::new(merkle_proof, fh));

        // check not enough bytes errors are generated
        // values cutoff
        check_bad_deserialisation(
            &raw_bytes[..100],
            root.clone(),
            DeserialiseError::NotEnoughBytes,
        );
        // tags cutoff
        check_bad_deserialisation(
            &raw_bytes[..33],
            root.clone(),
            DeserialiseError::NotEnoughBytes,
        );
        // hash cutoff
        check_bad_deserialisation(
            &raw_bytes[..20],
            root.clone(),
            DeserialiseError::NotEnoughBytes,
        );
    }

    #[test]
    fn deserialise_trees_bigger_shape() {
        // Serialisation represents ( ( ((R-20),(B)),(B),(R-16) ), ( (R-30),(B) ) )
        //                                  1    2    3    4           5    6
        // Traversal will be:    root (1234) (12) 1 2 3 4 (56) 5 6
        // Originial shapes

        // For readability (tag << 0 operations)
        #![allow(clippy::identity_op)]

        let gen_hash_data = |length| {
            let mut data = vec![0; length];
            data.try_fill(&mut thread_rng()).unwrap();
            let hash = Hash::blake2b_hash_bytes(&data).unwrap();
            (data, hash)
        };

        let (_, fh) = gen_hash_data(32);
        let [(d0, _), (_, h1), (_, h2), (d3, _), (d4, _), (_, h5)] =
            [20, 100, 100, 16, 30, 200].map(gen_hash_data);
        let tag_bytes = [
            (TAG_NODE << 6) | (TAG_NODE << 4) | (TAG_NODE << 2) | (TAG_READ << 0),
            (TAG_BLIND << 6) | (TAG_BLIND << 4) | (TAG_READ << 2) | (TAG_NODE << 0),
            (TAG_READ << 6) | (TAG_BLIND << 4),
        ];
        let val_bytes = [&d0, h1.as_ref(), h2.as_ref(), &d3, &d4, h5.as_ref()].concat();

        let blind_subtree_1 = Shape::Node(vec![
            Shape::Node(vec![Shape::Leaf(200), Shape::Leaf(300)]),
            Shape::Leaf(400),
            Shape::Node(vec![Shape::Leaf(50), Shape::Leaf(50), Shape::Leaf(50)]),
        ]);
        let blind_subtree_2 = Shape::Node(vec![
            Shape::Node(vec![Shape::Leaf(200), Shape::Leaf(300)]),
            Shape::Leaf(400),
            Shape::Node(vec![
                Shape::Leaf(50),
                Shape::Node(vec![Shape::Leaf(50), Shape::Leaf(400)]),
                Shape::Leaf(50),
            ]),
        ]);
        let subtree_size_16 = Shape::Node(vec![
            Shape::Leaf(10),
            Shape::Node(vec![Shape::Leaf(2), Shape::Leaf(4)]),
        ]);

        let n1 = Shape::Node(vec![Shape::Leaf(20), blind_subtree_1]);
        let n11 = Shape::Node(vec![n1, blind_subtree_2, subtree_size_16]);
        let n12 = Shape::Node(vec![Shape::Leaf(30), Shape::Leaf(100)]);
        let root = Shape::Node(vec![n11, n12]);

        let merkle_proof = MerkleProof::Node(vec![
            MerkleProof::Node(vec![
                MerkleProof::Node(vec![
                    MerkleProof::Leaf(MerkleProofLeaf::Read(d0.clone())),
                    MerkleProof::Leaf(MerkleProofLeaf::Blind(h1)),
                ]),
                MerkleProof::Leaf(MerkleProofLeaf::Blind(h2)),
                MerkleProof::Leaf(MerkleProofLeaf::Read(d3.clone())),
            ]),
            MerkleProof::Node(vec![
                MerkleProof::Leaf(MerkleProofLeaf::Read(d4.clone())),
                MerkleProof::Leaf(MerkleProofLeaf::Blind(h5)),
            ]),
        ]);

        let raw_bytes = [fh.as_ref(), &tag_bytes, &val_bytes].concat();
        check_deserialisation(&raw_bytes, root.clone(), Proof::new(merkle_proof, fh));

        // ExpectedLeaf tag error
        let tag_bytes_bad = [
            (TAG_NODE << 6) | (TAG_NODE << 4) | (TAG_NODE << 2) | (TAG_READ << 0),
            (TAG_BLIND << 6) | (TAG_NODE << 4) | (TAG_READ << 2) | (TAG_NODE << 0),
            (TAG_READ << 6) | (TAG_BLIND << 4),
        ];
        let raw_bytes = [fh.as_ref(), &tag_bytes_bad, &val_bytes].concat();
        check_bad_deserialisation(&raw_bytes, root.clone(), DeserialiseError::ExpectedLeaf);
    }

    #[test]
    fn deserialise_too_many_bytes() {
        let gen_hash_data = || {
            let data = rand::random::<[u8; 12]>().to_vec();
            let hash = Hash::blake2b_hash_bytes(&data).unwrap();
            (data, hash)
        };

        let (data, _hash) = gen_hash_data();
        let final_hash_bytes = rand::random::<[u8; DIGEST_SIZE]>();

        let shape = Shape::Leaf(12);

        let extra_bytes = [120, 200, 50, 10];
        let bytes = [
            final_hash_bytes.as_ref(),
            &[TAG_READ << 6],
            &data,
            &extra_bytes,
        ]
        .concat();

        check_bad_deserialisation(&bytes, shape, DeserialiseError::TooManyBytes);
    }

    #[test]
    fn deserialise_invalid_tag() {
        let gen_hash_data = || {
            let data = rand::random::<[u8; 12]>().to_vec();
            let hash = Hash::blake2b_hash_bytes(&data).unwrap();
            (data, hash)
        };

        let (_data, hash) = gen_hash_data();
        let final_hash_bytes = rand::random::<[u8; DIGEST_SIZE]>();

        let shape = Shape::Leaf(12);

        let bytes = [final_hash_bytes.as_ref(), &[0b01 << 6], hash.as_ref()].concat();

        check_bad_deserialisation(&bytes, shape, DeserialiseError::InvalidTag);
    }
}

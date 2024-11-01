// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

// TODO: RV-271 Allow unused code until functionality is exposed to the OCaml bindings.
#![allow(dead_code)]

use super::{merkle::MerkleTree, tree::Tree};
use crate::state_backend::hash::Hash;
use arbitrary_int::u2;
use itertools::Itertools;

/// Structure of a proof transitioning from state A to state B.
///
/// The proof needs to be able to:
/// - Contain enough information to be able to run a single step on A
/// - Obtain the hash of the state after the step
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
}

/// Tag of a node
const TAG_NODE: u8 = 0b00;
/// Tag of a blind leaf
const TAG_BLIND: u8 = 0b10;
/// Tag of a read leaf
const TAG_READ: u8 = 0b11;

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
pub type MerkleProof = Tree<MerkleProofLeaf>;

/// Type used to describe the leaves of a [`MerkleProof`].
/// For more details see the documentation of [`MerkleProof`].
#[derive(Clone)]
pub enum MerkleProofLeaf {
    /// A leaf that is not read. It may be written.
    /// Contains the hash of the contents from initial state.
    ///
    /// Note: a blinded leaf can correspond to a blinded subtree in a [`MerkleTree`] (due to compression)
    Blind(Hash),
    /// A leaf that is read. It may also be written.
    /// Contains the read data from the initial state.
    /// The initial hash can be deduced based on the read data.
    Read(Vec<u8>),
}

impl From<MerkleTree> for MerkleProof {
    /// Transform a [`MerkleTree`] to a [`MerkleProof`]
    /// TODO: RV-240
    fn from(_tree: MerkleTree) -> Self {
        todo!("Not implemented")
    }
}

impl MerkleProof {
    /// Return a 2-bit tag for the variant of the node in the proof.
    pub fn to_raw_tag(&self) -> u2 {
        match self {
            MerkleProof::Node(_) => u2::new(TAG_NODE),
            MerkleProof::Leaf(MerkleProofLeaf::Blind(_)) => u2::new(TAG_BLIND),
            MerkleProof::Leaf(MerkleProofLeaf::Read(_)) => u2::new(TAG_READ),
        }
    }
}

fn serialise_raw_tags(raw_tags: impl Iterator<Item = u2>) -> Vec<u8> {
    // Tag serialisation to bytes depends on the number of bits required to hold a raw tag
    // Here, a raw tag is 2 bits wide, hence we use 8 / 2 = 4 chunks
    raw_tags
        .chunks(4)
        .into_iter()
        .map(|chunk| {
            chunk.zip([6, 4, 2, 0]).fold(0, |acc: u8, (tag, offset)| {
                let bits = tag.value() << offset;
                acc | bits
            })
        })
        .collect()
}

fn iter_raw_tags(proof: &MerkleProof) -> impl Iterator<Item = u2> + '_ {
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

/// Serialise a merkle proof to an array of bytes.
///
/// In the encoding, lengths are not necessary, but tags are,
/// since the tags depend on runtime information and events
pub fn serialise_proof(proof: &Proof) -> impl Iterator<Item = u8> + '_ {
    // Here we collect the `iter_raw_tags` iterator to be able to chunkify it and transform it
    // by compressing the `u2` tags to a byte-array, fully utilising the bytes capacity.
    let final_hash_encoding = proof.final_state_hash.as_ref().iter().copied();
    let tags_encoding = serialise_raw_tags(iter_raw_tags(&proof.partial_tree)).into_iter();
    let nodes_encoding = serialise_proof_values(&proof.partial_tree);

    final_hash_encoding
        .chain(tags_encoding)
        .chain(nodes_encoding)
}

#[cfg(test)]
mod tests {
    use super::{serialise_proof, MerkleProof, MerkleProofLeaf, TAG_BLIND, TAG_NODE, TAG_READ};
    use crate::{
        state_backend::proof_backend::proof::Proof,
        storage::{Hash, DIGEST_SIZE},
    };
    use proptest::proptest;
    use rand::Fill;

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
                MerkleProof::Node(_) => panic!("Expected a merkle proof leaf"),
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
        check_serialisation(root, &[TAG_NODE << 6 | TAG_READ << 4, 12, 15, 30, 40]);

        let root = MerkleProof::Node(vec![n1.clone(), n2.clone()]);
        check_serialisation(
            root,
            &[
                [TAG_NODE << 6 | TAG_READ << 4 | TAG_BLIND << 2].as_ref(),
                &[12, 15, 30, 40],
                h1.as_ref(),
            ]
            .concat(),
        );

        let root = MerkleProof::Node(vec![n1.clone(), n2.clone(), n3.clone()]);
        check_serialisation(
            root,
            &[
                [TAG_NODE << 6 | TAG_READ << 4 | TAG_BLIND << 2 | TAG_BLIND].as_ref(),
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
                    TAG_NODE << 6 | TAG_READ << 4 | TAG_BLIND << 2 | TAG_READ,
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
        // Check size of serialisation of a randomly generated merkle tree.

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
}

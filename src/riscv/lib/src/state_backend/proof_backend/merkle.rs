// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Merkle trees used for proof generation by the PVM

#![allow(dead_code)]

use crate::state_backend::hash::{Hash, HashError, DIGEST_SIZE};

/// A variable-width Merkle tree with optional `AccessInfo` metadata for leaves.
/// Values of this type are produced by the proof-generating backend to capture
/// a snapshot of the machine state along with access information for leaves
/// which hold data that was used in a particular evaluation step.
#[derive(Debug)]
pub enum MerkleTree {
    Leaf(Hash, AccessInfo, Vec<u8>),
    Node(Hash, Vec<Self>),
}

/// Type of access associated with leaves in a `MerkleTree`.
#[derive(Debug)]
pub enum AccessInfo {
    NoAccess,
    Read,
    Write,
    ReadWrite,
}

impl MerkleTree {
    /// Get the root hash of a Merkle tree
    fn root_hash(&self) -> Hash {
        match self {
            Self::Node(hash, _) => *hash,
            Self::Leaf(hash, _, _) => *hash,
        }
    }

    /// Check the validity of the Merkle root by recomputing all hashes
    fn check_root_hash(&self) -> bool {
        let mut deque = std::collections::VecDeque::new();
        deque.push_back(self);

        while let Some(node) = deque.pop_front() {
            let is_valid_hash = match node {
                Self::Leaf(hash, _, data) => {
                    Hash::blake2b_hash_bytes(data).is_ok_and(|h| h == *hash)
                }
                Self::Node(hash, children) => {
                    // TODO RV-250: Instead of building the whole input and
                    // hashing it, we should use incremental hashing, which
                    // isn't currently supported in `tezos_crypto_rs`.
                    //
                    // Check the hash of this node and push children to deque to be checked
                    let mut hashes: Vec<u8> = Vec::with_capacity(DIGEST_SIZE * children.len());
                    children.iter().for_each(|child| {
                        deque.push_back(child);
                        hashes.extend_from_slice(child.root_hash().as_ref())
                    });
                    Hash::blake2b_hash_bytes(&hashes).is_ok_and(|h| h == *hash)
                }
            };
            if !is_valid_hash {
                return false;
            }
        }
        true
    }
}

// TODO: RV-237 Ensure consistency between `RootHashable` and `Merkleisable`
// implementations
pub trait Merkleisable {
    /// Build the Merkle tree described by the layouat of the data.
    fn to_merkle_tree(&self) -> Result<MerkleTree, HashError>;
}

#[cfg(test)]
mod tests {
    use super::{AccessInfo, MerkleTree, Merkleisable};
    use crate::state_backend::hash::{Hash, HashError, RootHashable};
    use proptest::prelude::*;

    impl Merkleisable for &Vec<u8> {
        fn to_merkle_tree(&self) -> Result<MerkleTree, HashError> {
            let hash = Hash::blake2b_hash_bytes(self)?;
            Ok(MerkleTree::Leaf(
                hash,
                AccessInfo::NoAccess,
                (*self).clone(),
            ))
        }
    }

    impl<A: Merkleisable, B: Merkleisable> Merkleisable for (A, B) {
        fn to_merkle_tree(&self) -> Result<MerkleTree, HashError> {
            let left = self.0.to_merkle_tree()?;
            let right = self.1.to_merkle_tree()?;
            let hash = RootHashable::hash(&(left.root_hash(), right.root_hash()))?;
            Ok(MerkleTree::Node(hash, vec![left, right]))
        }
    }

    #[test]
    fn test_merkle_tree() {
        proptest!(|
        (leaves in prop::collection::vec(
            prop::collection::vec(0u8..255, 0..100),
            6)
        )| {
            let tree = ((&leaves[0], (&leaves[1], &leaves[2])), ((&leaves[3], &leaves[4]), &leaves[5]))
                .to_merkle_tree()
                .expect("Error building Merkle tree");
            assert!(tree.check_root_hash())
        });
    }
}

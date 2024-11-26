// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Merkle trees used for proof generation by the PVM

// TODO: RV-271 Allow unused code until functionality is exposed to the OCaml bindings.
#![allow(dead_code)]

use crate::state_backend::hash::{Hash, HashError, RootHashable, DIGEST_SIZE};
use itertools::Itertools;

/// A variable-width Merkle tree with [`AccessInfo`] metadata for leaves.
///
/// Values of this type are produced by the proof-generating backend to capture
/// a snapshot of the machine state along with access information for leaves
/// which hold data that was used in a particular evaluation step.
#[derive(Debug, Clone)]
pub enum MerkleTree {
    Leaf(Hash, AccessInfo, Vec<u8>),
    Node(Hash, Vec<Self>),
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

    /// Compress all fully blindable subtrees to a single leaf node, obtaining a [`CompressedMerkleTree`].
    ///
    /// To fit a proof in a manager operation, the Merkle tree it contains needs to be compressed.
    /// This is obtained by transforming all fully blindable trees
    /// (subtrees of only [`AccessInfo::NoAccess`] or only [`AccessInfo::Write`])
    /// into a leaf of the corresponding [`AccessInfo`] variant.
    fn compress(self) -> Result<CompressedMerkleTree, HashError> {
        // TODO: RV-290 Change to non-recursive implentation to avoid stackoverflow.
        use CompressedMerkleTree::Leaf as CompresedLeaf;
        use CompressedMerkleTree::Node as CompresedNode;

        match self {
            Self::Leaf(hash, access, data) => Ok(CompresedLeaf(hash, access.to_compressed(data))),
            Self::Node(hash, children) => {
                // compress each children individually
                let compact_children: Vec<_> =
                    children.into_iter().map(Self::compress).try_collect()?;

                // obtain array of hashes and corresponding array of
                // the compression performed for each child node.
                // A subtree can either be compressed to a `Read` / `ReadWrite` leaf or not be fully compressed.
                let (hashes, compressions) = compact_children
                    .iter()
                    .map(|child| {
                        use CompressedAccessInfo::*;
                        match child {
                            CompresedLeaf(hash, access_info) => (
                                hash,
                                match access_info {
                                    NoAccess | Write => Some(access_info.clone()),
                                    Read(_) | ReadWrite(_) => None,
                                },
                            ),
                            CompresedNode(hash, _) => (hash, None),
                        }
                    })
                    .unzip::<_, _, Vec<&Hash>, Vec<_>>();

                // Obtain the compression type of all children
                // if all children have been compressed successfully to the same type of leaf.
                let compression = compressions
                    .into_iter()
                    .reduce(|a, b| (a == b).then_some(a?))
                    .flatten();

                let compressed_tree = match compression {
                    Some(access) => CompresedLeaf(RootHashable::hash(&hashes)?, access),
                    None => CompresedNode(hash, compact_children),
                };

                Ok(compressed_tree)
            }
        }
    }
}

impl RootHashable for MerkleTree {
    fn hash(&self) -> Result<Hash, HashError> {
        Ok(self.root_hash())
    }
}

/// Type of access associated with leaves in a [`MerkleTree`].
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AccessInfo {
    NoAccess,
    Write,
    Read,
    ReadWrite,
}

impl AccessInfo {
    /// Convert an access info to a compressed access info, potentially with additional data.
    pub(super) fn to_compressed(self, data: Vec<u8>) -> CompressedAccessInfo {
        match self {
            Self::NoAccess => CompressedAccessInfo::NoAccess,
            Self::Write => CompressedAccessInfo::Write,
            Self::Read => CompressedAccessInfo::Read(data),
            Self::ReadWrite => CompressedAccessInfo::ReadWrite(data),
        }
    }
}

/// Intermediary representation obtained when compressing a [`MerkleTree`].
///
/// For the compressed tree, we only care about the data in the non-blinded leaves.
#[derive(Debug, Clone, PartialEq)]
pub(super) enum CompressedMerkleTree {
    Leaf(Hash, CompressedAccessInfo),
    Node(Hash, Vec<Self>),
}

impl CompressedMerkleTree {
    /// Get the root hash of a compreessed Merkle tree
    fn root_hash(&self) -> Hash {
        match self {
            Self::Node(hash, _) => *hash,
            Self::Leaf(hash, _) => *hash,
        }
    }

    /// Check the validity of the Merkle root by recomputing all hashes
    fn check_root_hash(&self) -> bool {
        let mut deque = std::collections::VecDeque::new();
        deque.push_back(self);

        while let Some(node) = deque.pop_front() {
            let is_valid_hash = match node {
                Self::Leaf(hash, access_info) => match access_info {
                    CompressedAccessInfo::NoAccess | CompressedAccessInfo::Write => true,
                    CompressedAccessInfo::Read(data) | CompressedAccessInfo::ReadWrite(data) => {
                        Hash::blake2b_hash_bytes(data).is_ok_and(|h| h == *hash)
                    }
                },
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

/// Type of access associated with leaves in a [`CompressedMerkleTree`].
///
/// If a subtree is made up of only [`AccessInfo::NoAccess`] or of only [`AccessInfo::Read`]
/// then the subtree can be compressed to a leaf of the corresponding access type.
/// For the non-blinded variants, it holds the byte data associated with the access.
#[derive(Debug, Clone, PartialEq)]
pub(super) enum CompressedAccessInfo {
    NoAccess,
    Write,
    Read(Vec<u8>),
    ReadWrite(Vec<u8>),
}

// TODO: RV-237 Ensure consistency between `RootHashable` and `Merkleisable`
// implementations
pub trait Merkleisable {
    /// Build the Merkle tree described by the layouat of the data.
    fn to_merkle_tree(&self) -> Result<MerkleTree, HashError>;
}

impl<T: Merkleisable> Merkleisable for [T] {
    fn to_merkle_tree(&self) -> Result<MerkleTree, HashError> {
        let children = self
            .iter()
            .map(|e| e.to_merkle_tree())
            .collect::<Result<Vec<_>, _>>()?;

        Ok(MerkleTree::Node(children.hash()?, children))
    }
}

impl<T: Merkleisable> Merkleisable for Vec<T> {
    fn to_merkle_tree(&self) -> Result<MerkleTree, HashError> {
        self.as_slice().to_merkle_tree()
    }
}

impl<T: Merkleisable, const N: usize> Merkleisable for [T; N] {
    fn to_merkle_tree(&self) -> Result<MerkleTree, HashError> {
        self.as_ref().to_merkle_tree()
    }
}

macro_rules! impl_merkleisable_for_tuple {
    ($($name:ident),*) => {
        impl<$($name: Merkleisable),*> Merkleisable for ($($name,)*) {
            fn to_merkle_tree(&self) -> Result<MerkleTree, HashError> {
                #[allow(non_snake_case)]
                let ($($name,)*) = self;
                let children: Result<Vec<MerkleTree>, HashError> = [$($name.to_merkle_tree(),)*].into_iter().collect();
                let children = children?;
                Ok(MerkleTree::Node(children.hash()?, children))

            }
        }
    }
}

impl_merkleisable_for_tuple!(A, B);
impl_merkleisable_for_tuple!(A, B, C);
impl_merkleisable_for_tuple!(A, B, C, D);
impl_merkleisable_for_tuple!(A, B, C, D, E);
impl_merkleisable_for_tuple!(A, B, C, D, E, F);

#[cfg(test)]
mod tests {
    use super::{AccessInfo, MerkleTree, Merkleisable};
    use crate::state_backend::{
        hash::{Hash, HashError, RootHashable},
        proof_backend::proof::{MerkleProof, MerkleProofLeaf},
    };
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

    impl RootHashable for Vec<u8> {
        fn hash(&self) -> Result<Hash, HashError> {
            Hash::blake2b_hash_bytes(self)
        }
    }

    fn m_l(data: &[u8], access: AccessInfo) -> Result<MerkleTree, HashError> {
        let hash = Hash::blake2b_hash_bytes(data)?;
        Ok(MerkleTree::Leaf(hash, access, data.to_vec()))
    }

    fn m_t(left: MerkleTree, right: MerkleTree) -> Result<MerkleTree, HashError> {
        let hash = RootHashable::hash(&(left.root_hash(), right.root_hash()))?;
        Ok(MerkleTree::Node(hash, vec![left, right]))
    }

    #[test]
    fn test_merkle_tree() {
        proptest!(|
        (leaves in prop::collection::vec(
            prop::collection::vec(0u8..255, 0..100),
            6)
        )| {
            let state = ((&leaves[0], (&leaves[1], &leaves[2])), ((&leaves[3], &leaves[4]), &leaves[5]));
            let tree = state
                .to_merkle_tree()
                .expect("Error building Merkle tree");
            assert!(tree.check_root_hash());
            assert!(tree.root_hash() == state.hash()?);
        });
    }

    #[test]
    fn test_compression() {
        let test = |l: Vec<Vec<u8>>| -> Result<_, HashError> {
            use AccessInfo::*;

            let single_leaves = m_t(
                m_t(m_l(&l[0], NoAccess)?, m_l(&l[1], Write)?)?,
                m_t(m_l(&l[2], Read)?, m_l(&l[3], ReadWrite)?)?,
            )?;
            let no_access_t = m_t(
                m_l(&l[4], NoAccess)?,
                m_t(m_l(&l[5], NoAccess)?, m_l(&l[6], NoAccess)?)?,
            )?;
            let write_t = m_t(
                m_t(m_l(&l[7], Write)?, m_l(&l[8], Write)?)?,
                m_t(m_l(&l[9], Write)?, m_l(&l[10], Write)?)?,
            )?;
            let read_t = m_t(
                m_t(m_l(&l[11], Read)?, m_l(&l[12], Read)?)?,
                m_t(m_l(&l[13], Read)?, m_l(&l[14], Read)?)?,
            )?;
            let read_write_t = m_t(
                m_t(m_l(&l[15], ReadWrite)?, m_l(&l[16], ReadWrite)?)?,
                m_l(&l[17], ReadWrite)?,
            )?;
            // Note, even though no_access is blindable tree, and same for write,
            // they should not combine into a single blinded tree here.
            // This is due to not being of the same type, but they should each
            // just be a leaf instead of the tree structure above.
            let combine_isolated = m_t(m_t(no_access_t, write_t)?, m_t(read_t, read_write_t)?)?;

            let mix = m_t(
                m_t(
                    m_l(&l[18], NoAccess)?,
                    m_t(
                        m_l(&l[19], NoAccess)?,
                        m_t(m_l(&l[20], Write)?, m_l(&l[21], Write)?)?, // the Write leaves will be compressed
                    )?,
                )?,
                m_t(
                    m_t(
                        m_l(&l[22], ReadWrite)?,
                        m_t(
                            m_l(&l[23], NoAccess)?,
                            m_l(&l[24], NoAccess)?, // the NoAccess leaves will get compressed
                        )?,
                    )?,
                    m_l(&l[25], Read)?,
                )?,
            )?;

            let merkle_tree = m_t(single_leaves, m_t(combine_isolated, mix)?)?;

            let merkle_proof_leaf =
                |data: &Vec<u8>, access: AccessInfo| -> Result<MerkleProof, HashError> {
                    let hash = Hash::blake2b_hash_bytes(data)?;
                    Ok(MerkleProof::Leaf(match access {
                        NoAccess | Write => MerkleProofLeaf::Blind(hash),
                        Read | ReadWrite => MerkleProofLeaf::Read(data.clone()),
                    }))
                };

            let proof_single_leaves = MerkleProof::Node(vec![
                MerkleProof::Node(vec![
                    merkle_proof_leaf(&l[0], NoAccess)?,
                    merkle_proof_leaf(&l[1], Write)?,
                ]),
                MerkleProof::Node(vec![
                    merkle_proof_leaf(&l[2], Read)?,
                    merkle_proof_leaf(&l[3], ReadWrite)?,
                ]),
            ]);

            // Not even though structurally the code has the same arborescent shape,
            // the proof shape is changed, some nodes becoming leaves now. (subtrees being compressed)
            let proof_no_access = MerkleProof::Leaf(MerkleProofLeaf::Blind(
                [
                    Hash::blake2b_hash_bytes(&l[4])?,
                    [
                        Hash::blake2b_hash_bytes(&l[5])?,
                        Hash::blake2b_hash_bytes(&l[6])?,
                    ]
                    .hash()?,
                ]
                .hash()?,
            ));

            let proof_write = MerkleProof::Leaf(MerkleProofLeaf::Blind(
                [
                    [
                        Hash::blake2b_hash_bytes(&l[7])?,
                        Hash::blake2b_hash_bytes(&l[8])?,
                    ]
                    .hash()?,
                    [
                        Hash::blake2b_hash_bytes(&l[9])?,
                        Hash::blake2b_hash_bytes(&l[10])?,
                    ]
                    .hash()?,
                ]
                .hash()?,
            ));

            let proof_read = MerkleProof::Node(vec![
                MerkleProof::Node(vec![
                    merkle_proof_leaf(&l[11], Read)?,
                    merkle_proof_leaf(&l[12], Read)?,
                ]),
                MerkleProof::Node(vec![
                    merkle_proof_leaf(&l[13], Read)?,
                    merkle_proof_leaf(&l[14], Read)?,
                ]),
            ]);

            let proof_read_write = MerkleProof::Node(vec![
                MerkleProof::Node(vec![
                    merkle_proof_leaf(&l[15], ReadWrite)?,
                    merkle_proof_leaf(&l[16], ReadWrite)?,
                ]),
                merkle_proof_leaf(&l[17], ReadWrite)?,
            ]);

            let proof_combine_isolated = MerkleProof::Node(vec![
                MerkleProof::Node(vec![proof_no_access, proof_write]),
                MerkleProof::Node(vec![proof_read, proof_read_write]),
            ]);

            let proof_mix = MerkleProof::Node(vec![
                MerkleProof::Node(vec![
                    merkle_proof_leaf(&l[18], NoAccess)?,
                    MerkleProof::Node(vec![
                        merkle_proof_leaf(&l[19], NoAccess)?,
                        MerkleProof::Leaf(MerkleProofLeaf::Blind(
                            [
                                Hash::blake2b_hash_bytes(&l[20])?,
                                Hash::blake2b_hash_bytes(&l[21])?,
                            ]
                            .hash()?,
                        )),
                    ]),
                ]),
                MerkleProof::Node(vec![
                    MerkleProof::Node(vec![
                        merkle_proof_leaf(&l[22], ReadWrite)?,
                        MerkleProof::Leaf(MerkleProofLeaf::Blind(
                            [
                                Hash::blake2b_hash_bytes(&l[23])?,
                                Hash::blake2b_hash_bytes(&l[24])?,
                            ]
                            .hash()?,
                        )),
                    ]),
                    merkle_proof_leaf(&l[25], Read)?,
                ]),
            ]);

            let proof = MerkleProof::Node(vec![
                proof_single_leaves,
                MerkleProof::Node(vec![proof_combine_isolated, proof_mix]),
            ]);

            let merkle_tree_root_hash = merkle_tree.root_hash();

            let compressed_merkle_tree = merkle_tree.compress()?;
            assert!(compressed_merkle_tree.check_root_hash());
            assert_eq!(compressed_merkle_tree.root_hash(), merkle_tree_root_hash);

            let compressed_merkle_proof: MerkleProof = compressed_merkle_tree.into();

            assert_eq!(compressed_merkle_proof, proof);

            Ok(())
        };

        // this whole proptest macro delegates to a pure rust function in order to have easy access to formatting
        proptest!(|(l in prop::collection::vec(
            prop::collection::vec(0u8..255, 0..100),
            26
        ))| {
            test(l).expect("Unexpected Hashing error");
        });
    }
}

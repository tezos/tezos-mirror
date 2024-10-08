// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

// TODO: RV-271 Allow unused code until functionality is exposed to the OCaml bindings.
#![allow(dead_code)]

use super::merkle::MerkleTree;
use crate::state_backend::hash::Hash;

/// Assume the proof is for transitioning from state A to state B.
/// The proof needs to be able to:
/// - Contain enough information to be able to run a single step on A
/// - Obtain the hash of the state after the step
pub struct Proof {
    /// State of the final state B
    final_state_hash: Hash,
    /// Merkle tree structure of the proof
    proof: MerkleProof,
}

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
pub enum MerkleProof {
    /// A node which holds an amount of child nodes.
    /// Hash not needed since it can be reconstructed from the children.
    Node(Vec<Self>),
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

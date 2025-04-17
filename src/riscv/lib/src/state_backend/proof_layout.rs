// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::collections::VecDeque;

use super::Array;
use super::Atom;
use super::DynArray;
use super::Layout;
use super::Many;
use super::Ref;
use super::RefProofGenOwnedAlloc;
use super::RefVerifierAlloc;
use super::hash::Hash;
use super::hash::HashError;
use super::owned_backend::Owned;
use super::proof_backend::merkle::MERKLE_ARITY;
use super::proof_backend::merkle::MERKLE_LEAF_SIZE;
use super::proof_backend::merkle::MerkleTree;
use super::proof_backend::merkle::MerkleWriter;
use super::proof_backend::merkle::build_custom_merkle_tree;
use super::proof_backend::merkle::chunks_to_writer;
use super::proof_backend::proof::MerkleProof;
use super::proof_backend::proof::MerkleProofLeaf;
use super::proof_backend::tree::Tree;
use super::verify_backend::PartialState;
use super::verify_backend::{self};
use crate::array_utils::boxed_array;
use crate::state_backend::verify_backend::PageId;
use crate::storage::binary;

/// Errors that may occur when parsing a Merkle proof
#[derive(Debug, thiserror::Error)]
pub enum FromProofError {
    #[error("Error during hashing: {0}")]
    Hash(#[from] HashError),

    #[error("Error during deserialisation: {0}")]
    Deserialise(#[from] bincode::Error),

    #[error("Encountered an invalid hash in a blinded node or leaf")]
    InvalidHash,

    #[error("Encountered a node with a bad number of branches: expected {expected}, got {got}")]
    BadNumberOfBranches { expected: usize, got: usize },

    #[error("Expected a leaf of size {expected}, got {got}")]
    UnexpectedLeafSize { expected: usize, got: usize },

    #[error("Encountered a leaf where a node was expected")]
    UnexpectedLeaf,

    #[error("Encountered a node where a leaf was expected")]
    UnexpectedNode,
}

type Result<T, E = FromProofError> = std::result::Result<T, E>;

/// Errors that may occur when hashing a [`verify_backend::Verifier`] state
#[derive(Debug, thiserror::Error)]
pub enum PartialHashError {
    #[error("Error during hashing: {0}")]
    Hash(#[from] HashError),

    #[error("Error from proof: {0}")]
    FromProof(#[from] FromProofError),

    /// Indicates that a hash could not be computed due to absent data,
    /// but from which it is possible to recover if the level at which
    /// it was raised is part of a blinded subtree and its hash is present
    /// in the proof.
    #[error("Potentially recoverable error")]
    PotentiallyRecoverable,

    /// Indicates that a hash could not be computed because the data being
    /// hashed is only partially available.
    #[error("Fatal error")]
    Fatal,
}

/// Common result type for parsing a Merkle proof
pub type FromProofResult<L> = Result<<L as Layout>::Allocated<verify_backend::Verifier>>;

/// Part of a tree that may be absent
pub enum ProofPart<'a, T: ?Sized> {
    /// This part of the tree is absent.
    Absent,

    /// There is a proof for this part of the tree.
    Present(&'a T),
}

impl<T> Clone for ProofPart<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for ProofPart<'_, T> {}

/// Part of a Merkle proof tree
pub type ProofTree<'a> = ProofPart<'a, MerkleProof>;

impl<'a> ProofTree<'a> {
    /// Interpret this part of the Merkle proof as a node with `LEN` branches.
    pub fn into_branches<const LEN: usize>(self) -> Result<Box<[Self; LEN]>> {
        let ProofTree::Present(proof) = self else {
            // The requested branches are not represented in the Merkle proof at all, not even
            // through a blinded node.
            return Ok(boxed_array![ProofTree::Absent; LEN]);
        };

        match proof {
            Tree::Node(branches) => {
                let branches: &[MerkleProof; LEN] =
                    branches.as_slice().try_into().map_err(|_| {
                        FromProofError::BadNumberOfBranches {
                            got: branches.len(),
                            expected: LEN,
                        }
                    })?;
                Ok(branches
                    .iter()
                    .map(ProofTree::Present)
                    .collect::<Vec<_>>()
                    .try_into()
                    .map_err(|_| {
                        unreachable!(
                            "Converting a vector to an array of the same size always succeeds"
                        )
                    })
                    .unwrap())
            }

            Tree::Leaf(leaf) => match leaf {
                MerkleProofLeaf::Blind(_hash) => Ok(boxed_array![ProofTree::Absent; LEN]),
                _ => Err(FromProofError::UnexpectedLeaf)?,
            },
        }
    }

    /// Interpret this part of the Merkle proof as a leaf.
    pub fn into_leaf(self) -> Result<ProofPart<'a, [u8]>> {
        if let ProofTree::Present(proof) = self {
            match proof {
                Tree::Node(_) => Err(FromProofError::UnexpectedNode),
                Tree::Leaf(leaf) => match leaf {
                    MerkleProofLeaf::Blind(_) => Ok(ProofPart::Absent),
                    MerkleProofLeaf::Read(data) => Ok(ProofPart::Present(data.as_slice())),
                },
            }
        } else {
            Ok(ProofPart::Absent)
        }
    }

    /// For the purpose of computing the final hash of a `Verifier` state,
    /// interpret this part of a Merkle proof as a leaf and return its hash if
    /// it is a blinded leaf or hash the data if it is present.
    pub(crate) fn partial_hash_leaf(self) -> Result<Hash, PartialHashError> {
        let ProofTree::Present(proof) = self else {
            return Err(PartialHashError::PotentiallyRecoverable);
        };

        let Tree::Leaf(leaf) = proof else {
            return Err(FromProofError::UnexpectedNode.into());
        };

        let hash = match leaf {
            MerkleProofLeaf::Blind(hash) => *hash,
            MerkleProofLeaf::Read(data) => Hash::blake2b_hash_bytes(data)?,
        };

        Ok(hash)
    }

    /// For the purpose of computing the final hash of a `Verifier` state,
    /// if present, try to interpret this part of a Merkle proof as:
    /// - a node with `LEN` branches, in which case return the proof branches
    ///   and no proof hash
    /// - a blinded leaf which corresponds to a node with `LEN` children,
    ///   in which case return absent branches and the proof hash
    ///
    /// If the proof tree is absent, return absent branches and no proof hash.
    pub fn into_branches_with_hash<const LEN: usize>(
        self,
    ) -> Result<(Box<[ProofTree<'a>; LEN]>, Option<Hash>), PartialHashError> {
        let ProofTree::Present(proof) = self else {
            return Ok((boxed_array![ProofTree::Absent; LEN], None));
        };

        match proof {
            Tree::Node(branches) if branches.len() != LEN => Err(PartialHashError::FromProof(
                FromProofError::BadNumberOfBranches {
                    got: branches.len(),
                    expected: LEN,
                },
            )),
            Tree::Node(branches) => Ok((
                branches
                    .iter()
                    .map(ProofTree::Present)
                    .collect::<Vec<_>>()
                    .into_boxed_slice()
                    .try_into()
                    .map_err(|_| PartialHashError::Fatal)?,
                None,
            )),
            Tree::Leaf(leaf) => match leaf {
                MerkleProofLeaf::Blind(hash) => {
                    Ok((boxed_array![ProofTree::Absent; LEN], Some(*hash)))
                }
                _ => Err(FromProofError::UnexpectedLeaf)?,
            },
        }
    }
}

/// [`Layouts`] which may be used in a Merkle proof
///
/// [`Layouts`]: crate::state_backend::Layout
pub trait ProofLayout: Layout {
    /// Obtain the complete Merkle tree which captures an execution trace
    /// using the proof-generating backend.
    fn to_merkle_tree(state: RefProofGenOwnedAlloc<Self>) -> Result<MerkleTree, HashError>;

    /// Parse a Merkle proof into the allocated form of this layout.
    fn from_proof(proof: ProofTree) -> FromProofResult<Self>;

    /// Compute the state hash of a partial `Verifier` state using its
    /// corresponding proof tree where data is missing.
    fn partial_state_hash(
        state: RefVerifierAlloc<Self>,
        proof: ProofTree,
    ) -> Result<Hash, PartialHashError>;
}

impl<T: ProofLayout> ProofLayout for Box<T> {
    fn to_merkle_tree(state: RefProofGenOwnedAlloc<Self>) -> Result<MerkleTree, HashError> {
        T::to_merkle_tree(*state)
    }

    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        T::from_proof(proof).map(Box::new)
    }

    fn partial_state_hash(
        state: RefVerifierAlloc<Self>,
        proof: ProofTree,
    ) -> Result<Hash, PartialHashError> {
        T::partial_state_hash(*state, proof)
    }
}

impl<T> ProofLayout for Atom<T>
where
    T: serde::Serialize + serde::de::DeserializeOwned + 'static,
{
    fn to_merkle_tree(state: RefProofGenOwnedAlloc<Self>) -> Result<MerkleTree, HashError> {
        // The Merkle leaf must hold the serialisation of the initial state.
        // Directly serialising the `ProofGen` state would produce the serialisation
        // of the final state. Therefore, we rebind and serialise the wrapped `Owned` state.
        let region = state.into_region();
        let access_info = region.get_access_info();
        let cell = super::Cell::<T, Ref<'_, Owned>>::bind(region.inner_region_ref());
        let serialised = binary::serialise(&cell)?;
        MerkleTree::make_merkle_leaf(serialised, access_info)
    }

    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        <Array<T, 1>>::from_proof(proof).map(super::Cell::from)
    }

    fn partial_state_hash(
        state: RefVerifierAlloc<Self>,
        proof: ProofTree,
    ) -> Result<Hash, PartialHashError> {
        let region = state.into_region();
        match region.get_partial_region() {
            PartialState::Complete(region) => Ok(Hash::blake2b_hash(region)?),
            PartialState::Absent => proof.partial_hash_leaf(),
            PartialState::Incomplete => Err(PartialHashError::Fatal),
        }
    }
}

impl<T, const LEN: usize> ProofLayout for Array<T, LEN>
where
    T: serde::Serialize + serde::de::DeserializeOwned + 'static,
{
    fn to_merkle_tree(state: RefProofGenOwnedAlloc<Self>) -> Result<MerkleTree, HashError> {
        // RV-282: Break down into multiple leaves if the size of the `Cells`
        // is too large for a proof.
        //
        // The Merkle leaf must hold the serialisation of the initial state.
        // Directly serialising the `ProofGen` state would produce the serialisation
        // of the final state. Therefore, we rebind and serialise the wrapped `Owned` state.
        let region = state.into_region();
        let access_info = region.get_access_info();
        let cells = super::Cells::<T, LEN, Ref<'_, Owned>>::bind(region.inner_region_ref());
        let serialised = binary::serialise(&cells)?;
        MerkleTree::make_merkle_leaf(serialised, access_info)
    }

    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        let region = if let ProofTree::Present(proof) = proof {
            let leaf = match proof {
                Tree::Node(_) => Err(FromProofError::UnexpectedNode)?,
                Tree::Leaf(leaf) => leaf,
            };

            match leaf {
                MerkleProofLeaf::Blind(_) => verify_backend::Region::Absent,
                MerkleProofLeaf::Read(data) => {
                    let cells: super::Cells<T, LEN, Owned> = binary::deserialise(data)?;
                    let arr: Box<[Option<T>; LEN]> = Box::new(cells.into_region().map(Some));
                    verify_backend::Region::Partial(arr)
                }
            }
        } else {
            verify_backend::Region::Absent
        };

        Ok(super::Cells::bind(region))
    }

    fn partial_state_hash(
        state: RefVerifierAlloc<Self>,
        proof: ProofTree,
    ) -> Result<Hash, PartialHashError> {
        let region = state.into_region();
        match region.get_partial_region() {
            PartialState::Complete(region) => Ok(Hash::blake2b_hash(region)?),
            PartialState::Absent => proof.partial_hash_leaf(),
            PartialState::Incomplete => Err(PartialHashError::Fatal),
        }
    }
}

impl<const LEN: usize> ProofLayout for DynArray<LEN> {
    fn to_merkle_tree(state: RefProofGenOwnedAlloc<Self>) -> Result<MerkleTree, HashError> {
        let region = state.region_ref();
        let mut writer = MerkleWriter::new(
            MERKLE_LEAF_SIZE,
            MERKLE_ARITY,
            region.get_read(),
            region.get_write(),
            LEN.div_ceil(MERKLE_ARITY),
        );
        let read =
            |address| -> [u8; MERKLE_LEAF_SIZE.get()] { region.inner_dyn_region_read(address) };
        chunks_to_writer::<LEN, _, _>(&mut writer, read)?;
        writer.finalise()
    }

    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        let mut pipeline = vec![(0usize, LEN, proof)];
        let mut pages = Vec::new();

        while let Some((start, length, tree)) = pipeline.pop() {
            if length <= MERKLE_LEAF_SIZE.get() {
                // Must be a leaf.
                let super::ProofPart::Present(data) = tree.into_leaf()? else {
                    // No point in doing anything if the leaf isn't present.
                    continue;
                };

                let start = verify_backend::PageId::from_address(start);

                // TODO RV-463: Leaves smaller than `MERKLE_LEAF_SIZE` should also be accepted.
                let data: Box<[u8; MERKLE_LEAF_SIZE.get()]> =
                    Box::new(data.to_vec().try_into().map_err(|_| {
                        FromProofError::UnexpectedLeafSize {
                            got: data.len(),
                            expected: MERKLE_LEAF_SIZE.get(),
                        }
                    })?);
                pages.push((start, data));
            } else {
                // Expecting a branching point.
                // TODO RV-463: Nodes with fewer than `MERKLE_ARITY` children should also be accepted.
                let branches = tree.into_branches::<{ MERKLE_ARITY }>()?;

                push_work_items_for_branches(
                    start,
                    length,
                    branches.as_slice(),
                    |branch_start, branch_length, branch| {
                        pipeline.push((branch_start, branch_length, branch));
                    },
                );
            }
        }

        let region = verify_backend::DynRegion::from_pages(pages);
        let data = super::DynCells::bind(region);
        Ok(data)
    }

    fn partial_state_hash(
        state: RefVerifierAlloc<Self>,
        proof: ProofTree,
    ) -> Result<Hash, PartialHashError> {
        enum Event<'a> {
            Span(usize, usize, ProofTree<'a>),
            Node(Option<Hash>),
        }

        let mut queue = VecDeque::new();
        queue.push_back(Event::Span(0usize, LEN, proof));

        let mut hashes: Vec<Result<Hash, PartialHashError>> = Vec::new();

        while let Some(event) = queue.pop_front() {
            match event {
                Event::Span(start, length, tree) => {
                    if length <= MERKLE_LEAF_SIZE.get() {
                        // TODO RV-463: Leaves smaller than `MERKLE_LEAF_SIZE` should also be accepted.
                        // The span's size if that of a leaf, obtain its hash if possible and push
                        // the result to the `hashes` stack.
                        match state
                            .region_ref()
                            .get_partial_page(PageId::from_address(start))
                        {
                            PartialState::Absent => hashes.push(tree.partial_hash_leaf()),
                            PartialState::Complete(data) => {
                                hashes.push(Ok(Hash::blake2b_hash_bytes(data)?))
                            }
                            PartialState::Incomplete => {
                                return Err(PartialHashError::Fatal);
                            }
                        };
                    } else {
                        // TODO RV-463: Nodes with fewer than `MERKLE_ARITY` children should also be accepted.
                        // The span's size is that of a node, produce `Event::Span` work items for each of its
                        // children and add them to the work queue, followed by an `Event::Node`.
                        let (branches, proof_hash) =
                            tree.into_branches_with_hash::<{ MERKLE_ARITY }>()?;

                        push_work_items_for_branches(
                            start,
                            length,
                            branches.as_ref(),
                            |branch_start, branch_length, branch| {
                                queue.push_back(Event::Span(branch_start, branch_length, branch));
                            },
                        );

                        queue.push_back(Event::Node(proof_hash));
                    }
                }
                Event::Node(proof_hash) => {
                    if hashes.is_empty() {
                        // The hashes which need to be combined have not yet been computed because
                        // their processing resulted in more `Event::Span` items. Push to the back
                        // of the work queue.
                        queue.push_back(Event::Node(proof_hash));
                        continue;
                    }
                    if hashes.len() < MERKLE_ARITY {
                        return Err(PartialHashError::Fatal);
                    };

                    // Take `MERKLE_ARITY` children hashes, compute their parent's hash, and
                    // push it to the `hashes` stack.
                    let node_hashes: Vec<_> = hashes.drain(hashes.len() - MERKLE_ARITY..).collect();
                    hashes.push(combine_partial_hashes(node_hashes, proof_hash))
                }
            }
        }

        if hashes.len() == 1 {
            hashes.pop().unwrap()
        } else {
            Err(PartialHashError::Fatal)
        }
    }
}

impl<A, B> ProofLayout for (A, B)
where
    A: ProofLayout,
    B: ProofLayout,
{
    fn to_merkle_tree(state: RefProofGenOwnedAlloc<Self>) -> Result<MerkleTree, HashError> {
        let children = vec![A::to_merkle_tree(state.0)?, B::to_merkle_tree(state.1)?];
        MerkleTree::make_merkle_node(children)
    }

    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        let [left, right] = *proof.into_branches()?;
        Ok((A::from_proof(left)?, B::from_proof(right)?))
    }

    fn partial_state_hash(
        state: RefVerifierAlloc<Self>,
        proof: ProofTree,
    ) -> Result<Hash, PartialHashError> {
        let (branches, proof_hash) = proof.into_branches_with_hash::<2>()?;

        let hashes = [
            A::partial_state_hash(state.0, branches[0]),
            B::partial_state_hash(state.1, branches[1]),
        ];

        combine_partial_hashes(hashes, proof_hash)
    }
}

impl<A, B, C> ProofLayout for (A, B, C)
where
    A: ProofLayout,
    B: ProofLayout,
    C: ProofLayout,
{
    fn to_merkle_tree(state: RefProofGenOwnedAlloc<Self>) -> Result<MerkleTree, HashError> {
        let children = vec![
            A::to_merkle_tree(state.0)?,
            B::to_merkle_tree(state.1)?,
            C::to_merkle_tree(state.2)?,
        ];
        MerkleTree::make_merkle_node(children)
    }

    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        let [a, b, c] = *proof.into_branches()?;
        Ok((A::from_proof(a)?, B::from_proof(b)?, C::from_proof(c)?))
    }

    fn partial_state_hash(
        state: RefVerifierAlloc<Self>,
        proof: ProofTree,
    ) -> Result<Hash, PartialHashError> {
        let (branches, proof_hash) = proof.into_branches_with_hash::<3>()?;

        let hashes = [
            A::partial_state_hash(state.0, branches[0]),
            B::partial_state_hash(state.1, branches[1]),
            C::partial_state_hash(state.2, branches[2]),
        ];

        combine_partial_hashes(hashes, proof_hash)
    }
}

impl<A, B, C, D> ProofLayout for (A, B, C, D)
where
    A: ProofLayout,
    B: ProofLayout,
    C: ProofLayout,
    D: ProofLayout,
{
    fn to_merkle_tree(state: RefProofGenOwnedAlloc<Self>) -> Result<MerkleTree, HashError> {
        let children = vec![
            A::to_merkle_tree(state.0)?,
            B::to_merkle_tree(state.1)?,
            C::to_merkle_tree(state.2)?,
            D::to_merkle_tree(state.3)?,
        ];
        MerkleTree::make_merkle_node(children)
    }

    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        let [a, b, c, d] = *proof.into_branches()?;
        Ok((
            A::from_proof(a)?,
            B::from_proof(b)?,
            C::from_proof(c)?,
            D::from_proof(d)?,
        ))
    }

    fn partial_state_hash(
        state: RefVerifierAlloc<Self>,
        proof: ProofTree,
    ) -> Result<Hash, PartialHashError> {
        let (branches, proof_hash) = proof.into_branches_with_hash::<4>()?;

        let hashes = [
            A::partial_state_hash(state.0, branches[0]),
            B::partial_state_hash(state.1, branches[1]),
            C::partial_state_hash(state.2, branches[2]),
            D::partial_state_hash(state.3, branches[3]),
        ];

        combine_partial_hashes(hashes, proof_hash)
    }
}

impl<A, B, C, D, E> ProofLayout for (A, B, C, D, E)
where
    A: ProofLayout,
    B: ProofLayout,
    C: ProofLayout,
    D: ProofLayout,
    E: ProofLayout,
{
    fn to_merkle_tree(state: RefProofGenOwnedAlloc<Self>) -> Result<MerkleTree, HashError> {
        let children = vec![
            A::to_merkle_tree(state.0)?,
            B::to_merkle_tree(state.1)?,
            C::to_merkle_tree(state.2)?,
            D::to_merkle_tree(state.3)?,
            E::to_merkle_tree(state.4)?,
        ];
        MerkleTree::make_merkle_node(children)
    }

    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        let [a, b, c, d, e] = *proof.into_branches()?;
        Ok((
            A::from_proof(a)?,
            B::from_proof(b)?,
            C::from_proof(c)?,
            D::from_proof(d)?,
            E::from_proof(e)?,
        ))
    }

    fn partial_state_hash(
        state: RefVerifierAlloc<Self>,
        proof: ProofTree,
    ) -> Result<Hash, PartialHashError> {
        let (branches, proof_hash) = proof.into_branches_with_hash::<5>()?;

        let hashes = [
            A::partial_state_hash(state.0, branches[0]),
            B::partial_state_hash(state.1, branches[1]),
            C::partial_state_hash(state.2, branches[2]),
            D::partial_state_hash(state.3, branches[3]),
            E::partial_state_hash(state.4, branches[4]),
        ];

        combine_partial_hashes(hashes, proof_hash)
    }
}

impl<A, B, C, D, E, F> ProofLayout for (A, B, C, D, E, F)
where
    A: ProofLayout,
    B: ProofLayout,
    C: ProofLayout,
    D: ProofLayout,
    E: ProofLayout,
    F: ProofLayout,
{
    fn to_merkle_tree(state: RefProofGenOwnedAlloc<Self>) -> Result<MerkleTree, HashError> {
        let children = vec![
            A::to_merkle_tree(state.0)?,
            B::to_merkle_tree(state.1)?,
            C::to_merkle_tree(state.2)?,
            D::to_merkle_tree(state.3)?,
            E::to_merkle_tree(state.4)?,
            F::to_merkle_tree(state.5)?,
        ];
        MerkleTree::make_merkle_node(children)
    }

    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        let [a, b, c, d, e, f] = *proof.into_branches()?;
        Ok((
            A::from_proof(a)?,
            B::from_proof(b)?,
            C::from_proof(c)?,
            D::from_proof(d)?,
            E::from_proof(e)?,
            F::from_proof(f)?,
        ))
    }

    fn partial_state_hash(
        state: RefVerifierAlloc<Self>,
        proof: ProofTree,
    ) -> Result<Hash, PartialHashError> {
        let (branches, proof_hash) = proof.into_branches_with_hash::<6>()?;

        let hashes = [
            A::partial_state_hash(state.0, branches[0]),
            B::partial_state_hash(state.1, branches[1]),
            C::partial_state_hash(state.2, branches[2]),
            D::partial_state_hash(state.3, branches[3]),
            E::partial_state_hash(state.4, branches[4]),
            F::partial_state_hash(state.5, branches[5]),
        ];

        combine_partial_hashes(hashes, proof_hash)
    }
}

impl<T, const LEN: usize> ProofLayout for [T; LEN]
where
    T: ProofLayout,
{
    fn to_merkle_tree(state: RefProofGenOwnedAlloc<Self>) -> Result<MerkleTree, HashError> {
        let children = state
            .into_iter()
            .map(T::to_merkle_tree)
            .collect::<Result<Vec<_>, _>>()?;

        MerkleTree::make_merkle_node(children)
    }

    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        proof
            .into_branches::<LEN>()?
            .into_iter()
            .map(T::from_proof)
            .collect::<Result<Vec<_>, _>>()?
            .try_into()
            .map_err(|_| {
                // We can't use `expected` because the error can't be displayed.
                unreachable!()
            })
    }

    fn partial_state_hash(
        state: RefVerifierAlloc<Self>,
        proof: ProofTree,
    ) -> Result<Hash, PartialHashError> {
        let (branches, proof_hash) = proof.into_branches_with_hash::<LEN>()?;
        let hashes = state
            .into_iter()
            .zip(branches.iter())
            .map(|(state, proof)| T::partial_state_hash(state, *proof))
            .collect::<Vec<Result<Hash, PartialHashError>>>();
        combine_partial_hashes(hashes, proof_hash)
    }
}

impl<T, const LEN: usize> ProofLayout for Many<T, LEN>
where
    T: ProofLayout,
{
    fn to_merkle_tree(state: RefProofGenOwnedAlloc<Self>) -> Result<MerkleTree, HashError> {
        let leaves = state
            .into_iter()
            .map(T::to_merkle_tree)
            .collect::<Result<Vec<_>, _>>()?;

        build_custom_merkle_tree(MERKLE_ARITY, leaves)
    }

    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        let mut pipeline = vec![(0usize, LEN, proof)];

        let mut data = Vec::with_capacity(LEN);
        for _ in 0..LEN {
            data.push(T::from_proof(ProofTree::Absent)?);
        }

        while let Some((start, length, tree)) = pipeline.pop() {
            if length == 1 {
                data[start] = T::from_proof(tree)?;
            } else {
                // Expecting a branching point.
                // TODO RV-463: Nodes with fewer than `MERKLE_ARITY` children should also be accepted.
                let branches = tree.into_branches::<{ MERKLE_ARITY }>()?;

                push_work_items_for_branches(
                    start,
                    length,
                    branches.as_slice(),
                    |branch_start, branch_length, branch| {
                        pipeline.push((branch_start, branch_length, branch));
                    },
                );
            }
        }
        Ok(data)
    }

    fn partial_state_hash(
        state: RefVerifierAlloc<Self>,
        proof: ProofTree,
    ) -> Result<Hash, PartialHashError> {
        enum Event<'a> {
            Span(usize, usize, ProofTree<'a>),
            Node(Option<Hash>),
        }

        // `T::partial_state_hash` needs to take ownership of the elements of `state`.
        // Given that `T` is not `Copy`, in order to take ownership of arbitrary elements
        // of `state` we'd first need to duplicate it and wrap each element in a type
        // which supports taking ownership.
        // However, in practice, we compute the hash of each element sequentially, meaning
        // that we can simply iterate over the state directly when calling `T::partial_state_hash`.
        let mut state = state.into_iter();
        let mut next_vec_index = 0;

        let mut queue = VecDeque::new();
        queue.push_back(Event::Span(0usize, LEN, proof));

        let mut hashes: Vec<Result<Hash, PartialHashError>> = Vec::new();

        while let Some(event) = queue.pop_front() {
            match event {
                Event::Span(start, length, tree) => {
                    if length == 1 {
                        // Check that iterating over the state is equivalent to calling `state[start]`
                        debug_assert_eq!(start, next_vec_index);
                        next_vec_index += 1;
                        hashes.push(T::partial_state_hash(
                            state.next().ok_or(PartialHashError::Fatal)?,
                            tree,
                        ))
                    } else {
                        // TODO RV-463: Nodes with fewer than `MERKLE_ARITY` children should also be accepted.
                        // The span's size is that of a node, produce `Event::Span` work items for each of its
                        // children and add them to the work queue, followed by an `Event::Node`.
                        let (branches, proof_hash) =
                            tree.into_branches_with_hash::<{ MERKLE_ARITY }>()?;

                        push_work_items_for_branches(
                            start,
                            length,
                            branches.as_ref(),
                            |branch_start, branch_length, branch| {
                                queue.push_back(Event::Span(branch_start, branch_length, branch));
                            },
                        );

                        queue.push_back(Event::Node(proof_hash));
                    }
                }
                Event::Node(proof_hash) => {
                    if hashes.is_empty() {
                        // The hashes which need to be combined have not yet been computed because
                        // their processing resulted in more `Event::Span` items. Push to the back
                        // of the work queue.
                        queue.push_back(Event::Node(proof_hash));
                        continue;
                    }
                    if hashes.len() < MERKLE_ARITY {
                        return Err(PartialHashError::Fatal);
                    };

                    // Take `MERKLE_ARITY` children hashes, compute their parent's hash, and
                    // push it to the `hashes` stack.
                    let node_hashes: Vec<_> = hashes.drain(hashes.len() - MERKLE_ARITY..).collect();
                    hashes.push(combine_partial_hashes(node_hashes, proof_hash))
                }
            }
        }

        // Check that we iterated over all the elements of the state
        debug_assert_eq!(next_vec_index, LEN);

        if hashes.len() == 1 {
            hashes.pop().unwrap()
        } else {
            Err(PartialHashError::Fatal)
        }
    }
}

/// Attempt to compute the partial hash of a node from its children's partial
/// hashes if they are present. If none of the children hashes can be computed
/// due to absent data, this node is either a blinded leaf in the proof, in which
/// case its hash can be recovered from the proof, or it is part of a blinded
/// subtree whose hash cannot be computed as this point.
pub fn combine_partial_hashes(
    hash_results: impl AsRef<[Result<Hash, PartialHashError>]>,
    proof_hash: Option<Hash>,
) -> Result<Hash, PartialHashError> {
    let hash_results = hash_results.as_ref();
    if hash_results.is_empty() {
        return Ok(Hash::combine(&[])?);
    }

    // If the first result is a hash, all results need to be a hash in order to
    // compute the combined hash. If the first result is a potentially
    // recoverable error, all results need to to be potentially recoverable
    // errors in order to fall back on the proof hash. Anything else is a fatal error.
    let expect_ok = match hash_results[0] {
        Ok(_) => true,
        Err(PartialHashError::PotentiallyRecoverable) => false,
        _ => return Err(PartialHashError::Fatal),
    };

    let mut hashes = Vec::with_capacity(hash_results.len());
    let hash_results_len = hash_results.len();
    for r in hash_results {
        match r {
            Ok(hash) if expect_ok => hashes.push(*hash),
            Err(PartialHashError::PotentiallyRecoverable) if !expect_ok => (),
            _ => return Err(PartialHashError::Fatal),
        }
    }

    if expect_ok {
        debug_assert_eq!(hashes.len(), hash_results_len);
        return Ok(Hash::combine(hashes.as_slice())?);
    };

    proof_hash.ok_or(PartialHashError::PotentiallyRecoverable)
}

fn push_work_items_for_branches<'a>(
    mut branch_start: usize,
    mut length_left: usize,
    branches: &'_ [ProofTree<'a>],
    mut push: impl FnMut(usize, usize, ProofTree<'a>),
) {
    let branch_max_length = length_left.div_ceil(MERKLE_ARITY);

    for branch in branches.iter() {
        let this_branch_length = branch_max_length.min(length_left);

        if this_branch_length > 0 {
            push(branch_start, this_branch_length, *branch);
        }

        branch_start = branch_start.saturating_add(this_branch_length);
        length_left = length_left.saturating_sub(this_branch_length);
    }
}

#[cfg(test)]
mod tests {
    use proptest::prop_assert;
    use proptest::prop_assert_eq;
    use proptest::proptest;
    use tests::verify_backend::handle_stepper_panics;

    use super::*;
    use crate::state_backend::Cells;
    use crate::state_backend::FnManagerIdent;
    use crate::state_backend::ManagerWrite;
    use crate::state_backend::proof_backend::ProofGen;
    use crate::state_backend::proof_backend::ProofRegion;

    const CELLS_SIZE: usize = 32;

    // When producing a proof from a `ProofGen` state, values written during
    // the execution of the tick being proven should not be blinded, whereas
    // values which were not accessed should be blinded. When a proof contains
    // blinded values, it should be possible to compute the final hash of the
    // `Verifier` state constructed from this proof.
    #[test]
    fn test_proof_blinding() {
        type TestLayout = (Array<u64, CELLS_SIZE>, Array<u64, CELLS_SIZE>);

        proptest!(|(value_before: u64, value_after: u64, i in 0..CELLS_SIZE)| {
            // Bind `ProofGen` cells and write at one address
            let cells1 = [value_before; CELLS_SIZE];
            let mut proof_region1: ProofRegion<u64, CELLS_SIZE, Ref<'_, Owned>> =
                ProofRegion::bind(&cells1);
            ProofGen::<Ref<'_, Owned>>::region_write(&mut proof_region1, i, value_after);
            let proof_cells1: Cells<u64, CELLS_SIZE, Ref<'_, ProofGen<Ref<'_, Owned>>>> =
                Cells::bind(&proof_region1);

            // Bind `ProofGen` cells and do not access them
            let cells2 = [value_before; CELLS_SIZE];
            let proof_region2: ProofRegion<u64, CELLS_SIZE, Ref<'_, Owned>> =
                ProofRegion::bind(&cells2);
            let proof_cells2: Cells<u64, CELLS_SIZE, Ref<'_, ProofGen<Ref<'_, Owned>>>> =
                Cells::bind(&proof_region2);

            let proof_state = (proof_cells1, proof_cells2);

            let merkle_proof = <TestLayout as ProofLayout>::to_merkle_tree(proof_state)
                .unwrap()
                .to_merkle_proof()
                .unwrap();
            let proof_tree = ProofTree::Present(&merkle_proof);

            let verifier_state = <TestLayout as ProofLayout>::from_proof(proof_tree).unwrap();

            // The first component of the state was present in the proof, can be
            // fully read, and contains the initial state.
            prop_assert_eq!(verifier_state.0.read_all(), vec![value_before; CELLS_SIZE]);

            // The second component of the state is fully blinded: no values can
            // be read from the array.
            for i in 0..CELLS_SIZE {
                prop_assert!(handle_stepper_panics(|| verifier_state.1.read(i)).is_err());
            };

            let ref_verifier_state = (
                verifier_state.0.struct_ref::<FnManagerIdent>(),
                verifier_state.1.struct_ref::<FnManagerIdent>(),
            );
            prop_assert!(
                <TestLayout as ProofLayout>::partial_state_hash(ref_verifier_state, proof_tree).is_ok()
            );
        })
    }
}

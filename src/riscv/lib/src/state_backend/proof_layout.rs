// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{
    hash,
    proof_backend::{
        proof::{MerkleProof, MerkleProofLeaf},
        tree::Tree,
    },
    verify_backend, Array, Atom, DynArray, Layout, Many,
};
use crate::{default::ConstDefault, storage::binary};
use std::collections::BTreeMap;

/// Errors that may occur when parsing a Merkle proof
#[derive(Debug, thiserror::Error)]
pub enum FromProofError {
    #[error("Error during hashing: {0}")]
    Hash(#[from] hash::HashError),

    #[error("Error during deserialisation: {0}")]
    Deserialise(#[from] bincode::Error),

    #[error("Encountered an invalid hash in a blinded node or leaf")]
    InvalidHash,

    #[error("Encountered a node with a bad number of branches: expected {expected}, got {got}")]
    BadNumberOfBranches { expected: usize, got: usize },

    #[error("Encountered a leaf where a node was expected")]
    UnexpectedLeaf,

    #[error("Encountered a node where a leaf was expected")]
    UnexpectedNode,
}

type Result<T, E = FromProofError> = std::result::Result<T, E>;

/// Common result type for parsing a Merkle proof
pub type FromProofResult<L> = Result<<L as Layout>::Allocated<verify_backend::Verifier>>;

/// Part of a tree that may be absent
pub enum ProofPart<'a, T: ?Sized> {
    /// This part of the tree is absent.
    Absent,

    /// There is a proof for this part of the tree.
    Present(&'a T),
}

impl<'a, T> Clone for ProofPart<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T> Copy for ProofPart<'a, T> {}

/// Part of a Merkle proof tree
pub type ProofTree<'a> = ProofPart<'a, MerkleProof>;

impl<'a> ProofTree<'a> {
    /// Interpret this part of the Merkle proof as a node with `LEN` branches.
    pub fn into_branches<const LEN: usize>(self) -> Result<[Self; LEN]> {
        let ProofTree::Present(proof) = self else {
            // The requested branches are not represented in the Merkle proof at all, not even
            // through a blinded node.
            return Ok([ProofTree::Absent; LEN]);
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
                Ok(branches.each_ref().map(ProofTree::Present))
            }

            Tree::Leaf(leaf) => match leaf {
                MerkleProofLeaf::Blind(_hash) => Ok([ProofTree::Absent; LEN]),
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
}

/// [`Layouts`] which may be used in a Merkle proof
///
/// [`Layouts`]: crate::state_backend::Layout
pub trait ProofLayout: Layout {
    /// Parse a Merkle proof into the allocated form of this layout.
    fn from_proof(proof: ProofTree) -> FromProofResult<Self>;
}

impl<T: ConstDefault + serde::de::DeserializeOwned + 'static> ProofLayout for Atom<T> {
    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        <Array<T, 1>>::from_proof(proof).map(super::Cell::from)
    }
}

impl<T: 'static, const LEN: usize> ProofLayout for Array<T, LEN>
where
    [T; LEN]: ConstDefault + serde::de::DeserializeOwned,
{
    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        let region = if let ProofTree::Present(proof) = proof {
            let leaf = match proof {
                Tree::Node(_) => Err(FromProofError::UnexpectedNode)?,
                Tree::Leaf(leaf) => leaf,
            };

            match leaf {
                MerkleProofLeaf::Blind(_) => verify_backend::Region::Absent,
                MerkleProofLeaf::Read(data) => {
                    let data: [T; LEN] = binary::deserialise(data.as_slice())?;
                    verify_backend::Region::Present(data)
                }
            }
        } else {
            verify_backend::Region::Absent
        };

        Ok(super::Cells::bind(region))
    }
}

impl<const LEN: usize> ProofLayout for DynArray<LEN> {
    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        let mut pipeline = vec![(0usize, LEN, proof)];
        let mut pages = BTreeMap::new();

        while let Some((start, length, tree)) = pipeline.pop() {
            if length <= super::MERKLE_LEAF_SIZE.get() {
                // Must be a leaf.

                let super::ProofPart::Present(data) = tree.into_leaf()? else {
                    // No point in doing anything if the leaf isn't present.
                    continue;
                };

                let data: Box<[u8]> = binary::deserialise(data)?;

                // TODO: Check data is the right length.
                assert_eq!(data.len(), length);

                pages.insert(start, data);
            } else {
                // Expecting a branching point.

                let branches = tree.into_branches::<{ super::MERKLE_ARITY }>()?;
                let branch_max_length = length.div_ceil(super::MERKLE_ARITY);

                let mut branch_start = start;
                let mut length_left = length;
                for branch in branches {
                    let this_branch_length = branch_max_length.min(length_left);

                    if this_branch_length > 0 {
                        pipeline.push((branch_start, this_branch_length, branch));
                    }

                    branch_start = branch_start.saturating_add(this_branch_length);
                    length_left = length_left.saturating_sub(this_branch_length);
                }
            }
        }

        let region = verify_backend::DynRegion::from_pages(pages);
        let data = super::DynCells::bind(region);
        Ok(data)
    }
}

impl<A, B> ProofLayout for (A, B)
where
    A: ProofLayout,
    B: ProofLayout,
{
    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        let [left, right] = proof.into_branches()?;
        Ok((A::from_proof(left)?, B::from_proof(right)?))
    }
}

impl<A, B, C> ProofLayout for (A, B, C)
where
    A: ProofLayout,
    B: ProofLayout,
    C: ProofLayout,
{
    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        let [a, b, c] = proof.into_branches()?;
        Ok((A::from_proof(a)?, B::from_proof(b)?, C::from_proof(c)?))
    }
}

impl<A, B, C, D> ProofLayout for (A, B, C, D)
where
    A: ProofLayout,
    B: ProofLayout,
    C: ProofLayout,
    D: ProofLayout,
{
    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        let [a, b, c, d] = proof.into_branches()?;
        Ok((
            A::from_proof(a)?,
            B::from_proof(b)?,
            C::from_proof(c)?,
            D::from_proof(d)?,
        ))
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
    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        let [a, b, c, d, e] = proof.into_branches()?;
        Ok((
            A::from_proof(a)?,
            B::from_proof(b)?,
            C::from_proof(c)?,
            D::from_proof(d)?,
            E::from_proof(e)?,
        ))
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
    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        let [a, b, c, d, e, f] = proof.into_branches()?;
        Ok((
            A::from_proof(a)?,
            B::from_proof(b)?,
            C::from_proof(c)?,
            D::from_proof(d)?,
            E::from_proof(e)?,
            F::from_proof(f)?,
        ))
    }
}

impl<T, const LEN: usize> ProofLayout for [T; LEN]
where
    T: ProofLayout,
{
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
}

impl<T, const LEN: usize> ProofLayout for Many<T, LEN>
where
    T: ProofLayout,
{
    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        proof
            .into_branches::<LEN>()?
            .into_iter()
            .map(|branch| T::from_proof(branch))
            .collect::<Result<Vec<_>, _>>()
    }
}

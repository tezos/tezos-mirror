// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use super::Array;
use super::Atom;
use super::DynArray;
use super::Layout;
use super::Many;
use super::Ref;
use super::RefProofGenOwnedAlloc;
use super::hash::HashError;
use super::owned_backend::Owned;
use super::proof_backend::merkle::MERKLE_ARITY;
use super::proof_backend::merkle::MERKLE_LEAF_SIZE;
use super::proof_backend::merkle::MerkleTree;
use super::proof_backend::merkle::MerkleWriter;
use super::proof_backend::merkle::chunks_to_writer;
use super::proof_backend::proof::MerkleProof;
use super::proof_backend::proof::MerkleProofLeaf;
use super::proof_backend::tree::Tree;
use super::verify_backend;
use crate::array_utils::boxed_array;
use crate::default::ConstDefault;
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
}

impl<T> ProofLayout for Atom<T>
where
    T: serde::Serialize + serde::de::DeserializeOwned + ConstDefault + 'static,
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
}

impl<T, const LEN: usize> ProofLayout for Array<T, LEN>
where
    T: serde::Serialize + serde::de::DeserializeOwned + ConstDefault + 'static,
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
                let branch_max_length = length.div_ceil(MERKLE_ARITY);

                let mut branch_start = start;
                let mut length_left = length;
                for branch in branches.into_iter() {
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
    fn to_merkle_tree(state: RefProofGenOwnedAlloc<Self>) -> Result<MerkleTree, HashError> {
        let children = vec![A::to_merkle_tree(state.0)?, B::to_merkle_tree(state.1)?];
        MerkleTree::make_merkle_node(children)
    }

    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        let [left, right] = *proof.into_branches()?;
        Ok((A::from_proof(left)?, B::from_proof(right)?))
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
}

impl<T, const LEN: usize> ProofLayout for [T; LEN]
where
    T: ProofLayout,
{
    fn to_merkle_tree(state: RefProofGenOwnedAlloc<Self>) -> Result<MerkleTree, HashError> {
        iter_to_proof::<_, T>(state)
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
}

impl<T, const LEN: usize> ProofLayout for Many<T, LEN>
where
    T: ProofLayout,
{
    fn to_merkle_tree(state: RefProofGenOwnedAlloc<Self>) -> Result<MerkleTree, HashError> {
        iter_to_proof::<_, T>(state)
    }

    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        proof
            .into_branches::<LEN>()?
            .iter()
            .copied()
            .map(T::from_proof)
            .collect::<Result<Vec<_>, _>>()
    }
}

fn iter_to_proof<'a, 'b, I, T>(iter: I) -> Result<MerkleTree, HashError>
where
    I: IntoIterator<Item = RefProofGenOwnedAlloc<'a, 'b, T>>,
    T: ProofLayout,
    'b: 'a,
{
    let children = iter
        .into_iter()
        .map(T::to_merkle_tree)
        .collect::<Result<Vec<_>, _>>()?;

    MerkleTree::make_merkle_node(children)
}

#[cfg(test)]
mod tests {
    use proptest::prop_assert;
    use proptest::prop_assert_eq;
    use proptest::proptest;
    use tests::verify_backend::handle_not_found;

    use super::*;
    use crate::state_backend::Cells;
    use crate::state_backend::ManagerWrite;
    use crate::state_backend::proof_backend::ProofGen;
    use crate::state_backend::proof_backend::ProofRegion;

    const CELLS_SIZE: usize = 32;

    // When producing a proof from a `ProofGen` state, values written during
    // the execution of the tick being proven should not be blinded, whereas
    // values which were not accessed should be blinded.
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
                prop_assert!(handle_not_found(|| verifier_state.1.read(i)).is_err());
            }
        })
    }
}

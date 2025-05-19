// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::collections::VecDeque;
use std::marker::PhantomData;

use serde::de::DeserializeOwned;

use super::deserialiser::DeserError;
use super::deserialiser::Deserialiser;
use super::deserialiser::DeserialiserNode;
use super::deserialiser::Partial;
use super::deserialiser::Result;
use super::deserialiser::Suspended;
use crate::state_backend::FromProofError;
use crate::state_backend::ProofPart;
use crate::state_backend::ProofTree;
use crate::state_backend::proof_backend::proof::MerkleProofLeaf;
use crate::state_backend::proof_backend::tree::Tree;
use crate::storage::binary;

/// Deserialiser for [`Deserialiser`] which owns the data.
pub struct ProofTreeDeserialiser<'t>(ProofTree<'t>);

impl<'t> Deserialiser for ProofTreeDeserialiser<'t> {
    type Suspended<R> = OwnedParserComb<'t, R>;

    type DeserialiserNode<R> = OwnedBranchComb<'t, R, Self>;

    fn into_leaf_raw<const LEN: usize>(self) -> Result<Self::Suspended<Partial<Box<[u8; LEN]>>>> {
        self.deserialise_as_leaf()?
            .map_present_fallible(|data| {
                let data_len = data.len();
                let bytes: Box<[u8; LEN]> =
                    data.try_into()
                        .map_err(|_| DeserError::UnexpectedLeafSize {
                            expected: LEN,
                            got: data_len,
                        })?;
                Ok(bytes)
            })
            .map(OwnedParserComb::new)
    }

    fn into_leaf<T: DeserializeOwned + 'static>(self) -> Result<Self::Suspended<Partial<T>>> {
        self.deserialise_as_leaf()?
            .map_present_fallible(|data| Ok(binary::deserialise::<T>(data.as_ref())?))
            .map(OwnedParserComb::new)
    }

    fn into_node(self) -> Result<Self::DeserialiserNode<Partial<()>>> {
        let branches = self.deserialise_as_node()?;
        Ok(OwnedBranchComb::new(branches))
    }
}

impl<'t> From<ProofTree<'t>> for ProofTreeDeserialiser<'t> {
    fn from(proof: ProofTree<'t>) -> Self {
        ProofTreeDeserialiser(proof)
    }
}

impl ProofTreeDeserialiser<'_> {
    /// Deserialise the proof as a leaf.
    pub fn deserialise_as_leaf(self) -> Result<Partial<Vec<u8>>> {
        match self.0 {
            ProofPart::Absent => Ok(Partial::Absent),
            ProofPart::Present(Tree::Node(_)) => Err(FromProofError::UnexpectedNode),
            ProofPart::Present(Tree::Leaf(MerkleProofLeaf::Blind(hash))) => {
                Ok(Partial::Blinded(*hash))
            }
            ProofPart::Present(Tree::Leaf(MerkleProofLeaf::Read(items))) => {
                Ok(Partial::Present(items.clone()))
            }
        }
    }

    /// Deserialise the proof as a node.
    pub fn deserialise_as_node(self) -> Result<Partial<Vec<Self>>> {
        match self.0 {
            ProofPart::Absent => Ok(Partial::Absent),
            ProofPart::Present(Tree::Leaf(MerkleProofLeaf::Blind(hash))) => {
                Ok(Partial::Blinded(*hash))
            }
            ProofPart::Present(Tree::Leaf(MerkleProofLeaf::Read(_))) => {
                Err(FromProofError::UnexpectedLeaf)
            }
            ProofPart::Present(Tree::Node(trees)) => Ok(Partial::Present(
                trees
                    .iter()
                    .map(ProofPart::Present)
                    .map(ProofTreeDeserialiser)
                    .collect(),
            )),
        }
    }
}

/// Suspended computation combinator for [`ProofTreeDeserialiser`] deserialiser.
pub struct OwnedParserComb<'t, R> {
    result: R,
    _pd: PhantomData<fn(ProofTreeDeserialiser<'t>)>,
}

impl<R> OwnedParserComb<'_, R> {
    fn new(result: R) -> Self {
        Self {
            result,
            _pd: PhantomData,
        }
    }
}

/// Branch deserialiser combinator for [`ProofTreeDeserialiser`] deserialiser.
pub struct OwnedBranchComb<'p, R, B> {
    f: OwnedParserComb<'p, R>,
    node_data: Partial<VecDeque<B>>,
}

impl<B> OwnedBranchComb<'_, Partial<()>, B> {
    /// Create a new [`OwnedBranchComb`] with the given branches,
    /// preserving the absent/blind/present information from the given [`Partial`].
    fn new(branches: Partial<Vec<B>>) -> Self {
        // Similar to `map_present` but for `&Partial<R>`.
        // This is done to preserve absent/blind/present information from node until calling `done()`.
        // See test_blind_node_parsing for an example.
        let f_comb = match &branches {
            Partial::Absent => Partial::Absent,
            Partial::Blinded(hash) => Partial::Blinded(*hash),
            Partial::Present(_) => Partial::Present(()),
        };

        Self {
            f: OwnedParserComb::new(f_comb),
            node_data: branches.map_present(VecDeque::from),
        }
    }
}

impl<'t, R> DeserialiserNode<R> for OwnedBranchComb<'t, R, ProofTreeDeserialiser<'t>> {
    type Parent = ProofTreeDeserialiser<'t>;

    fn next_branch<T>(
        mut self,
        branch_deserialiser: impl FnOnce(
            Self::Parent,
        )
            -> Result<<Self::Parent as Deserialiser>::Suspended<T>>,
    ) -> Result<<Self::Parent as Deserialiser>::DeserialiserNode<(R, T)>>
    where
        R: 'static,
        T: 'static,
    {
        let next_branch = match self.node_data {
            // If the node is absent or blinded, the branch to be deserialised as a tree is absent.
            Partial::Absent | Partial::Blinded(_) => ProofTreeDeserialiser(ProofTree::Absent),
            Partial::Present(ref mut branches) => {
                branches
                    .pop_front()
                    .ok_or(DeserError::BadNumberOfBranches {
                        expected: 1,
                        got: 0,
                    })?
            }
        };
        let br_comb = branch_deserialiser(next_branch)?;

        Ok(OwnedBranchComb {
            f: self.f.zip(br_comb),
            node_data: self.node_data,
        })
    }

    fn map<T>(
        self,
        f: impl FnOnce(R) -> T + 'static,
    ) -> <Self::Parent as Deserialiser>::DeserialiserNode<T>
    where
        T: 'static,
        R: 'static,
    {
        OwnedBranchComb {
            f: self.f.map(f),
            node_data: self.node_data,
        }
    }

    fn done(self) -> Result<<Self::Parent as Deserialiser>::Suspended<R>> {
        if let Partial::Present(branches) = self.node_data {
            if !branches.is_empty() {
                let length = branches.len();
                return Err(DeserError::BadNumberOfBranches {
                    expected: 0,
                    got: length,
                });
            }
        }

        Ok(self.f)
    }
}

impl<'t, R> Suspended for OwnedParserComb<'t, R> {
    type Output = R;

    type Parent = ProofTreeDeserialiser<'t>;

    fn map<T>(
        self,
        f: impl FnOnce(Self::Output) -> T + 'static,
    ) -> <Self::Parent as Deserialiser>::Suspended<T>
    where
        Self::Output: 'static,
    {
        OwnedParserComb::new(f(self.result))
    }

    fn zip<T>(
        self,
        other: <Self::Parent as Deserialiser>::Suspended<T>,
    ) -> <Self::Parent as Deserialiser>::Suspended<(Self::Output, T)>
    where
        Self::Output: 'static,
        T: 'static,
    {
        OwnedParserComb::new((self.result, other.result))
    }
}

impl<R> OwnedParserComb<'_, R> {
    pub fn into_result(self) -> R {
        self.result
    }
}

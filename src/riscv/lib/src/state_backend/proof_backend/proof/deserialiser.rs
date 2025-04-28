// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Module for defining traits which facilitate desserialising a tree structure.
//! More precisely, our usecase if for deserialising a Merkle tree either from a [`ProofTree`] or
//! from the raw bytes of a serialisation.
//!
//! Due to the nature of the deserialisation, the structure of the tree is not known until part of the
//! deserialisation is already parsed and the shape is known. This introduces the need for the
//! [`Suspended`] trait which abstracts over a computation to be obtained after parsing just enough from
//! the serialisation to deduce the shape of the tree.
//!
//! [`ProofTree`]: crate::state_backend::ProofTree

use serde::de::DeserializeOwned;

use crate::state_backend::FromProofError;
use crate::state_backend::hash::Hash;

/// Error used when deserialising using [`Deserialiser`] methods
pub type DeserError = FromProofError;

/// Result type used when deserialising using [`Deserialiser`] methods.
pub type Result<R, E = DeserError> = std::result::Result<R, E>;

/// Possible outcomes when parsing a node or a leaf from a Merkle proof
/// where the leaf is assumed to have type `T`.
pub enum Partial<T> {
    /// The leaf / node is altogether absent from the proof.
    Absent,
    /// A blinded subtree and its [`struct@Hash`] is provided.
    Blinded(Hash),
    /// Data successfully parsed and its type is `T`.
    Present(T),
}

impl<T> Partial<T> {
    /// Map the present result of a [`Partial<T>`] into [`Partial<R>`].
    pub fn map_present<R>(self, f: impl FnOnce(T) -> R) -> Partial<R> {
        match self {
            Partial::Absent => Partial::Absent,
            Partial::Blinded(hash) => Partial::Blinded(hash),
            Partial::Present(data) => Partial::Present(f(data)),
        }
    }

    /// Same as [`Partial::map_present`] but can fail.
    pub fn map_present_fallible<R, E>(
        self,
        f: impl FnOnce(T) -> Result<R, E>,
    ) -> Result<Partial<R>, E> {
        match self {
            Partial::Absent => Ok(Partial::Absent),
            Partial::Blinded(hash) => Ok(Partial::Blinded(hash)),
            Partial::Present(data) => Ok(Partial::Present(f(data)?)),
        }
    }
}

/// The main trait used for deserialising a proof.
///
/// Having an object of this trait is equivalent to having a proof and being able to deserialise it.
///
/// A proof can be interpreted in 3 cases:
/// 1. [`Deserialiser::into_leaf_raw`] The proof is a leaf and raw bytes are obtained.
/// 2. [`Deserialiser::into_leaf<T>`] The proof is a leaf and the type `T` is parsed.
/// 3. [`Deserialiser::into_node`] The proof is a node in the tree.
pub trait Deserialiser {
    /// After deserialising a proof, a [`Suspended<R>`] computation is obtained.
    type Suspended<R>: Suspended<Output = R, Parent = Self>;

    /// In case the proof is a node, [`Deserialiser::DeserialiserNode`] is the deserialiser for the branch case.
    type DeserialiserNode<R>: DeserialiserNode<R, Parent = Self>;

    /// It is expected for the proof to be a leaf. Obtain the raw bytes from that leaf.
    fn into_leaf_raw<const LEN: usize>(self) -> Result<Self::Suspended<Partial<Box<[u8; LEN]>>>>;

    /// It is expected for the proof to be a leaf. Parse the raw bytes of that leaf into a type `T`.
    fn into_leaf<T: DeserializeOwned + 'static>(self) -> Result<Self::Suspended<Partial<T>>>;

    /// It is expected for the proof to be a node. Obtain the deserialiser for the branch case.
    fn into_node(self) -> Result<Self::DeserialiserNode<Partial<()>>>;
}

/// The trait used for deserialising a proof's node.
/// Having an object of this trait is equivalent to knowing the current proof is a node.
/// Deserialisers for each of its branches are expected to be provided to continue the deserialisation.
pub trait DeserialiserNode<R> {
    type Parent: Deserialiser;

    /// The next branch of the current node is deserialised using the provided deserialiser `br_deser`.
    fn next_branch<T>(
        self,
        branch_deserialiser: impl FnOnce(
            Self::Parent,
        )
            -> Result<<Self::Parent as Deserialiser>::Suspended<T>>,
    ) -> Result<<Self::Parent as Deserialiser>::DeserialiserNode<(R, T)>>
    where
        R: 'static,
        T: 'static;

    /// Helper for mapping the current result into a new type.
    fn map<T>(
        self,
        f: impl FnOnce(R) -> T + 'static,
    ) -> <Self::Parent as Deserialiser>::DeserialiserNode<T>
    where
        T: 'static,
        R: 'static;

    /// Signal the end of deserialisation of the node's branches.
    /// Call this method after all calls to [`DeserialiserNode::next_branch`] have been made.
    fn done(self) -> Result<<Self::Parent as Deserialiser>::Suspended<R>>;
}

/// The trait represents a computation function obtained after deserialising a proof.
pub trait Suspended {
    /// End result of the computation.
    type Output;

    type Parent: Deserialiser;

    /// Helper to map the current result into a new type.
    fn map<T>(
        self,
        f: impl FnOnce(Self::Output) -> T + 'static,
    ) -> <Self::Parent as Deserialiser>::Suspended<T>
    where
        Self::Output: 'static;

    /// Helper to zip the current result with another result.
    fn zip<T>(
        self,
        other: <Self::Parent as Deserialiser>::Suspended<T>,
    ) -> <Self::Parent as Deserialiser>::Suspended<(Self::Output, T)>
    where
        Self::Output: 'static,
        T: 'static;
}
/// Helper trait for transforming `Self`` to a suspended computation from a given serialiser.
pub trait FromProof {
    type Output: Sized;

    fn from_proof<D: Deserialiser>(de: D) -> Result<D::Suspended<Self::Output>>;
}

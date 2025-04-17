// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use super::AllocatedOf;
use super::Array;
use super::Atom;
use super::DynArray;
use super::Layout;
use super::ManagerSerialise;
use super::Many;
use super::hash;
use super::hash::Hash;
use super::hash::HashError;
use super::hash::HashWriter;
use super::proof_backend::merkle::MERKLE_ARITY;
use super::proof_backend::merkle::MERKLE_LEAF_SIZE;
use super::proof_backend::merkle::chunks_to_writer;
use crate::state_backend::hash::build_custom_merkle_hash;

/// [`Layouts`] which may be used for commitments
///
/// [`Layouts`]: crate::state_backend::Layout
pub trait CommitmentLayout: Layout {
    /// Compute the root hash of the given state
    fn state_hash<M: ManagerSerialise>(state: AllocatedOf<Self, M>) -> Result<Hash, HashError>;
}

impl<T: CommitmentLayout> CommitmentLayout for Box<T> {
    fn state_hash<M: ManagerSerialise>(state: AllocatedOf<Self, M>) -> Result<Hash, HashError> {
        T::state_hash(*state)
    }
}

impl<T> CommitmentLayout for Atom<T>
where
    T: serde::Serialize + 'static,
{
    fn state_hash<M: ManagerSerialise>(state: AllocatedOf<Self, M>) -> Result<Hash, HashError> {
        Hash::blake2b_hash(state)
    }
}

impl<T, const LEN: usize> CommitmentLayout for Array<T, LEN>
where
    T: serde::Serialize + Copy + 'static,
{
    fn state_hash<M: ManagerSerialise>(state: AllocatedOf<Self, M>) -> Result<Hash, HashError> {
        Hash::blake2b_hash(state)
    }
}

impl<const LEN: usize> CommitmentLayout for DynArray<LEN> {
    fn state_hash<M: ManagerSerialise>(state: AllocatedOf<Self, M>) -> Result<Hash, HashError> {
        let mut writer = HashWriter::new(MERKLE_LEAF_SIZE);
        chunks_to_writer::<LEN, _, _>(&mut writer, |address| {
            state.read::<[u8; MERKLE_LEAF_SIZE.get()]>(address)
        })?;
        let hashes = writer.finalise()?;
        hash::build_custom_merkle_hash(MERKLE_ARITY, hashes)
    }
}

impl<A, B> CommitmentLayout for (A, B)
where
    A: CommitmentLayout,
    B: CommitmentLayout,
{
    fn state_hash<M: ManagerSerialise>(state: AllocatedOf<Self, M>) -> Result<Hash, HashError> {
        let hashes = [A::state_hash(state.0)?, B::state_hash(state.1)?];
        Hash::combine(&hashes)
    }
}

impl<A, B, C> CommitmentLayout for (A, B, C)
where
    A: CommitmentLayout,
    B: CommitmentLayout,
    C: CommitmentLayout,
{
    fn state_hash<M: ManagerSerialise>(state: AllocatedOf<Self, M>) -> Result<Hash, HashError> {
        let hashes = [
            A::state_hash(state.0)?,
            B::state_hash(state.1)?,
            C::state_hash(state.2)?,
        ];
        Hash::combine(&hashes)
    }
}

impl<A, B, C, D> CommitmentLayout for (A, B, C, D)
where
    A: CommitmentLayout,
    B: CommitmentLayout,
    C: CommitmentLayout,
    D: CommitmentLayout,
{
    fn state_hash<M: ManagerSerialise>(state: AllocatedOf<Self, M>) -> Result<Hash, HashError> {
        let hashes = [
            A::state_hash(state.0)?,
            B::state_hash(state.1)?,
            C::state_hash(state.2)?,
            D::state_hash(state.3)?,
        ];
        Hash::combine(&hashes)
    }
}

impl<A, B, C, D, E> CommitmentLayout for (A, B, C, D, E)
where
    A: CommitmentLayout,
    B: CommitmentLayout,
    C: CommitmentLayout,
    D: CommitmentLayout,
    E: CommitmentLayout,
{
    fn state_hash<M: ManagerSerialise>(state: AllocatedOf<Self, M>) -> Result<Hash, HashError> {
        let hashes = [
            A::state_hash(state.0)?,
            B::state_hash(state.1)?,
            C::state_hash(state.2)?,
            D::state_hash(state.3)?,
            E::state_hash(state.4)?,
        ];
        Hash::combine(&hashes)
    }
}

impl<A, B, C, D, E, F> CommitmentLayout for (A, B, C, D, E, F)
where
    A: CommitmentLayout,
    B: CommitmentLayout,
    C: CommitmentLayout,
    D: CommitmentLayout,
    E: CommitmentLayout,
    F: CommitmentLayout,
{
    fn state_hash<M: ManagerSerialise>(state: AllocatedOf<Self, M>) -> Result<Hash, HashError> {
        let hashes = [
            A::state_hash(state.0)?,
            B::state_hash(state.1)?,
            C::state_hash(state.2)?,
            D::state_hash(state.3)?,
            E::state_hash(state.4)?,
            F::state_hash(state.5)?,
        ];
        Hash::combine(&hashes)
    }
}

impl<T, const LEN: usize> CommitmentLayout for [T; LEN]
where
    T: CommitmentLayout,
{
    fn state_hash<M: ManagerSerialise>(state: AllocatedOf<Self, M>) -> Result<Hash, HashError> {
        let hashes: Vec<Hash> = state
            .into_iter()
            .map(T::state_hash)
            .collect::<Result<Vec<_>, _>>()?;
        Hash::combine(&hashes)
    }
}

impl<T, const LEN: usize> CommitmentLayout for Many<T, LEN>
where
    T: CommitmentLayout,
{
    fn state_hash<M: ManagerSerialise>(state: AllocatedOf<Self, M>) -> Result<Hash, HashError> {
        let nodes: Vec<Hash> = state
            .into_iter()
            .map(T::state_hash)
            .collect::<Result<Vec<_>, _>>()?;
        build_custom_merkle_hash(MERKLE_ARITY, nodes)
    }
}

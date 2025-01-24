// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use super::{
    hash::{self, Hash, HashError, HashWriter},
    proof_backend::merkle::{chunks_to_writer, MERKLE_ARITY, MERKLE_LEAF_SIZE},
    Array, Atom, DynArray, Layout, Many, RefOwnedAlloc,
};
use crate::default::ConstDefault;

/// [`Layouts`] which may be used for commitments
///
/// [`Layouts`]: crate::state_backend::Layout
pub trait CommitmentLayout: Layout {
    /// Compute the root hash of the given state
    fn state_hash(state: RefOwnedAlloc<Self>) -> Result<Hash, HashError>;
}

impl<T: ConstDefault + serde::Serialize + 'static> CommitmentLayout for Atom<T> {
    fn state_hash(state: RefOwnedAlloc<Self>) -> Result<Hash, HashError> {
        Hash::blake2b_hash(state)
    }
}

impl<T: serde::Serialize + 'static, const LEN: usize> CommitmentLayout for Array<T, LEN>
where
    [T; LEN]: ConstDefault,
{
    fn state_hash(state: RefOwnedAlloc<Self>) -> Result<Hash, HashError> {
        Hash::blake2b_hash(state)
    }
}

impl<const LEN: usize> CommitmentLayout for DynArray<LEN> {
    fn state_hash(state: RefOwnedAlloc<Self>) -> Result<Hash, HashError> {
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
    fn state_hash(state: RefOwnedAlloc<Self>) -> Result<Hash, HashError> {
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
    fn state_hash(state: RefOwnedAlloc<Self>) -> Result<Hash, HashError> {
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
    fn state_hash(state: RefOwnedAlloc<Self>) -> Result<Hash, HashError> {
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
    fn state_hash(state: RefOwnedAlloc<Self>) -> Result<Hash, HashError> {
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
    fn state_hash(state: RefOwnedAlloc<Self>) -> Result<Hash, HashError> {
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
    fn state_hash(state: RefOwnedAlloc<Self>) -> Result<Hash, HashError> {
        iter_state_hash::<_, T, LEN>(state)
    }
}

impl<T, const LEN: usize> CommitmentLayout for Many<T, LEN>
where
    T: CommitmentLayout,
{
    fn state_hash(state: RefOwnedAlloc<Self>) -> Result<Hash, HashError> {
        iter_state_hash::<_, T, LEN>(state)
    }
}

fn iter_state_hash<'a, I, T, const LEN: usize>(iter: I) -> Result<Hash, HashError>
where
    I: IntoIterator<Item = RefOwnedAlloc<'a, T>>,
    T: CommitmentLayout,
{
    let hashes: Vec<Hash> = iter
        .into_iter()
        .map(T::state_hash)
        .collect::<Result<Vec<_>, _>>()?;

    Hash::combine(&hashes)
}

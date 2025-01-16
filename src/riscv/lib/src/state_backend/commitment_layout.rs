// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use super::{
    chunks_to_writer,
    hash::{self, Hash, HashError, HashWriter, RootHashable, DIGEST_SIZE},
    owned_backend::Owned,
    AllocatedOf, Array, Atom, DynArray, Layout, Many, Ref, MERKLE_ARITY, MERKLE_LEAF_SIZE,
};
use crate::default::ConstDefault;

/// [`Layouts`] which may be used for commitments
///
/// [`Layouts`]: crate::state_backend::Layout
pub trait CommitmentLayout: Layout {
    /// Compute the root hash of the given state
    fn state_hash(state: AllocatedOf<Self, Ref<'_, Owned>>) -> Result<Hash, HashError>;
}

impl<T: ConstDefault + serde::Serialize + 'static> CommitmentLayout for Atom<T> {
    fn state_hash(state: AllocatedOf<Self, Ref<'_, Owned>>) -> Result<Hash, HashError> {
        Hash::blake2b_hash(state)
    }
}

impl<T: serde::Serialize + 'static, const LEN: usize> CommitmentLayout for Array<T, LEN>
where
    [T; LEN]: ConstDefault,
{
    fn state_hash(state: AllocatedOf<Self, Ref<'_, Owned>>) -> Result<Hash, HashError> {
        Hash::blake2b_hash(state)
    }
}

impl<const LEN: usize> CommitmentLayout for DynArray<LEN> {
    fn state_hash(state: AllocatedOf<Self, Ref<'_, Owned>>) -> Result<Hash, HashError> {
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
    fn state_hash(state: AllocatedOf<Self, Ref<'_, Owned>>) -> Result<Hash, HashError> {
        let hashes = [A::state_hash(state.0)?, B::state_hash(state.1)?];
        hashes.hash()
    }
}

impl<A, B, C> CommitmentLayout for (A, B, C)
where
    A: CommitmentLayout,
    B: CommitmentLayout,
    C: CommitmentLayout,
{
    fn state_hash(state: AllocatedOf<Self, Ref<'_, Owned>>) -> Result<Hash, HashError> {
        let hashes = [
            A::state_hash(state.0)?,
            B::state_hash(state.1)?,
            C::state_hash(state.2)?,
        ];
        hashes.hash()
    }
}

impl<A, B, C, D> CommitmentLayout for (A, B, C, D)
where
    A: CommitmentLayout,
    B: CommitmentLayout,
    C: CommitmentLayout,
    D: CommitmentLayout,
{
    fn state_hash(state: AllocatedOf<Self, Ref<'_, Owned>>) -> Result<Hash, HashError> {
        let hashes = [
            A::state_hash(state.0)?,
            B::state_hash(state.1)?,
            C::state_hash(state.2)?,
            D::state_hash(state.3)?,
        ];
        hashes.hash()
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
    fn state_hash(state: AllocatedOf<Self, Ref<'_, Owned>>) -> Result<Hash, HashError> {
        let hashes = [
            A::state_hash(state.0)?,
            B::state_hash(state.1)?,
            C::state_hash(state.2)?,
            D::state_hash(state.3)?,
            E::state_hash(state.4)?,
        ];
        hashes.hash()
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
    fn state_hash(state: AllocatedOf<Self, Ref<'_, Owned>>) -> Result<Hash, HashError> {
        let hashes = [
            A::state_hash(state.0)?,
            B::state_hash(state.1)?,
            C::state_hash(state.2)?,
            D::state_hash(state.3)?,
            E::state_hash(state.4)?,
            F::state_hash(state.5)?,
        ];
        hashes.hash()
    }
}

impl<T, const LEN: usize> CommitmentLayout for [T; LEN]
where
    T: CommitmentLayout,
{
    fn state_hash(state: AllocatedOf<Self, Ref<'_, Owned>>) -> Result<Hash, HashError> {
        iter_state_hash::<_, T, LEN>(state)
    }
}

impl<T, const LEN: usize> CommitmentLayout for Many<T, LEN>
where
    T: CommitmentLayout,
{
    fn state_hash(state: AllocatedOf<Self, Ref<'_, Owned>>) -> Result<Hash, HashError> {
        iter_state_hash::<_, T, LEN>(state)
    }
}

fn iter_state_hash<'a, I, T, const LEN: usize>(iter: I) -> Result<Hash, HashError>
where
    I: IntoIterator<Item = AllocatedOf<T, Ref<'a, Owned>>>,
    T: CommitmentLayout,
{
    let mut hashes: Vec<u8> = Vec::with_capacity(DIGEST_SIZE * LEN);

    iter.into_iter().try_for_each(|e| {
        hashes.extend_from_slice(T::state_hash(e)?.as_ref());
        Ok::<(), HashError>(())
    })?;

    // TODO RV-250: Instead of building the whole input and hashing it,
    // we should use incremental hashing, which isn't currently supported
    // in `tezos_crypto_rs`.
    Hash::blake2b_hash_bytes(&hashes)
}

// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::convert::Infallible;
use std::marker::PhantomData;

use crate::default::ConstDefault;
use crate::machine_state::memory::Address;
use crate::state_backend::AllocatedOf;
use crate::state_backend::CommitmentLayout;
use crate::state_backend::FromProofResult;
use crate::state_backend::Layout;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerSerialise;
use crate::state_backend::Many;
use crate::state_backend::ProofLayout;
use crate::state_backend::ProofTree;
use crate::storage::Hash;
use crate::storage::HashError;

/// Integer to keep track of the fence counter
#[derive(
    Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
pub struct FenceCounter(pub u32);

impl FenceCounter {
    /// Initial fence counter
    pub const INITIAL: Self = Self(0);

    /// Maximum fence counter value
    pub const MAX: Self = Self(u32::MAX);

    /// Increment the fence counter.
    #[inline]
    pub fn next(self) -> Self {
        Self(self.0.wrapping_add(1))
    }
}

impl ConstDefault for FenceCounter {
    const DEFAULT: Self = Self::INITIAL;
}

/// Configuration object for the size of a cache indexed by physical address.
///
/// *NB* you should ensure `SIZE == 1 << BITS`, otherwise a compilation error will occur.
#[derive(Clone)]
pub struct Sizes<const BITS: usize, const SIZE: usize, CachedLayout>(
    PhantomData<CachedLayout>,
    Infallible,
);

impl<const BITS: usize, const SIZE: usize, CachedLayout> Sizes<BITS, SIZE, CachedLayout> {
    pub const CACHE_SIZE: usize = if 1 << BITS == SIZE {
        SIZE
    } else {
        panic!("BITS parameter does not match SIZE parameter");
    };

    const CACHE_MASK: usize = {
        Self::fence_counter_wrapping_protection();
        Self::CACHE_SIZE - 1
    };

    // We know that phys_addr here is always u16-aligned.
    // Therefore, we can safely halve the number of buckets we
    // look at.
    #[inline(always)]
    pub const fn cache_index(phys_addr: Address) -> usize {
        (phys_addr >> 1) as usize & Self::CACHE_MASK
    }

    /// Assert that the fence counter would not wrap before every cache entry has been invalidated
    /// _at least_ once.
    const fn fence_counter_wrapping_protection() {
        let invalidation_count_until_wrapping = FenceCounter::MAX.0 as usize;
        let cache_entries = Self::CACHE_SIZE;

        assert!(
            invalidation_count_until_wrapping > cache_entries,
            "The fence counter does a full cycle before all cache entries could be invalidated!"
        );
    }
}

impl<const BITS: usize, const SIZE: usize, CachedLayout: Layout> Layout
    for Sizes<BITS, SIZE, CachedLayout>
{
    type Allocated<M: ManagerBase> = <Many<CachedLayout, SIZE> as Layout>::Allocated<M>;
}

impl<const BITS: usize, const SIZE: usize, CachedLayout: CommitmentLayout> CommitmentLayout
    for Sizes<BITS, SIZE, CachedLayout>
{
    fn state_hash<M: ManagerSerialise>(state: AllocatedOf<Self, M>) -> Result<Hash, HashError> {
        Many::<CachedLayout, SIZE>::state_hash(state)
    }
}

impl<const BITS: usize, const SIZE: usize, CachedLayout: ProofLayout> ProofLayout
    for Sizes<BITS, SIZE, CachedLayout>
{
    fn to_merkle_tree(
        state: crate::state_backend::RefProofGenOwnedAlloc<Self>,
    ) -> Result<crate::state_backend::proof_backend::merkle::MerkleTree, HashError> {
        Many::<CachedLayout, SIZE>::to_merkle_tree(state)
    }

    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        Many::<CachedLayout, SIZE>::from_proof(proof)
    }

    fn partial_state_hash(
        state: crate::state_backend::RefVerifierAlloc<Self>,
        proof: crate::state_backend::ProofTree,
    ) -> Result<Hash, crate::state_backend::PartialHashError> {
        Many::<CachedLayout, SIZE>::partial_state_hash(state, proof)
    }
}

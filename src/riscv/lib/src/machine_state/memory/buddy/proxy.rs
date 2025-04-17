// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Simplified [`BuddyLayout`] selection using const-generics

use super::BuddyLayout;
use super::branch_combinations::BuddyBranch1KiLayout;
use super::branch_combinations::BuddyBranch4Layout;
use super::branch_combinations::BuddyBranch16Layout;
use super::branch_combinations::BuddyBranch256Layout;
use super::leaf::BuddyLeafLayout;
use crate::state_backend::AllocatedOf;
use crate::state_backend::CommitmentLayout;
use crate::state_backend::FnManager;
use crate::state_backend::FromProofResult;
use crate::state_backend::Layout;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerSerialise;
use crate::state_backend::PartialHashError;
use crate::state_backend::ProofLayout;
use crate::state_backend::ProofTree;
use crate::state_backend::Ref;
use crate::state_backend::RefProofGenOwnedAlloc;
use crate::state_backend::RefVerifierAlloc;
use crate::state_backend::proof_backend::merkle::MerkleTree;
use crate::storage::Hash;
use crate::storage::HashError;

/// Proxy for a [`BuddyLayout`] that manages the specified number of `PAGES`
pub struct BuddyLayoutProxy<const PAGES: usize>;

impl<const PAGES: usize> Layout for BuddyLayoutProxy<PAGES>
where
    (): BuddyLayoutMatch<PAGES>,
{
    type Allocated<M: ManagerBase> = <PickLayout<PAGES> as Layout>::Allocated<M>;
}

impl<const PAGES: usize> CommitmentLayout for BuddyLayoutProxy<PAGES>
where
    (): BuddyLayoutMatch<PAGES>,
{
    fn state_hash<M: ManagerSerialise>(state: AllocatedOf<Self, M>) -> Result<Hash, HashError> {
        <PickLayout<PAGES> as CommitmentLayout>::state_hash(state)
    }
}

impl<const PAGES: usize> ProofLayout for BuddyLayoutProxy<PAGES>
where
    (): BuddyLayoutMatch<PAGES>,
{
    fn to_merkle_tree(state: RefProofGenOwnedAlloc<Self>) -> Result<MerkleTree, HashError> {
        <PickLayout<PAGES> as ProofLayout>::to_merkle_tree(state)
    }

    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        <PickLayout<PAGES> as ProofLayout>::from_proof(proof)
    }

    fn partial_state_hash(
        state: RefVerifierAlloc<Self>,
        proof: ProofTree,
    ) -> Result<Hash, PartialHashError> {
        <PickLayout<PAGES> as ProofLayout>::partial_state_hash(state, proof)
    }
}

impl<const PAGES: usize> BuddyLayout for BuddyLayoutProxy<PAGES>
where
    (): BuddyLayoutMatch<PAGES>,
{
    type Buddy<M: ManagerBase> = <PickLayout<PAGES> as BuddyLayout>::Buddy<M>;

    fn bind<M: ManagerBase>(space: Self::Allocated<M>) -> Self::Buddy<M> {
        <PickLayout<PAGES> as BuddyLayout>::bind(space)
    }

    fn struct_ref<'a, F, M: ManagerBase>(space: &'a Self::Buddy<M>) -> Self::Allocated<F::Output>
    where
        F: FnManager<Ref<'a, M>>,
    {
        <PickLayout<PAGES> as BuddyLayout>::struct_ref::<F, M>(space)
    }
}

/// Picks a [`BuddyLayout`] given a number of pages
type PickLayout<const PAGES: usize, T = ()> = <T as BuddyLayoutMatch<PAGES>>::AssocLayout;

/// Link between a number of pages and a specific [`BuddyLayout`]
pub trait BuddyLayoutMatch<const PAGES: usize> {
    type AssocLayout: BuddyLayout;
}

impl<T> BuddyLayoutMatch<1> for T {
    type AssocLayout = BuddyLeafLayout<1>;
}

impl<T> BuddyLayoutMatch<2> for T {
    type AssocLayout = BuddyLeafLayout<2>;
}

impl<T> BuddyLayoutMatch<64> for T {
    type AssocLayout = BuddyLeafLayout<64>;
}

impl<T> BuddyLayoutMatch<256> for T {
    type AssocLayout = BuddyBranch4Layout<BuddyLeafLayout<64>>;
}

impl<T> BuddyLayoutMatch<1024> for T {
    type AssocLayout = BuddyBranch4Layout<BuddyLayoutProxy<256>>;
}

impl<T> BuddyLayoutMatch<{ 16 * 1024 }> for T {
    type AssocLayout = BuddyBranch16Layout<BuddyLayoutProxy<1024>>;
}

impl<T> BuddyLayoutMatch<{ 256 * 1024 }> for T {
    type AssocLayout = BuddyBranch256Layout<BuddyLayoutProxy<1024>>;
}

impl<T> BuddyLayoutMatch<{ 1024 * 1024 }> for T {
    type AssocLayout = BuddyBranch1KiLayout<BuddyLayoutProxy<1024>>;
}

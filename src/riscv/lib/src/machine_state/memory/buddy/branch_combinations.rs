// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Like [`super::branch`] but for various branch sizes
//!
//! Introducing more types instead of composing [`BuddyBranch2`]/[`BuddyBranch2Layout`] makes type
//! checking much faster.

use serde::Deserialize;
use serde::Serialize;

use super::Buddy;
use super::BuddyLayout;
use super::branch::BuddyBranch2;
use super::branch::BuddyBranch2Layout;
use crate::state::NewState;
use crate::state_backend::AllocatedOf;
use crate::state_backend::CommitmentLayout;
use crate::state_backend::FnManager;
use crate::state_backend::FromProofResult;
use crate::state_backend::Layout;
use crate::state_backend::ManagerAlloc;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerClone;
use crate::state_backend::ManagerDeserialise;
use crate::state_backend::ManagerRead;
use crate::state_backend::ManagerReadWrite;
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

/// Generate a new combined Buddy branch.
macro_rules! combined_buddy_branch {
    ($name:ident = $buddy1:ident * $buddy2:ident) => {
        paste::paste! {
            /// Allocated combined Buddy branch
            pub struct [<$name Alloc>]<B: Layout, M: ManagerBase>(AllocatedOf<[<$buddy1 Layout>]<[<$buddy2 Layout>]<B>>, M>);

            // Passthrough implementation, default derive macro can't derive this ...
            impl<B, M> PartialEq for [<$name Alloc>]<B, M>
            where
                B: Layout,
                M: ManagerSerialise,
                AllocatedOf<[<$buddy1 Layout>]<[<$buddy2 Layout>]<B>>, M>: PartialEq,
            {
                fn eq(&self, other: &Self) -> bool {
                    self.0.eq(&other.0)
                }
            }

            // Passthrough implementation, default derive macro can't derive this ...
            impl<B, M> Serialize for [<$name Alloc>]<B, M>
            where
                B: Layout,
                M: ManagerSerialise,
                AllocatedOf<[<$buddy1 Layout>]<[<$buddy2 Layout>]<B>>, M>: Serialize,
            {
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where
                    S: serde::Serializer,
                {
                    self.0.serialize(serializer)
                }
            }


            // Passthrough implementation, default derive macro can't derive this ...
            impl<'de, B, M> Deserialize<'de> for [<$name Alloc>]<B, M>
            where
                B: Layout,
                M: ManagerDeserialise,
                AllocatedOf<[<$buddy1 Layout>]<[<$buddy2 Layout>]<B>>, M>: Deserialize<'de>,
            {
                fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
                where
                    D: serde::Deserializer<'de>,
                {
                    Ok(Self(Deserialize::deserialize(deserializer)?))
                }
            }

            // NOTE: We can't use `struct_layout!` to help us define the type and impls below. The
            // macro doesn't define new type aliases for the combined layouts which results in a
            // massive type tree to traverse. This makes type checking super slow.

            /// Layout for a combined Buddy branch
            pub struct [<$name Layout>]<B>(B);

            impl<B: Layout> Layout for [<$name Layout>]<B> {
                type Allocated<M: ManagerBase> = [<$name Alloc>]<B, M>;

            }

            impl<B: CommitmentLayout> CommitmentLayout for [<$name Layout>]<B> {
                fn state_hash<M: ManagerSerialise>(state: AllocatedOf<Self, M>) -> Result<Hash, HashError> {
                    <[<$buddy1 Layout>]<[<$buddy2 Layout>]<B>>>::state_hash(state.0)
                }
            }

            impl<B: ProofLayout> ProofLayout for [<$name Layout>]<B> {
                fn to_merkle_tree(state: RefProofGenOwnedAlloc<Self>) -> Result<MerkleTree, HashError> {
                    <[<$buddy1 Layout>]<[<$buddy2 Layout>]<B>>>::to_merkle_tree(state.0)
                }

                fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
                    let inner = <[<$buddy1 Layout>]<[<$buddy2 Layout>]<B>>>::from_proof(proof)?;
                    Ok([<$name Alloc>](inner))
                }

                fn partial_state_hash(
                    state: RefVerifierAlloc<Self>,
                    proof: ProofTree,
                ) -> Result<Hash, PartialHashError> {
                    <[<$buddy1 Layout>]<[<$buddy2 Layout>]<B>>>::partial_state_hash(state.0, proof)
                }
            }

            impl<B: BuddyLayout> BuddyLayout for [<$name Layout>]<B> {
                type Buddy<M: ManagerBase> = $name<B::Buddy<M>, M>;

                fn bind<M: ManagerBase>(space: Self::Allocated<M>) -> Self::Buddy<M> {
                    let inner = <[<$buddy1 Layout>]<[<$buddy2 Layout>]<B>> as BuddyLayout>::bind(space.0);
                    $name(inner)
                }

                fn struct_ref<'a, F, M: ManagerBase>(space: &'a Self::Buddy<M>) -> Self::Allocated<F::Output>
                where
                    F: FnManager<Ref<'a, M>>,
                {
                    let inner = <[<$buddy1 Layout>]<[<$buddy2 Layout>]<B>> as BuddyLayout>::struct_ref::<F, M>(&space.0);
                    [<$name Alloc>](inner)
                }
            }
        }

        /// Combined Buddy branch
        pub struct $name<B, M: ManagerBase>($buddy1<$buddy2<B, M>, M>);

        // Passthrough implementation, default derive macro can't derive this ...
        impl<B: Serialize, M: ManagerSerialise> Serialize for $name<B, M> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                Serialize::serialize(&self.0, serializer)
            }
        }

        // Passthrough implementation, default derive macro can't derive this ...
        impl<'de, B: Deserialize<'de>, M: ManagerDeserialise> Deserialize<'de> for $name<B, M> {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                Ok(Self(Deserialize::deserialize(deserializer)?))
            }
        }

        // Passthrough implementation, default derive macro can't derive this ...
        impl<B, M: ManagerRead> PartialEq for $name<B, M>
        where
            B: PartialEq,
        {
            fn eq(&self, other: &Self) -> bool {
                self.0.eq(&other.0)
            }
        }

        impl<B, M> NewState<M> for $name<B, M>
        where
            B: Buddy<M>,
            M: ManagerBase,
        {
            fn new(manager: &mut M) -> Self
            where
                M: ManagerAlloc,
            {
                Self(NewState::new(manager))
            }
        }

        impl<B, M> Buddy<M> for $name<B, M>
        where
            B: Buddy<M>,
            M: ManagerBase,
        {
            const PAGES: u64 = <$buddy1<$buddy2<B, M>, M> as Buddy<M>>::PAGES;

            fn allocate(&mut self, pages: u64) -> Option<u64>
            where
                M: ManagerReadWrite,
            {
                self.0.allocate(pages)
            }

            fn allocate_fixed(&mut self, idx: u64, pages: u64, replace: bool) -> Option<()>
            where
                M: ManagerReadWrite,
            {
                self.0.allocate_fixed(idx, pages, replace)
            }

            fn deallocate(&mut self, idx: u64, pages: u64)
            where
                M: ManagerReadWrite,
            {
                self.0.deallocate(idx, pages)
            }

            fn longest_free_sequence(&self) -> u64
            where
                M: ManagerRead,
            {
                self.0.longest_free_sequence()
            }

            fn count_free_start(&self) -> u64
            where
                M: ManagerRead,
            {
                self.0.count_free_start()
            }

            fn count_free_end(&self) -> u64
            where
                M: ManagerRead,
            {
                self.0.count_free_end()
            }

            fn clone(&self) -> Self
            where
                M: ManagerClone,
            {
                $name(self.0.clone())
            }
        }
    };
}

combined_buddy_branch!(BuddyBranch4 = BuddyBranch2 * BuddyBranch2);
combined_buddy_branch!(BuddyBranch8 = BuddyBranch4 * BuddyBranch2);
combined_buddy_branch!(BuddyBranch16 = BuddyBranch4 * BuddyBranch4);
combined_buddy_branch!(BuddyBranch32 = BuddyBranch4 * BuddyBranch8);
combined_buddy_branch!(BuddyBranch64 = BuddyBranch8 * BuddyBranch8);
combined_buddy_branch!(BuddyBranch128 = BuddyBranch16 * BuddyBranch8);
combined_buddy_branch!(BuddyBranch256 = BuddyBranch16 * BuddyBranch16);
combined_buddy_branch!(BuddyBranch1Ki = BuddyBranch32 * BuddyBranch32);
combined_buddy_branch!(BuddyBranch256Ki = BuddyBranch256 * BuddyBranch1Ki);

// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Leaf of a tree that forms a Buddy-style memory manager

use serde::Deserialize;
use serde::Serialize;

use super::Buddy;
use super::BuddyLayout;
use crate::bits::ones;
use crate::state::NewState;
use crate::state_backend::AllocatedOf;
use crate::state_backend::Atom;
use crate::state_backend::Cell;
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

/// Layout for a leaf of a tree that forms a Buddy-style memory manager
pub struct BuddyLeafLayout<const PAGES: u64>;

impl<const PAGES: u64> Layout for BuddyLeafLayout<PAGES> {
    type Allocated<M: ManagerBase> = BuddyLeaf<PAGES, M>;
}

impl<const PAGES: u64> CommitmentLayout for BuddyLeafLayout<PAGES> {
    fn state_hash<M: ManagerSerialise>(state: AllocatedOf<Self, M>) -> Result<Hash, HashError> {
        Atom::state_hash(state.set)
    }
}

impl<const PAGES: u64> ProofLayout for BuddyLeafLayout<PAGES> {
    fn to_merkle_tree(state: RefProofGenOwnedAlloc<Self>) -> Result<MerkleTree, HashError> {
        Atom::to_merkle_tree(state.set)
    }

    fn from_proof(proof: ProofTree) -> FromProofResult<Self> {
        Ok(Self::Allocated {
            set: Atom::from_proof(proof)?,
        })
    }

    fn partial_state_hash(
        state: RefVerifierAlloc<Self>,
        proof: ProofTree,
    ) -> Result<Hash, PartialHashError> {
        Atom::partial_state_hash(state.set, proof)
    }
}

impl<const PAGES: u64> BuddyLayout for BuddyLeafLayout<PAGES> {
    type Buddy<M: ManagerBase> = BuddyLeaf<PAGES, M>;

    fn bind<M: ManagerBase>(space: Self::Allocated<M>) -> Self::Buddy<M> {
        space
    }

    fn struct_ref<'a, F, M: ManagerBase>(space: &'a Self::Buddy<M>) -> Self::Allocated<F::Output>
    where
        F: FnManager<Ref<'a, M>>,
    {
        BuddyLeaf {
            set: space.set.struct_ref::<F>(),
        }
    }
}

/// Leaf of a tree that forms a Buddy-style memory manager
pub struct BuddyLeaf<const PAGES: u64, M: ManagerBase> {
    /// Each bit of the `u64` represents a page.
    /// The least significant bit is the page with index 0.
    set: Cell<u64, M>,
}

impl<const PAGES: u64, M: ManagerBase> NewState<M> for BuddyLeaf<PAGES, M> {
    fn new(manager: &mut M) -> Self
    where
        M: ManagerAlloc,
    {
        Self {
            set: Cell::new(manager),
        }
    }
}

impl<const PAGES: u64, M: ManagerBase> Buddy<M> for BuddyLeaf<PAGES, M> {
    const PAGES: u64 = PAGES;

    fn allocate(&mut self, pages: u64) -> Option<u64>
    where
        M: ManagerReadWrite,
    {
        if pages == 0 || pages > Self::PAGES {
            return None;
        }

        let set = self.set.read();

        for start in 0..=(Self::PAGES - pages) {
            let mask = ones(pages) << start;

            // Since the mask projects only the bits representing the current page range, none of
            // the bits may be set. If they are, then there is an existing overlapping allocation
            // in place already.
            if (set & mask) == 0 {
                self.set.write(set | mask);
                return Some(start);
            }
        }

        None
    }

    fn allocate_fixed(&mut self, idx: u64, pages: u64, replace: bool) -> Option<()>
    where
        M: ManagerReadWrite,
    {
        if pages == 0 || pages > Self::PAGES.saturating_sub(idx) {
            return None;
        }

        // Shortcut to avoid state reads
        if idx == 0 && pages == Self::PAGES && replace {
            self.set.write(!0);
            return Some(());
        }

        // Sequence of `pages` 1s starting at bit `idx`
        let mask = ones(pages) << idx;

        let set = self.set.read();

        if !replace && (set & mask) != 0 {
            // If none of the bits representing the to-be-mapped pages are set, then
            // `already_mapped` should be 0 after applying the mask
            return None;
        }

        self.set.write(set | mask);

        Some(())
    }

    fn deallocate(&mut self, idx: u64, pages: u64)
    where
        M: ManagerReadWrite,
    {
        if pages == 0 || pages > Self::PAGES.saturating_sub(idx) {
            return;
        }

        // Shortcut to avoid state reads
        if idx == 0 && pages == Self::PAGES {
            self.set.write(0);
            return;
        }

        // Sequence of `pages` 1s starting at bit `idx`
        let mask = ones(pages) << idx;

        // Clear the bits representing the page range
        let set = self.set.read();
        self.set.write(set & !mask);
    }

    fn longest_free_sequence(&self) -> u64
    where
        M: ManagerRead,
    {
        let set = self.set.read();

        if set == 0 {
            return Self::PAGES;
        }

        // Find the longest sequence of 0s
        (0..Self::PAGES).fold(0, |longest_seq, shift| {
            let free_max_pages = Self::PAGES - shift;
            let free_end = (set >> shift).trailing_zeros() as u64;
            free_end.min(free_max_pages).max(longest_seq)
        })
    }

    fn count_free_start(&self) -> u64
    where
        M: ManagerRead,
    {
        Self::PAGES.min(self.set.read().trailing_zeros() as u64)
    }

    fn count_free_end(&self) -> u64
    where
        M: ManagerRead,
    {
        let leading_unused_bits = (u64::BITS as u64)
            .checked_sub(Self::PAGES)
            .expect("PAGES must not be larger than 64");
        (self.set.read().leading_zeros() as u64).saturating_sub(leading_unused_bits)
    }

    fn clone(&self) -> Self
    where
        M: ManagerClone,
    {
        Self {
            set: self.set.clone(),
        }
    }
}

impl<const PAGES: u64, M: ManagerSerialise> Serialize for BuddyLeaf<PAGES, M> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.set.serialize(serializer)
    }
}

impl<'de, const PAGES: u64, M: ManagerDeserialise> Deserialize<'de> for BuddyLeaf<PAGES, M> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Self {
            set: Deserialize::deserialize(deserializer)?,
        })
    }
}

impl<const PAGES: u64, M: ManagerRead> PartialEq for BuddyLeaf<PAGES, M> {
    fn eq(&self, other: &Self) -> bool {
        self.set.eq(&other.set)
    }
}

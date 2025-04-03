// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Buddy-style memory manager
//!
//! Memory managers let you allocate and deallocate memory so it can be re-used later. It removes
//! the need to bookkeep memory allocations from the user, allowing them to only focus on "how much"
//! memory they need.
//!
//! A Buddy allocator employs a tree to represent the allocations across the entire memory range.
//! This allows it to have logarithmic time complexity for finding allocations. This sub-linear
//! complexity is achieved by splitting the memory range into halves, and recursively splitting.
//! Each branch of the tree manages the two halves of its memory range. This splitting stops once we
//! go below a certain memory size for a branch. In this case the branch becomes a leaf which is
//! represented by a bit array where each bit represents the occupancy state of a memory page.
//!
//! See: <https://en.wikipedia.org/wiki/Buddy_memory_allocation>

mod branch;
mod branch_combinations;
mod leaf;
mod proxy;

pub use proxy::BuddyLayoutProxy;

use crate::state::NewState;
use crate::state_backend::CommitmentLayout;
use crate::state_backend::FnManager;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerClone;
use crate::state_backend::ManagerRead;
use crate::state_backend::ManagerReadWrite;
use crate::state_backend::ProofLayout;
use crate::state_backend::Ref;

/// Layout for a Buddy-style memory manager
pub trait BuddyLayout: CommitmentLayout + ProofLayout {
    /// Buddy-style memory manager implementation
    type Buddy<M: ManagerBase>: Buddy<M>;

    /// Bind the allocated space.
    fn bind<M: ManagerBase>(space: Self::Allocated<M>) -> Self::Buddy<M>;

    /// Given a manager morphism `F : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    fn struct_ref<'a, F, M: ManagerBase>(space: &'a Self::Buddy<M>) -> Self::Allocated<F::Output>
    where
        F: FnManager<Ref<'a, M>>;
}

/// Buddy-style memory manager
pub trait Buddy<M: ManagerBase>: NewState<M> {
    /// Number of pages being managed
    const PAGES: u64;

    /// Allocate a number of pages. Returns the index of the first page in the allocated range.
    fn allocate(&mut self, pages: u64) -> Option<u64>
    where
        M: ManagerReadWrite;

    /// Allocate a fixed range of pages. If `replace` is `true`, the range is allocated even if it
    /// overlaps an existing allocation.
    fn allocate_fixed(&mut self, idx: u64, pages: u64, replace: bool) -> Option<()>
    where
        M: ManagerReadWrite;

    /// Deallocate a range of pages.
    fn deallocate(&mut self, idx: u64, pages: u64)
    where
        M: ManagerReadWrite;

    /// Count the largest sequence of free pages.
    fn longest_free_sequence(&self) -> u64
    where
        M: ManagerRead;

    /// Count the number of free pages at the start of the managed area.
    fn count_free_start(&self) -> u64
    where
        M: ManagerRead;

    /// Count the number of free pages at the end of the managed area.
    fn count_free_end(&self) -> u64
    where
        M: ManagerRead;

    /// Clone the memory manager state.
    fn clone(&self) -> Self
    where
        M: ManagerClone;
}

#[cfg(test)]
mod tests {
    use rand::Rng;
    use rand::seq::SliceRandom;

    use super::*;
    use crate::backend_test;

    fn distribute(mut total: u64, min: u64) -> Vec<u64> {
        let mut rng = rand::thread_rng();
        let mut steps: Vec<u64> = std::iter::from_fn(|| {
            if total == 0 {
                return None;
            }

            let steps = total.div_euclid(2).max(min + 1);
            let steps = rng.gen_range(min..=steps).min(total);

            total = total.saturating_sub(steps);

            Some(steps)
        })
        .collect();

        steps.shuffle(&mut rng);
        steps
    }

    backend_test!(buddy_alloc_only, F, {
        type BuddyHeapLayout = BuddyLayoutProxy<{ 1024 * 1024 }>;

        let mut manager = F::manager();
        let mut state = <BuddyHeapLayout as BuddyLayout>::Buddy::new(&mut manager);

        let total_pages = state.longest_free_sequence();

        for _ in 0..100 {
            // Create a distribution of allocation sizes that when used together would allocate all
            // available memory
            let allocations = distribute(total_pages, 1);

            let mut expected_idx = 0;
            for length in allocations {
                let idx = state.allocate(length);
                assert_eq!(idx, Some(expected_idx));
                expected_idx += length;
            }

            assert_eq!(state.allocate(1), None);
            state.deallocate(0, total_pages);
        }
    });

    backend_test!(buddy_alloc_dealloc, F, {
        type BuddyHeapLayout = BuddyLayoutProxy<{ 1024 * 1024 }>;

        let mut manager = F::manager();
        let mut state = <BuddyHeapLayout as BuddyLayout>::Buddy::new(&mut manager);

        let total_pages = state.longest_free_sequence();

        let mut rng = rand::thread_rng();
        for _ in 0..100 {
            // Create a distribution of allocation sizes that when used together would allocate all
            // available memory
            let allocations = distribute(total_pages, 1);

            let mut occupied = Vec::new();

            for length in allocations {
                // When we accumulated at least 3 allocations, we randomly free one of them. This
                // is to cause fragmentation and test the allocator's ability to reuse freed memory.
                if occupied.len() >= 3 {
                    let index = rng.gen_range(0..occupied.len());
                    let (idx, length) = occupied.remove(index);
                    state.deallocate(idx, length);
                }

                let idx = state.allocate(length).unwrap();
                occupied.push((idx, length));
            }

            for (idx, length) in occupied {
                state.deallocate(idx, length);
            }
        }
    });

    backend_test!(buddy_alloc_fixed, F, {
        type BuddyHeapLayout = BuddyLayoutProxy<{ 1024 * 1024 }>;

        let mut manager = F::manager();
        let mut state = <BuddyHeapLayout as BuddyLayout>::Buddy::new(&mut manager);

        // Create a distribution of allocation sizes that when used together would allocate all
        // available memory
        let allocations = distribute(state.longest_free_sequence(), 1);

        for length in allocations {
            let idx = state.allocate(length).unwrap();

            // Re-allocating the same range should fail
            assert!(state.allocate_fixed(idx, length, false).is_none());

            // Overriding the range allocation must succeed
            state.allocate_fixed(idx, length, true).unwrap();

            // After freeing the range, it should be possible to allocate it again
            state.deallocate(idx, length);
            state.allocate_fixed(idx, length, false).unwrap();
        }
    });
}

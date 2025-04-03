// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Branch of a tree that forms a Buddy-style memory manager

use serde::Deserialize;
use serde::Serialize;

use super::Buddy;
use super::BuddyLayout;
use crate::state::NewState;
use crate::state_backend::Atom;
use crate::state_backend::Cell;
use crate::state_backend::FnManager;
use crate::state_backend::ManagerAlloc;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerClone;
use crate::state_backend::ManagerDeserialise;
use crate::state_backend::ManagerRead;
use crate::state_backend::ManagerReadWrite;
use crate::state_backend::ManagerSerialise;
use crate::state_backend::Ref;
use crate::struct_layout;

/// Information about what is free in each buddy
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FreeInfo {
    /// Length of the longest sequence of free pages in the left buddy
    left_longest_free_sequence: u64,

    /// Number of free pages at the start of the left buddy
    left_free_start: u64,

    /// Number of free pages at the end of the left buddy
    left_free_end: u64,

    /// Length of the longest sequence of free pages in the right buddy
    right_longest_free_sequence: u64,

    /// Number of free pages at the start of the right buddy
    right_free_start: u64,

    /// Number of free pages at the end of the right buddy
    right_free_end: u64,
}

struct_layout! {
    pub struct BuddyBranch2Layout<B> {
        free_info: Atom<FreeInfo>,
        left: Box<B>,
        right: Box<B>,
    }
}

impl<B: BuddyLayout> BuddyLayout for BuddyBranch2Layout<B> {
    type Buddy<M: ManagerBase> = BuddyBranch2<B::Buddy<M>, M>;

    fn bind<M: ManagerBase>(space: Self::Allocated<M>) -> Self::Buddy<M> {
        BuddyBranch2 {
            free_info: space.free_info,
            left: Box::new(B::bind(*space.left)),
            right: Box::new(B::bind(*space.right)),
        }
    }

    fn struct_ref<'a, F, M: ManagerBase>(space: &'a Self::Buddy<M>) -> Self::Allocated<F::Output>
    where
        F: FnManager<Ref<'a, M>>,
    {
        BuddyBranch2LayoutF {
            free_info: space.free_info.struct_ref::<F>(),
            left: Box::new(B::struct_ref::<F, M>(&space.left)),
            right: Box::new(B::struct_ref::<F, M>(&space.right)),
        }
    }
}

/// Branch in a Buddy-style memory manager tree
pub struct BuddyBranch2<B, M: ManagerBase> {
    free_info: Cell<FreeInfo, M>,
    left: Box<B>,
    right: Box<B>,
}

impl<B: Buddy<M>, M: ManagerBase> BuddyBranch2<B, M> {
    fn refresh(&mut self)
    where
        M: ManagerReadWrite,
    {
        self.free_info.write(FreeInfo {
            left_longest_free_sequence: self.left.longest_free_sequence(),
            left_free_start: self.left.count_free_start(),
            left_free_end: self.left.count_free_end(),
            right_longest_free_sequence: self.right.longest_free_sequence(),
            right_free_start: self.right.count_free_start(),
            right_free_end: self.right.count_free_end(),
        });
    }
}

impl<B, M> NewState<M> for BuddyBranch2<B, M>
where
    B: Buddy<M>,
    M: ManagerBase,
{
    fn new(manager: &mut M) -> Self
    where
        M: ManagerAlloc,
    {
        Self {
            free_info: Cell::new_with(manager, FreeInfo {
                left_longest_free_sequence: B::PAGES,
                left_free_start: B::PAGES,
                left_free_end: B::PAGES,
                right_longest_free_sequence: B::PAGES,
                right_free_start: B::PAGES,
                right_free_end: B::PAGES,
            }),
            left: Box::new(B::new(manager)),
            right: Box::new(B::new(manager)),
        }
    }
}

impl<B, M> Buddy<M> for BuddyBranch2<B, M>
where
    B: Buddy<M>,
    M: ManagerBase,
{
    const PAGES: u64 = B::PAGES * 2;

    fn allocate(&mut self, pages: u64) -> Option<u64>
    where
        M: ManagerReadWrite,
    {
        if !(1..=Self::PAGES).contains(&pages) {
            return None;
        }

        // The requested range can be allocated entirely in the left buddy
        if pages <= self.free_info.left_longest_free_sequence {
            let idx = self.left.allocate(pages)?;
            self.refresh();
            return Some(idx);
        }

        let left_free_end = self.free_info.left_free_end;
        let right_free_start = self.free_info.right_free_start;
        let overlap = left_free_end.saturating_add(right_free_start);

        // There might be space that covers the end of the left buddy and the start of the right
        // buddy. We may use this space as it is continuous.
        if pages <= overlap && left_free_end > 0 {
            let idx = B::PAGES.checked_sub(left_free_end)?;
            self.left.allocate_fixed(idx, left_free_end, true)?;

            let right_pages = pages.saturating_sub(left_free_end);

            // Allocate the right buddy. Be aware, if that fails, we need to back out of the left
            // buddy allocation.
            if right_pages > 0 && self.right.allocate_fixed(0, right_pages, true).is_none() {
                self.left.deallocate(idx, left_free_end);
                return None;
            }

            self.refresh();
            return Some(idx);
        }

        // Try allocating in the right buddy if there's enough space
        if pages <= self.free_info.right_longest_free_sequence {
            let idx = self.right.allocate(pages)?;
            self.refresh();

            // Return adjusted index to reflect position in the combined buddy
            return Some(idx + B::PAGES);
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

        let left_pages = B::PAGES.saturating_sub(idx).min(pages);
        let right_pages = pages.saturating_sub(left_pages);

        // The range covers the left buddy
        if left_pages > 0 {
            self.left.allocate_fixed(idx, left_pages, replace)?;
        }

        // The range covers the right buddy
        if right_pages > 0 {
            let right_idx = idx.saturating_sub(B::PAGES);
            let right_res = self.right.allocate_fixed(right_idx, right_pages, replace);

            // If the right allocation failed, we might need to do some clean up on the left buddy
            if right_res.is_none() {
                if left_pages > 0 {
                    self.left.deallocate(idx, left_pages);
                }

                return None;
            }
        }

        // Need to refresh the free counters
        self.refresh();

        Some(())
    }

    fn deallocate(&mut self, idx: u64, mut pages: u64)
    where
        M: ManagerReadWrite,
    {
        // Defer to only the right buddy if the range does not cover the left side
        if idx >= B::PAGES {
            self.right.deallocate(idx - B::PAGES, pages);
        } else {
            let end = pages.saturating_add(idx);

            // If the range crosses from left to right buddy
            if end > B::PAGES {
                let overhang = end.saturating_sub(B::PAGES);
                self.right.deallocate(0, overhang);
                pages = pages.saturating_sub(overhang);
            }

            self.left.deallocate(idx, pages);
        }

        // Need to refresh the free counters
        self.refresh();
    }

    fn longest_free_sequence(&self) -> u64
    where
        M: ManagerRead,
    {
        self.free_info
            .left_free_end
            .saturating_add(self.free_info.right_free_start)
            .max(self.free_info.left_longest_free_sequence)
            .max(self.free_info.right_longest_free_sequence)
    }

    fn count_free_start(&self) -> u64
    where
        M: ManagerRead,
    {
        let free_start = self.free_info.left_free_start;

        if free_start < B::PAGES {
            return free_start;
        }

        self.free_info.right_free_start.saturating_add(B::PAGES)
    }

    fn count_free_end(&self) -> u64
    where
        M: ManagerRead,
    {
        let free_end = self.free_info.right_free_end;

        if free_end < B::PAGES {
            return free_end;
        }

        self.free_info.left_free_end.saturating_add(B::PAGES)
    }

    fn clone(&self) -> Self
    where
        M: ManagerClone,
    {
        Self {
            free_info: self.free_info.clone(),
            left: Box::new(self.left.clone()),
            right: Box::new(self.right.clone()),
        }
    }
}

impl<B: Serialize, M: ManagerSerialise> Serialize for BuddyBranch2<B, M> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        BuddyBranch2LayoutF {
            free_info: &self.free_info,
            left: &self.left,
            right: &self.right,
        }
        .serialize(serializer)
    }
}

impl<'de, B: Deserialize<'de>, M: ManagerDeserialise> Deserialize<'de> for BuddyBranch2<B, M> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let inner: BuddyBranch2LayoutF<_, _, _> = Deserialize::deserialize(deserializer)?;
        Ok(Self {
            free_info: inner.free_info,
            left: inner.left,
            right: inner.right,
        })
    }
}

impl<B: PartialEq, M: ManagerRead> PartialEq for BuddyBranch2<B, M> {
    fn eq(&self, other: &Self) -> bool {
        self.free_info.eq(&other.free_info)
            && self.left.eq(&other.left)
            && self.right.eq(&other.right)
    }
}

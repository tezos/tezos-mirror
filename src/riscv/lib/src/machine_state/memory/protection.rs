// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::Address;
use crate::array_utils::boxed_from_fn;
use crate::state::NewState;
use crate::state_backend::AllocatedOf;
use crate::state_backend::Atom;
use crate::state_backend::Cell;
use crate::state_backend::FnManager;
use crate::state_backend::ManagerAlloc;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerClone;
use crate::state_backend::ManagerDeserialise;
use crate::state_backend::ManagerRead;
use crate::state_backend::ManagerSerialise;
use crate::state_backend::ManagerWrite;
use crate::state_backend::Many;
use crate::state_backend::Ref;

/// State layout for page permissions
pub type PagePermissionsLayout<const PAGES: usize> = Many<Atom<bool>, PAGES>;

/// Tracks access permissions for each page
pub struct PagePermissions<const PAGES: usize, M: ManagerBase> {
    pages: Box<[Cell<bool, M>; PAGES]>,
}

impl<const PAGES: usize, M: ManagerBase> PagePermissions<PAGES, M> {
    /// Bind the given allocated space as a page protections state value.
    pub fn bind(space: AllocatedOf<PagePermissionsLayout<PAGES>, M>) -> Self {
        Self {
            pages: space.try_into().unwrap_or_else(|_| {
                unreachable!("Converting a vector into an array of the same length always succeeds")
            }),
        }
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(
        &'a self,
    ) -> AllocatedOf<PagePermissionsLayout<PAGES>, F::Output> {
        self.pages
            .iter()
            .map(|item| item.struct_ref::<F>())
            .collect()
    }

    /// Check if the memory at `address..address+length` can be accessed.
    ///
    /// # Safety
    ///
    /// The address and length must be valid for an address space consisting of a number of `PAGES`.
    /// This function is not defined for address and length combinations which are out of bounds.
    #[inline]
    pub unsafe fn can_access(&self, address: Address, length: usize) -> bool
    where
        M: ManagerRead,
    {
        // Zero-sized accesses are always valid
        if length < 1 {
            return true;
        }

        let address = address as usize;

        // Extract the page index range from using the start and end addresses
        let start_page = address >> super::OFFSET_BITS;
        let end_page = address.wrapping_add(length).wrapping_sub(1) >> super::OFFSET_BITS;

        for page in start_page..=end_page {
            if unsafe { !self.pages.get_unchecked(page).read() } {
                return false;
            }
        }

        true
    }

    /// Change the access permissions for the given range.
    pub fn modify_access(&mut self, address: Address, length: usize, accessible: bool)
    where
        M: ManagerWrite,
    {
        if length < 1 {
            return;
        }

        let address = address as usize;
        let start_page = address >> super::OFFSET_BITS;
        let end_page = address.wrapping_add(length).wrapping_sub(1) >> super::OFFSET_BITS;

        (start_page..=end_page)
            .filter(|&page| page < PAGES)
            .for_each(|page| {
                self.pages[page].write(accessible);
            })
    }
}

impl<const PAGES: usize, M: ManagerBase> NewState<M> for PagePermissions<PAGES, M> {
    fn new(manager: &mut M) -> Self
    where
        M: ManagerAlloc,
    {
        PagePermissions {
            pages: boxed_from_fn(|| Cell::new(manager)),
        }
    }
}

impl<'de, const PAGES: usize, M: ManagerDeserialise> serde::Deserialize<'de>
    for PagePermissions<PAGES, M>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Self::bind(
            <AllocatedOf<PagePermissionsLayout<PAGES>, M>>::deserialize(deserializer)?,
        ))
    }
}

impl<const PAGES: usize, M: ManagerSerialise> serde::Serialize for PagePermissions<PAGES, M> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.pages.serialize(serializer)
    }
}

impl<const PAGES: usize, M: ManagerClone> Clone for PagePermissions<PAGES, M> {
    fn clone(&self) -> Self {
        Self {
            pages: self.pages.clone(),
        }
    }
}

// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::buddy::BuddyLayout;
use super::buddy::BuddyLayoutProxy;
use super::protection::PagePermissions;
use super::protection::PagePermissionsLayout;
use super::state::MemoryImpl;
use crate::state::NewState;
use crate::state_backend::AllocatedOf;
use crate::state_backend::DynArray;
use crate::state_backend::DynCells;
use crate::state_backend::FnManager;
use crate::state_backend::ManagerAlloc;
use crate::state_backend::ManagerBase;
use crate::state_backend::Ref;

/// State layout for the memory component
pub struct MemoryConfig<const PAGES: usize, const TOTAL_BYTES: usize>;

impl<const PAGES: usize, const TOTAL_BYTES: usize, B, M> NewState<M>
    for MemoryImpl<PAGES, TOTAL_BYTES, B, M>
where
    B: NewState<M>,
    M: ManagerBase,
{
    fn new(manager: &mut M) -> Self
    where
        M: ManagerAlloc,
    {
        MemoryImpl {
            data: DynCells::new(manager),
            readable_pages: PagePermissions::new(manager),
            writable_pages: PagePermissions::new(manager),
            executable_pages: PagePermissions::new(manager),
            allocated_pages: B::new(manager),
        }
    }
}

impl<const PAGES: usize, const TOTAL_BYTES: usize> super::MemoryConfig
    for MemoryConfig<PAGES, TOTAL_BYTES>
where
    BuddyLayoutProxy<PAGES>: BuddyLayout + 'static,
{
    const TOTAL_BYTES: usize = TOTAL_BYTES;

    type Layout = (
        DynArray<TOTAL_BYTES>,
        PagePermissionsLayout<PAGES>,
        PagePermissionsLayout<PAGES>,
        PagePermissionsLayout<PAGES>,
        BuddyLayoutProxy<PAGES>,
    );

    type State<M: ManagerBase> =
        MemoryImpl<PAGES, TOTAL_BYTES, <BuddyLayoutProxy<PAGES> as BuddyLayout>::Buddy<M>, M>;

    fn bind<M: ManagerBase>(space: AllocatedOf<Self::Layout, M>) -> Self::State<M> {
        if TOTAL_BYTES == 0 {
            panic!("Memory size must be positive");
        }

        if PAGES.checked_mul(super::PAGE_SIZE.get() as usize) != Some(TOTAL_BYTES) {
            panic!(
                "Memory size {} must be a non-overflowing multiple of the page size {}",
                TOTAL_BYTES,
                super::PAGE_SIZE
            );
        }

        MemoryImpl {
            data: space.0,
            readable_pages: PagePermissions::bind(space.1),
            writable_pages: PagePermissions::bind(space.2),
            executable_pages: PagePermissions::bind(space.3),
            allocated_pages: <BuddyLayoutProxy<PAGES> as BuddyLayout>::bind(space.4),
        }
    }

    fn struct_ref<'a, M, F>(instance: &'a Self::State<M>) -> AllocatedOf<Self::Layout, F::Output>
    where
        M: ManagerBase,
        F: FnManager<Ref<'a, M>>,
    {
        (
            instance.data.struct_ref::<F>(),
            instance.readable_pages.struct_ref::<F>(),
            instance.writable_pages.struct_ref::<F>(),
            instance.executable_pages.struct_ref::<F>(),
            <BuddyLayoutProxy<PAGES> as BuddyLayout>::struct_ref::<F, M>(&instance.allocated_pages),
        )
    }
}

/// Generates a valid memory configuration.
macro_rules! gen_memory_layout {
    ($name:ident = $size_in_g:literal GiB) => {
        pub type $name =
            MemoryConfig<{ $size_in_g * 1024 * 256 }, { $size_in_g * 1024 * 1024 * 1024 }>;
    };

    ($name:ident = $size_in_m:literal MiB) => {
        pub type $name = MemoryConfig<{ $size_in_m * 256 }, { $size_in_m * 1024 * 1024 }>;
    };

    ($name:ident = $size_in_k:literal KiB) => {
        pub type $name = MemoryConfig<{ $size_in_k / 4 }, { $size_in_k * 1024 }>;
    };
}

gen_memory_layout!(M4K = 4 KiB);
gen_memory_layout!(M8K = 8 KiB);
gen_memory_layout!(M1M = 1 MiB);
gen_memory_layout!(M64M = 64 MiB);
gen_memory_layout!(M1G = 1 GiB);
gen_memory_layout!(M4G = 4 GiB);

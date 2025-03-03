// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::state::MemoryImpl;
use crate::state_backend::{AllocatedOf, DynArray, FnManager, ManagerBase, Ref};

/// State layout for the memory component
pub struct MemoryConfig<const TOTAL_BYTES: usize>(DynArray<TOTAL_BYTES>);

impl<const TOTAL_BYTES: usize> super::MemoryConfig for MemoryConfig<TOTAL_BYTES> {
    const TOTAL_BYTES: usize = TOTAL_BYTES;

    type Layout = DynArray<TOTAL_BYTES>;

    type State<M: ManagerBase> = MemoryImpl<TOTAL_BYTES, M>;

    fn bind<M: ManagerBase>(space: AllocatedOf<Self::Layout, M>) -> Self::State<M> {
        if TOTAL_BYTES == 0 {
            panic!("Memory size must be positive");
        }

        MemoryImpl { data: space }
    }

    fn struct_ref<'a, M, F>(instance: &'a Self::State<M>) -> AllocatedOf<Self::Layout, F::Output>
    where
        M: ManagerBase,
        F: FnManager<Ref<'a, M>>,
    {
        instance.data.struct_ref::<F>()
    }
}

/// Generates a valid memory configuration.
macro_rules! gen_memory_layout {
    ($name:ident = $size_in_g:literal GiB) => {
        pub type $name = MemoryConfig<{ $size_in_g * 1024 * 1024 * 1024 }>;
    };

    ($name:ident = $size_in_m:literal MiB) => {
        pub type $name = MemoryConfig<{ $size_in_m * 1024 * 1024 }>;
    };

    ($name:ident = $size_in_k:literal KiB) => {
        pub type $name = MemoryConfig<{ $size_in_k * 1024 }>;
    };
}

gen_memory_layout!(M1K = 1 KiB);
gen_memory_layout!(M8K = 8 KiB);
gen_memory_layout!(M1M = 1 MiB);
gen_memory_layout!(M64M = 64 MiB);
gen_memory_layout!(M1G = 1 GiB);
gen_memory_layout!(M4G = 4 GiB);

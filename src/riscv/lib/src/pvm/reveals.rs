// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::state_backend::{
    AllocatedOf, Atom, Cell, DynArray, DynCells, FnManager, ManagerBase, ManagerClone, ManagerRead,
    Ref,
};
use tezos_smart_rollup_constants::riscv::REVEAL_REQUEST_MAX_SIZE;

/// Reveal request layout
pub type RevealRequestLayout = (DynArray<REVEAL_REQUEST_MAX_SIZE>, Atom<u64>);

/// Request content of reveal
pub struct RevealRequest<M: ManagerBase> {
    /// Reveal request payload
    pub bytes: DynCells<REVEAL_REQUEST_MAX_SIZE, M>,
    /// Size of reveal request payload
    pub size: Cell<u64, M>,
}

impl<M: ManagerBase> RevealRequest<M> {
    /// Bind the reveal request to the given allocated region.
    pub fn bind(space: AllocatedOf<RevealRequestLayout, M>) -> Self {
        Self {
            bytes: space.0,
            size: space.1,
        }
    }

    /// Given a manager morphism `f : &M -> N`, return the reveal request layout's
    /// allocated structure containing the constituents of `N` that were produced
    /// from the constituents of `&M`.
    pub fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(
        &'a self,
    ) -> AllocatedOf<RevealRequestLayout, F::Output> {
        (self.bytes.struct_ref::<F>(), self.size.struct_ref::<F>())
    }

    pub fn to_vec(&self) -> Vec<u8>
    where
        M: ManagerRead,
    {
        use std::cmp::min;

        let size = self.size.read() as usize;
        let mut buffer = vec![0u8; min(size, REVEAL_REQUEST_MAX_SIZE)];
        self.bytes.read_all(0, &mut buffer);
        buffer
    }
}

impl<M: ManagerClone> Clone for RevealRequest<M> {
    fn clone(&self) -> Self {
        Self {
            bytes: self.bytes.clone(),
            size: self.size.clone(),
        }
    }
}

// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::state_backend::{Atom, Cell, DynArray, DynCells, ManagerBase};
use tezos_smart_rollup_constants::riscv::REVEAL_REQUEST_MAX_SIZE;

/// Request content of reveal
pub struct RevealRequest<M: ManagerBase> {
    // Reveal request payload
    pub bytes: DynCells<REVEAL_REQUEST_MAX_SIZE, M>,
    // Size of reveal request payload
    pub size: Cell<u64, M>,
}

// Reveal request layout
pub type RevealRequestLayout = (DynArray<4096>, Atom<u64>);

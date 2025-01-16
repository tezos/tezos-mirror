use crate::state_backend::{Atom, Cell, DynArray, DynCells, ManagerBase};

/// Request content of reveal
pub struct RevealRequest<M: ManagerBase> {
    // Reveal request payload
    pub bytes: DynCells<4096, M>,
    // Size of reveal request payload
    pub size: Cell<u64, M>,
}

// Reveal request layout
pub type RevealRequestLayout = (DynArray<4096>, Atom<u64>);

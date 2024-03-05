use crate::{
    machine_state::{bus::Address, csregisters, mode, registers},
    state_backend::{self as backend, Atom, Cell},
};

/// RISC-V hart state
pub struct HartState<M: backend::Manager> {
    /// Integer registers
    pub xregisters: registers::XRegisters<M>,

    /// Floating-point number registers
    pub fregisters: registers::FRegisters<M>,

    /// Control and state registers
    pub csregisters: csregisters::CSRegisters<M>,

    /// Current running mode of hart
    pub mode: mode::ModeCell<M>,

    /// Program counter
    pub pc: Cell<Address, M>,
}

/// Layout of [HartState]
pub type HartStateLayout = (
    registers::XRegistersLayout,
    registers::FRegistersLayout,
    csregisters::CSRegistersLayout,
    mode::ModeLayout,
    Atom<Address>, // Program counter layout
);

impl<M: backend::Manager> HartState<M> {
    /// Bind the hart state to the given allocated space.
    pub fn bind(space: backend::AllocatedOf<HartStateLayout, M>) -> Self {
        Self {
            xregisters: registers::XRegisters::bind(space.0),
            fregisters: registers::FRegisters::bind(space.1),
            csregisters: csregisters::CSRegisters::bind(space.2),
            mode: mode::ModeCell::bind(space.3),
            pc: Cell::bind(space.4),
        }
    }

    /// Reset the hart state.
    pub fn reset(&mut self, pc: Address) {
        self.xregisters.reset();
        self.fregisters.reset();
        self.csregisters.reset();
        self.mode.reset();
        self.pc.write(pc);
    }
}
#[cfg(test)]
mod tests {
    use crate::{
        backend_test,
        machine_state::hart_state::{HartState, HartStateLayout},
        state_backend::tests::test_determinism,
    };

    backend_test!(test_hart_state_reset, F, {
        proptest::proptest!(|(pc: u64)| {
            test_determinism::<F, HartStateLayout, _>(|space| {
                let mut hart = HartState::bind(space);
                hart.reset(pc);
            });
        });
    });
}

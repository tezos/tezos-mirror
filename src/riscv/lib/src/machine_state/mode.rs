// SPDX-FileCopyrightText: 2023-2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::{
    machine_state::csregisters::Privilege,
    state_backend::{EnumCell, EnumCellLayout},
};

/// Modes the hardware state can be in when running code
#[derive(Debug, PartialEq, PartialOrd, Eq, Copy, Clone, strum::EnumIter)]
#[repr(u8)]
pub enum Mode {
    User = 0b000,
    Supervisor = 0b001,
    Machine = 0b011,
}

impl Mode {
    /// Obtain the corresponding [`Privilege`] for [`Mode`].
    pub fn privilege(&self) -> Privilege {
        match self {
            Mode::User => Privilege::Unprivileged,
            Mode::Supervisor => Privilege::Supervisor,
            Mode::Machine => Privilege::Machine,
        }
    }
}

impl From<u8> for Mode {
    #[inline]
    fn from(value: u8) -> Self {
        match value {
            0 => Mode::User,
            1 => Mode::Supervisor,
            _ => Mode::Machine,
        }
    }
}

impl Default for Mode {
    #[inline]
    fn default() -> Self {
        Self::Machine
    }
}

impl From<Mode> for u8 {
    #[inline]
    fn from(value: Mode) -> Self {
        value as u8
    }
}

/// Hart mode cell
pub type ModeCell<M> = EnumCell<Mode, u8, M>;

/// Layout for [Mode]
pub type ModeLayout = EnumCellLayout<u8>;

/// Modes the hardware state can trap into, a sub-enum of [`Mode`]
#[derive(Debug, PartialEq, PartialOrd, Eq, Copy, Clone, strum::EnumIter)]
#[repr(u8)]
pub enum TrapMode {
    Supervisor = Mode::Supervisor as u8,
    Machine = Mode::Machine as u8,
}

impl TrapMode {
    /// Construct the mode corresponding to the trap mode.
    pub fn as_mode(&self) -> Mode {
        match self {
            Self::Supervisor => Mode::Supervisor,
            Self::Machine => Mode::Machine,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        backend_test, create_state,
        machine_state::{
            backend::tests::test_determinism,
            mode::{Mode, ModeCell, ModeLayout},
        },
    };
    use strum::IntoEnumIterator;

    #[test]
    fn test_mode_reset() {
        test_determinism::<ModeLayout, _>(|space| {
            let mut mode1 = ModeCell::bind(space);
            mode1.reset();
        });
    }

    backend_test!(test_mode_read_write, F, {
        Mode::iter().for_each(|mode| {
            let first_value = mode;

            let mut inst = create_state!(ModeCell, ModeLayout, F);

            inst.write(first_value);
            assert_eq!(inst.read(), first_value);

            let value_read = bincode::serialize(&inst.struct_ref()).unwrap()[0];
            assert_eq!(Mode::from(value_read), first_value);
        });

        assert_eq!(Mode::from(42), Mode::default());
    });
}

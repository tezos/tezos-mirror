// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::machine_state::{
    backend::{self, Cell},
    csregisters::Privilege,
};

/// Modes the hardware state can be in when running code
#[derive(Debug, PartialEq, PartialOrd, Eq, Copy, Clone, strum::EnumIter)]
#[repr(u8)]
pub enum Mode {
    User = 0b000,
    Supervisor = 0b001,
    Machine = 0b011,
    Debug = 0b100,
}

impl Mode {
    /// Obtain the corresponding [`Privilege`] for [`Mode`].
    pub fn privilege(&self) -> Privilege {
        match self {
            Mode::User => Privilege::Unprivileged,
            Mode::Supervisor => Privilege::Supervisor,
            Mode::Machine => Privilege::Machine,
            Mode::Debug => Privilege::Machine,
        }
    }
}

impl TryFrom<u8> for Mode {
    type Error = String;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        let x = match value {
            0 => Mode::User,
            1 => Mode::Supervisor,
            3 => Mode::Machine,
            4 => Mode::Debug,
            _ => return Err(format!("Invalid value for Mode: {}", value)),
        };

        Ok(x)
    }
}

/// Hart mode
pub struct ModeCell<M: backend::Manager> {
    cell: Cell<u8, M>,
}

impl<M: backend::Manager> ModeCell<M> {
    #[inline(always)]
    pub fn write(&mut self, mode: Mode) {
        self.cell.write(mode as u8)
    }

    #[inline(always)]
    pub fn read(&self) -> Mode {
        let data = self.cell.read();
        Mode::try_from(data).unwrap()
    }
}

/// Layout for [Mode]
pub type ModeLayout = backend::Atom<u8>;

impl<M: backend::Manager> ModeCell<M> {
    /// Bind the mode cell to the given allocated space.
    pub fn bind(space: backend::AllocatedOf<ModeLayout, M>) -> Self {
        Self { cell: space }
    }

    /// Reset to the initial state.
    pub fn reset(&mut self, mode: Mode) {
        self.write(mode);
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        backend_test,
        machine_state::{
            backend::{tests::test_determinism, Backend, Layout},
            mode::{Mode, ModeCell, ModeLayout},
        },
    };
    use strum::IntoEnumIterator;

    backend_test!(test_mode_reset, F, {
        Mode::iter().for_each(|mode| {
            test_determinism::<F, ModeLayout, _>(|space| {
                let mut mode1 = ModeCell::bind(space);
                mode1.reset(mode);
            });
        });
    });

    backend_test!(test_mode_read_write, F, {
        let mut backend = F::new::<ModeLayout>();

        Mode::iter().for_each(|mode| {
            let first_value = mode;
            let offset = {
                let loc = ModeLayout::placed().into_location();
                let offset = loc.offset();
                let mut inst = ModeCell::bind(backend.allocate(loc));

                inst.write(first_value);
                assert_eq!(inst.read(), first_value);

                offset
            };

            let first_value_read = &mut [72u8];
            backend.read(offset, first_value_read);
            let value_read = first_value_read[0];
            assert_eq!(Mode::try_from(value_read), Ok(first_value));
        });

        assert!(Mode::try_from(42).is_err());
    });
}

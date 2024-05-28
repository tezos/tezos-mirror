// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! We reserve an address space of size [DEVICES_ADDRESS_SPACE_LENGTH] dedicated
//! to devices.

use super::Addressable;
use crate::machine_state::backend::{self, Region};

/// Length of the devices address space
pub const DEVICES_ADDRESS_SPACE_LENGTH: u64 = 2 * 1024 * 1024 * 1024;

/// Layout for the devices state
// Note, the [DevicesLayout] is not required to have the same size as the
// address space it represents. The address space is an interface detail,
// the layout size is a state implementation detail.
pub type DevicesLayout = backend::Array<u8, 1>;

/// Devices state
pub struct Devices<M: backend::Manager> {
    _placeholder: M::Region<u8, 1>,
}

impl<M: backend::Manager> Devices<M> {
    /// Bind the devices state.
    pub fn bind(space: backend::AllocatedOf<DevicesLayout, M>) -> Self {
        Self {
            _placeholder: space,
        }
    }

    /// Reset the devices state.
    pub fn reset(&mut self) {
        self._placeholder.write(0, 0);
    }
}

impl<E: backend::Elem, M: backend::Manager> Addressable<E> for Devices<M> {
    fn read(&self, _addr: super::Address) -> Result<E, super::OutOfBounds> {
        Err(super::OutOfBounds)
    }

    fn write(&mut self, _addr: super::Address, _value: E) -> Result<(), super::OutOfBounds> {
        Err(super::OutOfBounds)
    }

    fn write_all(
        &mut self,
        _addr: super::Address,
        _values: &[E],
    ) -> Result<(), super::OutOfBounds> {
        Err(super::OutOfBounds)
    }
}

#[cfg(test)]
mod tests {
    use super::{Devices, DevicesLayout};
    use crate::{
        backend_test,
        machine_state::backend::tests::{test_determinism, ManagerFor},
    };

    backend_test!(test_reset, F, {
        test_determinism::<F, DevicesLayout, _>(|space| {
            let mut devices: Devices<ManagerFor<'_, F, DevicesLayout>> = Devices::bind(space);
            devices.reset();
        });
    });
}

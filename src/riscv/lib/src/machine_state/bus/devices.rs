// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! We reserve an address space of size [DEVICES_ADDRESS_SPACE_LENGTH] dedicated
//! to devices.

use super::{AddressableRead, AddressableWrite};
use crate::machine_state::backend;

/// Length of the devices address space
pub const DEVICES_ADDRESS_SPACE_LENGTH: u64 = 2 * 1024 * 1024 * 1024;

/// Layout for the devices state
// Note, the [DevicesLayout] is not required to have the same size as the
// address space it represents. The address space is an interface detail,
// the layout size is a state implementation detail.
pub type DevicesLayout = backend::Array<u8, 1>;

/// Devices state
pub struct Devices<M: backend::ManagerBase> {
    _placeholder: backend::Cells<u8, 1, M>,
}

impl<M: backend::ManagerBase> Devices<M> {
    /// Bind the devices state.
    pub fn bind(space: backend::AllocatedOf<DevicesLayout, M>) -> Self {
        Self {
            _placeholder: space,
        }
    }

    /// Obtain a structure with references to the bound regions of this type.
    pub fn struct_ref(&self) -> backend::AllocatedOf<DevicesLayout, backend::Ref<'_, M>> {
        self._placeholder.struct_ref()
    }

    /// Reset the devices state.
    pub fn reset(&mut self)
    where
        M: backend::ManagerWrite,
    {
        self._placeholder.write(0, 0);
    }
}

impl<E: backend::Elem, M: backend::ManagerBase> AddressableRead<E> for Devices<M> {
    fn read(&self, _addr: super::Address) -> Result<E, super::OutOfBounds> {
        Err(super::OutOfBounds)
    }

    fn read_all(&self, _addr: super::Address, _values: &mut [E]) -> Result<(), super::OutOfBounds> {
        Err(super::OutOfBounds)
    }
}

impl<E: backend::Elem, M: backend::ManagerBase> AddressableWrite<E> for Devices<M> {
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
    use crate::machine_state::backend::tests::test_determinism;

    #[test]
    fn test_reset() {
        test_determinism::<DevicesLayout, _>(|space| {
            let mut devices: Devices<_> = Devices::bind(space);
            devices.reset();
        });
    }
}

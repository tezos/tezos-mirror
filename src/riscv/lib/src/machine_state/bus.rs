// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

pub mod devices;
pub mod main_memory;

use crate::machine_state::{backend, registers};
use derive_more::Error;
use std::mem;
use tezos_smart_rollup_constants::riscv::SbiError;

/// Bus address
pub type Address = registers::XValue;

/// An address is out of bounds.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Error, derive_more::Display)]
pub struct OutOfBounds;

impl From<OutOfBounds> for SbiError {
    fn from(_value: OutOfBounds) -> Self {
        SbiError::InvalidAddress
    }
}

/// Addressable space for reading
pub trait AddressableRead<E: backend::Elem> {
    /// Read an element of type `E` from the given address.
    fn read(&self, addr: Address) -> Result<E, OutOfBounds>;

    /// Read elements of type `E` from the given address.
    fn read_all(&self, addr: Address, values: &mut [E]) -> Result<(), OutOfBounds>;
}

/// Addressable space for writing
pub trait AddressableWrite<E: backend::Elem> {
    /// Write an element of type `E` to the given address.
    fn write(&mut self, addr: Address, value: E) -> Result<(), OutOfBounds>;

    /// Write consecutive elements of type `E` starting from the given address.
    fn write_all(&mut self, addr: Address, values: &[E]) -> Result<(), OutOfBounds>;
}

/// Address space identifier
#[derive(Debug, strum::EnumIter, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum AddressSpace {
    Devices,
    MainMemory,
    OutOfBounds,
}

impl AddressSpace {
    /// Determine which address space the address belongs to.
    #[inline(always)]
    fn locate<ML: main_memory::MainMemoryLayout>(addr: Address) -> (Self, Address) {
        // Is the address within the devices address space?
        if addr < devices::DEVICES_ADDRESS_SPACE_LENGTH {
            return (AddressSpace::Devices, addr);
        }

        let mem_addr = addr.saturating_sub(devices::DEVICES_ADDRESS_SPACE_LENGTH);
        let mem_size: u64 = ML::BYTES.try_into().expect("ML::BYTES out of bounds");

        // Is the address within the main memory address space?
        if mem_addr < mem_size {
            return (AddressSpace::MainMemory, mem_addr);
        }

        // Address is entirely out of bounds.
        let oob_addr = mem_addr.saturating_sub(mem_size);
        (AddressSpace::OutOfBounds, oob_addr)
    }

    /// Get the start of the address space.
    fn start<ML: main_memory::MainMemoryLayout>(&self) -> Address {
        match self {
            AddressSpace::Devices => 0,
            AddressSpace::MainMemory => devices::DEVICES_ADDRESS_SPACE_LENGTH,
            AddressSpace::OutOfBounds => {
                let mem_size: u64 = ML::BYTES.try_into().expect("ML::BYTES out of bounds");
                devices::DEVICES_ADDRESS_SPACE_LENGTH + mem_size
            }
        }
    }
}

/// Layout of the Bus
pub type BusLayout<ML> = (devices::DevicesLayout, ML);

/// Bus connects to the main memory and other devices.
pub struct Bus<ML: main_memory::MainMemoryLayout, M: backend::ManagerBase> {
    devices: devices::Devices<M>,
    memory: main_memory::MainMemory<ML, M>,
}

impl<ML: main_memory::MainMemoryLayout, M: backend::ManagerBase> Bus<ML, M> {
    /// Bind the Bus state to the allocated space.
    pub fn bind(space: backend::AllocatedOf<BusLayout<ML>, M>) -> Self {
        Self {
            devices: devices::Devices::bind(space.0),
            memory: main_memory::MainMemory::bind(space.1),
        }
    }

    /// Obtain a structure with references to the bound regions of this type.
    pub fn struct_ref(&self) -> backend::AllocatedOf<BusLayout<ML>, backend::Ref<'_, M>> {
        (self.devices.struct_ref(), self.memory.struct_ref())
    }

    /// Reset the bus state.
    pub fn reset(&mut self)
    where
        M: backend::ManagerWrite,
    {
        self.devices.reset();
        self.memory.reset();
    }
}

/// Address of where the main memory starts.
pub fn start_of_main_memory<ML: main_memory::MainMemoryLayout>() -> Address {
    AddressSpace::MainMemory.start::<ML>()
}

impl<E, ML, M> AddressableRead<E> for Bus<ML, M>
where
    E: backend::Elem,
    ML: main_memory::MainMemoryLayout,
    M: backend::ManagerRead,
    devices::Devices<M>: AddressableRead<E>,
    main_memory::MainMemory<ML, M>: AddressableRead<E>,
{
    #[inline(always)]
    fn read(&self, addr: Address) -> Result<E, OutOfBounds> {
        let (addr_space, local_address) = AddressSpace::locate::<ML>(addr);
        match addr_space {
            AddressSpace::Devices => self.devices.read(local_address),
            AddressSpace::MainMemory => self.memory.read(local_address),
            AddressSpace::OutOfBounds => Err(OutOfBounds),
        }
    }

    #[inline(always)]
    fn read_all(&self, addr: Address, values: &mut [E]) -> Result<(), OutOfBounds> {
        let end_addr = addr + mem::size_of_val(values).saturating_sub(1) as Address;

        let (addr_space, local_addr) = AddressSpace::locate::<ML>(addr);
        let (end_addr_space, _) = AddressSpace::locate::<ML>(end_addr);

        if addr_space != end_addr_space {
            // We don't allow cross-address space reads
            return Err(OutOfBounds);
        }

        match addr_space {
            AddressSpace::Devices => self.devices.read_all(local_addr, values),
            AddressSpace::MainMemory => self.memory.read_all(local_addr, values),
            AddressSpace::OutOfBounds => Err(OutOfBounds),
        }
    }
}

impl<E, ML, M> AddressableWrite<E> for Bus<ML, M>
where
    E: backend::Elem,
    ML: main_memory::MainMemoryLayout,
    M: backend::ManagerWrite,
    devices::Devices<M>: AddressableWrite<E>,
    main_memory::MainMemory<ML, M>: AddressableWrite<E>,
{
    #[inline(always)]
    fn write(&mut self, addr: Address, value: E) -> Result<(), OutOfBounds> {
        let (addr_space, local_address) = AddressSpace::locate::<ML>(addr);
        match addr_space {
            AddressSpace::Devices => self.devices.write(local_address, value),
            AddressSpace::MainMemory => self.memory.write(local_address, value),
            AddressSpace::OutOfBounds => Err(OutOfBounds),
        }
    }

    #[inline(always)]
    fn write_all(&mut self, addr: Address, values: &[E]) -> Result<(), OutOfBounds> {
        let end_addr = addr + mem::size_of_val(values).saturating_sub(1) as Address;

        let (addr_space, local_addr) = AddressSpace::locate::<ML>(addr);
        let (end_addr_space, _) = AddressSpace::locate::<ML>(end_addr);

        if addr_space != end_addr_space {
            // We don't allow cross-address space writes
            return Err(OutOfBounds);
        }

        match addr_space {
            AddressSpace::Devices => self.devices.write_all(local_addr, values),
            AddressSpace::MainMemory => self.memory.write_all(local_addr, values),
            AddressSpace::OutOfBounds => Err(OutOfBounds),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{main_memory::tests::T1K, AddressSpace, Bus, BusLayout};
    use crate::machine_state::backend::tests::test_determinism;
    use strum::IntoEnumIterator;

    #[test]
    fn test_reset() {
        test_determinism::<BusLayout<T1K>, _>(|space| {
            let mut bus: Bus<T1K, _> = Bus::bind(space);
            bus.reset();
        });
    }

    #[test]
    fn test_addr_spaces_start() {
        for addr_space in AddressSpace::iter() {
            let start = addr_space.start::<T1K>();
            let (round_trip, in_addr_space) = AddressSpace::locate::<T1K>(start);
            assert_eq!(addr_space, round_trip);
            assert_eq!(in_addr_space, 0);
        }
    }
}

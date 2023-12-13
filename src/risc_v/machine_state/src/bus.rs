// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

pub mod main_memory;

use crate::{backend, registers};
use std::marker::PhantomData;

/// Bus address
pub type Address = registers::XValue;

/// Addressable space
pub trait Addressable<E: backend::Elem> {
    /// Read an element of type `E` from the given address.
    fn read(&self, addr: Address) -> E;

    /// Write an element of type `E` to the given address.
    fn write(&mut self, addr: Address, value: E);
}

/// Layout of the Bus
pub struct BusLayout<ML> {
    _pd: PhantomData<ML>,
}

impl<ML: main_memory::MainMemoryLayout> backend::Layout for BusLayout<ML> {
    type Placed = (backend::PlacedOf<main_memory::M1G>, backend::PlacedOf<ML>);

    fn place_with(alloc: &mut backend::Choreographer) -> Self::Placed {
        (main_memory::M1G::place_with(alloc), ML::place_with(alloc))
    }

    type Allocated<M: backend::Manager> = (
        backend::AllocatedOf<main_memory::M1G, M>,
        usize,
        backend::AllocatedOf<ML, M>,
    );

    fn allocate<M: backend::Manager>(backend: &mut M, placed: Self::Placed) -> Self::Allocated<M> {
        let offset = <main_memory::M1G as main_memory::MainMemoryLayout>::offset(&placed.0);
        (
            main_memory::M1G::allocate(backend, placed.0),
            offset,
            ML::allocate(backend, placed.1),
        )
    }
}

/// Bus connects to the main memory and other devices.
pub struct Bus<ML: main_memory::MainMemoryLayout, M: backend::Manager> {
    devices_placeholder: main_memory::MainMemory<main_memory::M1G, M>,
    memory_offset: Address,
    memory: main_memory::MainMemory<ML, M>,
}

impl<ML: main_memory::MainMemoryLayout, M: backend::Manager> Bus<ML, M> {
    /// Bind the Bus state to the allocated space.
    pub fn new_in(space: backend::AllocatedOf<BusLayout<ML>, M>) -> Self {
        Self {
            devices_placeholder: space.0,
            memory_offset: space.1 as Address,
            memory: main_memory::MainMemory::new_in(space.2),
        }
    }
}

impl<E, ML, M> Addressable<E> for Bus<ML, M>
where
    E: backend::Elem,
    ML: main_memory::MainMemoryLayout,
    M: backend::Manager,
    main_memory::MainMemory<ML, M>: Addressable<E>,
    main_memory::MainMemory<main_memory::M1G, M>: Addressable<E>,
{
    #[inline(always)]
    fn read(&self, addr: Address) -> E {
        if addr < self.memory_offset {
            self.devices_placeholder
                .read(addr.saturating_sub(self.memory_offset))
        } else {
            self.memory.read(addr)
        }
    }

    #[inline(always)]
    fn write(&mut self, addr: Address, value: E) {
        if addr < self.memory_offset {
            self.devices_placeholder
                .write(addr.saturating_sub(self.memory_offset), value)
        } else {
            self.memory.write(addr, value)
        }
    }
}

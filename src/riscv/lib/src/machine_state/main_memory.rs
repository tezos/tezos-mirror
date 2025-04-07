// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024-2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    machine_state::registers::XValue,
    state_backend::{self as backend, ManagerDeserialise, ManagerSerialise},
};
use serde::{Deserialize, Serialize};
use std::mem;
use tezos_smart_rollup_constants::riscv::SbiError;
use thiserror::Error;

/// Bus address
pub type Address = XValue;

/// An address is out of bounds.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Error, derive_more::Display)]
pub struct OutOfBounds;

impl From<OutOfBounds> for SbiError {
    fn from(_value: OutOfBounds) -> Self {
        SbiError::InvalidAddress
    }
}

/// The first valid memory address.
pub const FIRST_ADDRESS: Address = 0;

/// Generates a [`backend::DynArray`] layout type given a memory size.
macro_rules! gen_memory_layout {
    ($name:ident = $size_in_g:literal GiB) => {
        pub type $name = crate::state_backend::DynArray<{ $size_in_g * 1024 * 1024 * 1024 }>;
    };

    ($name:ident = $size_in_m:literal MiB) => {
        pub type $name = crate::state_backend::DynArray<{ $size_in_m * 1024 * 1024 }>;
    };

    ($name:ident = $size_in_k:literal KiB) => {
        pub type $name = crate::state_backend::DynArray<{ $size_in_k * 1024 }>;
    };
}

gen_memory_layout!(M1K = 1 KiB);
gen_memory_layout!(M8K = 8 KiB);
gen_memory_layout!(M1M = 1 MiB);
gen_memory_layout!(M64M = 64 MiB);
gen_memory_layout!(M1G = 1 GiB);
gen_memory_layout!(M4G = 4 GiB);

/// Main memory layout, i.e. specifies how much memory there is
// XXX: We can't associate these layout types directly with [`backend::DynArray`] because
// inherent associated types are unstable. Hence we must go through a dummy
// trait.
pub trait MainMemoryLayout: backend::ProofLayout + backend::CommitmentLayout + 'static {
    const BYTES: usize;

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    fn data_struct_ref<
        'a,
        M: backend::ManagerBase + 'a,
        F: backend::FnManager<backend::Ref<'a, M>>,
    >(
        data: &'a Self::Allocated<M>,
    ) -> backend::AllocatedOf<Self, F::Output>;

    /// Read an element in the region. `address` is in bytes.
    fn data_read<E: backend::Elem, M: backend::ManagerRead>(
        data: &Self::Allocated<M>,
        address: usize,
    ) -> E;

    /// Read elements from the region. `address` is in bytes.
    fn data_read_all<E: backend::Elem, M: backend::ManagerRead>(
        data: &Self::Allocated<M>,
        address: usize,
        values: &mut [E],
    );

    /// Update an element in the region. `address` is in bytes.
    fn data_write<E: backend::Elem, M: backend::ManagerWrite>(
        data: &mut Self::Allocated<M>,
        address: usize,
        value: E,
    );

    /// Update multiple elements in the region. `address` is in bytes.
    fn data_write_all<E: backend::Elem, M: backend::ManagerWrite>(
        data: &mut Self::Allocated<M>,
        address: usize,
        values: &[E],
    );

    /// Serialise main memory's dynamic region.
    fn serialise_data<M: ManagerSerialise, S: serde::Serializer>(
        data: &Self::Allocated<M>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>;

    /// Deserialise main memory's dynamic region.
    fn deserialise_data<'de, M: ManagerDeserialise, D: serde::Deserializer<'de>>(
        deserializer: D,
    ) -> Result<Self::Allocated<M>, D::Error>;

    /// Clone the dynamic region.
    fn clone_data<M: backend::ManagerClone>(data: &Self::Allocated<M>) -> Self::Allocated<M>;
}

impl<const BYTES: usize> MainMemoryLayout for backend::DynArray<BYTES> {
    const BYTES: usize = BYTES;

    fn data_struct_ref<'a, M: backend::ManagerBase, F: backend::FnManager<backend::Ref<'a, M>>>(
        data: &'a Self::Allocated<M>,
    ) -> backend::AllocatedOf<Self, F::Output> {
        data.struct_ref::<F>()
    }

    fn data_read<E: backend::Elem, M: backend::ManagerRead>(
        data: &Self::Allocated<M>,
        address: usize,
    ) -> E {
        data.read(address)
    }

    fn data_read_all<E: backend::Elem, M: backend::ManagerRead>(
        data: &Self::Allocated<M>,
        address: usize,
        values: &mut [E],
    ) {
        data.read_all(address, values);
    }

    fn data_write<E: backend::Elem, M: backend::ManagerWrite>(
        data: &mut Self::Allocated<M>,
        address: usize,
        value: E,
    ) {
        data.write(address, value);
    }

    fn data_write_all<E: backend::Elem, M: backend::ManagerWrite>(
        data: &mut Self::Allocated<M>,
        address: usize,
        values: &[E],
    ) {
        data.write_all(address, values);
    }

    fn serialise_data<M: ManagerSerialise, S: serde::Serializer>(
        data: &Self::Allocated<M>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        data.serialize(serializer)
    }

    fn deserialise_data<'de, M: ManagerDeserialise, D: serde::Deserializer<'de>>(
        deserializer: D,
    ) -> Result<Self::Allocated<M>, D::Error> {
        serde::Deserialize::deserialize(deserializer)
    }

    fn clone_data<M: backend::ManagerClone>(data: &Self::Allocated<M>) -> Self::Allocated<M> {
        data.clone()
    }
}

/// Main memory state for the given layout
pub struct MainMemory<L: MainMemoryLayout + ?Sized, M: backend::ManagerBase> {
    pub data: backend::AllocatedOf<L, M>,
}

impl<L: MainMemoryLayout, M: backend::ManagerBase> MainMemory<L, M> {
    /// Bind the main memory state to the given allocated space.
    pub fn bind(space: backend::AllocatedOf<L, M>) -> Self {
        MainMemory { data: space }
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: backend::FnManager<backend::Ref<'a, M>>>(
        &'a self,
    ) -> backend::AllocatedOf<L, F::Output>
    where
        M: 'a,
    {
        L::data_struct_ref::<_, F>(&self.data)
    }

    /// Reset to the initial state.
    pub fn reset(&mut self)
    where
        M: backend::ManagerWrite,
    {
        for i in 0..L::BYTES {
            L::data_write(&mut self.data, i, 0u8);
        }
    }

    /// Read a value from memory.
    #[inline(always)]
    pub fn read<E: backend::Elem>(&self, addr: Address) -> Result<E, OutOfBounds>
    where
        M: backend::ManagerRead,
    {
        if addr as usize + mem::size_of::<E>() > L::BYTES {
            return Err(OutOfBounds);
        }

        Ok(L::data_read(&self.data, addr as usize))
    }

    /// Read multiple values from memory.
    #[inline(always)]
    pub fn read_all<E: backend::Elem>(
        &self,
        addr: Address,
        values: &mut [E],
    ) -> Result<(), OutOfBounds>
    where
        M: backend::ManagerRead,
    {
        if addr as usize + mem::size_of_val(values) > L::BYTES {
            return Err(OutOfBounds);
        }

        L::data_read_all(&self.data, addr as usize, values);

        Ok(())
    }

    /// Write a value to memory.
    pub fn write<E: backend::Elem>(&mut self, addr: Address, value: E) -> Result<(), OutOfBounds>
    where
        M: backend::ManagerWrite,
    {
        if addr as usize + mem::size_of::<E>() > L::BYTES {
            return Err(OutOfBounds);
        }

        L::data_write(&mut self.data, addr as usize, value);

        Ok(())
    }

    /// Write multiple values to memory.
    pub fn write_all<E: backend::Elem>(
        &mut self,
        addr: Address,
        values: &[E],
    ) -> Result<(), OutOfBounds>
    where
        M: backend::ManagerWrite,
    {
        let addr = addr as usize;

        if addr + mem::size_of_val(values) > L::BYTES {
            return Err(OutOfBounds);
        }

        L::data_write_all(&mut self.data, addr, values);

        Ok(())
    }
}

impl<L: MainMemoryLayout, M: ManagerSerialise> Serialize for MainMemory<L, M> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        L::serialise_data(&self.data, serializer)
    }
}

impl<'de, L, M> Deserialize<'de> for MainMemory<L, M>
where
    L: MainMemoryLayout,
    M: ManagerDeserialise,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let data = L::deserialise_data(deserializer)?;
        Ok(Self { data })
    }
}

impl<L: MainMemoryLayout, M: backend::ManagerClone> Clone for MainMemory<L, M> {
    fn clone(&self) -> Self {
        Self {
            data: L::clone_data(&self.data),
        }
    }
}

impl<L: MainMemoryLayout, M: backend::ManagerRead, N: backend::ManagerRead>
    PartialEq<MainMemory<L, N>> for MainMemory<L, M>
{
    fn eq(&self, other: &MainMemory<L, N>) -> bool {
        (0..L::BYTES)
            .all(|i| L::data_read::<u8, _>(&self.data, i) == L::data_read::<u8, _>(&other.data, i))
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::{backend_test, create_state};

    gen_memory_layout!(T1K = 1 KiB);

    backend_test!(test_endianess, F, {
        let mut memory = create_state!(MainMemory, T1K, F, T1K);

        memory.write(0, 0x1122334455667788u64).unwrap();

        macro_rules! check_address {
            ($ty:ty, $addr:expr, $value:expr) => {
                assert_eq!(memory.read::<$ty>($addr), Ok($value));
            };
        }

        check_address!(u64, 0, 0x1122334455667788);

        check_address!(u32, 0, 0x55667788);
        check_address!(u32, 4, 0x11223344);

        check_address!(u16, 0, 0x7788);
        check_address!(u16, 2, 0x5566);
        check_address!(u16, 4, 0x3344);
        check_address!(u16, 6, 0x1122);

        check_address!(u8, 0, 0x88);
        check_address!(u8, 1, 0x77);
        check_address!(u8, 2, 0x66);
        check_address!(u8, 3, 0x55);
        check_address!(u8, 4, 0x44);
        check_address!(u8, 5, 0x33);
        check_address!(u8, 6, 0x22);
        check_address!(u8, 7, 0x11);
    });
}

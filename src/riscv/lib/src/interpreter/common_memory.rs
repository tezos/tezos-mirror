// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    machine_state::{
        bus::{main_memory::MainMemoryLayout, AddressableRead, AddressableWrite, OutOfBounds},
        registers::XRegister,
        AccessType, MachineCoreState,
    },
    state_backend as backend,
    traps::Exception,
};

impl<ML, M> MachineCoreState<ML, M>
where
    ML: MainMemoryLayout,
    M: backend::ManagerReadWrite,
{
    /// Generic read function for loading `mem::size_of<T>` bytes from `address`
    pub(super) fn read_from_address<T: backend::Elem>(
        &mut self,
        address: u64,
    ) -> Result<T, Exception> {
        let address = self.translate(address, AccessType::Load)?;

        self.bus
            .read(address)
            .map_err(|_: OutOfBounds| Exception::LoadAccessFault(address))
    }

    /// Generic read function for loading `mem::size_of<T>` bytes from address val(rs1) + imm
    pub(super) fn read_from_bus<T: backend::Elem>(
        &mut self,
        imm: i64,
        rs1: XRegister,
    ) -> Result<T, Exception> {
        let address = self.hart.xregisters.read(rs1).wrapping_add(imm as u64);
        self.read_from_address(address)
    }

    /// Generic store-operation for writing `mem::size_of<T>` bytes starting at `address`
    pub(super) fn write_to_address<T: backend::Elem>(
        &mut self,
        address: u64,
        value: T,
    ) -> Result<(), Exception> {
        let address = self.translate(address, AccessType::Store)?;

        self.bus
            .write(address, value)
            .map_err(|_: OutOfBounds| Exception::StoreAMOAccessFault(address))
    }

    /// Generic store-operation for writing `mem::size_of<T>` bytes starting at address val(rs1) + imm
    pub(super) fn write_to_bus<T: backend::Elem>(
        &mut self,
        imm: i64,
        rs1: XRegister,
        value: T,
    ) -> Result<(), Exception> {
        let address = self.hart.xregisters.read(rs1).wrapping_add(imm as u64);
        self.write_to_address(address, value)
    }
}

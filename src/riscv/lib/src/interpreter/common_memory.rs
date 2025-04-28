// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::machine_state::MachineCoreState;
use crate::machine_state::memory;
use crate::machine_state::memory::BadMemoryAccess;
use crate::machine_state::memory::Memory;
use crate::machine_state::registers::XRegister;
use crate::state_backend as backend;
use crate::traps::Exception;

impl<MC, M> MachineCoreState<MC, M>
where
    MC: memory::MemoryConfig,
    M: backend::ManagerReadWrite,
{
    /// Generic read function for loading `mem::size_of<T>` bytes from `address`
    pub(super) fn read_from_address<T: backend::Elem>(
        &mut self,
        address: u64,
    ) -> Result<T, Exception> {
        self.main_memory
            .read(address)
            .map_err(|_: BadMemoryAccess| Exception::LoadAccessFault(address))
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
        self.main_memory
            .write(address, value)
            .map_err(|_: BadMemoryAccess| Exception::StoreAMOAccessFault(address))
    }

    /// Generic store operation for writing `mem::size_of<T>` bytes starting at address val(rs1) + imm
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

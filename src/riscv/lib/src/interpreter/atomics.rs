// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Core logic for atomic instructions

use crate::{
    machine_state::{bus::main_memory::MainMemoryLayout, registers::XRegister, MachineState},
    state_backend as backend,
    traps::Exception,
};
use std::mem;

pub const SC_SUCCESS: u64 = 0;
pub const SC_FAILURE: u64 = 1;

impl<ML, M> MachineState<ML, M>
where
    ML: MainMemoryLayout,
    M: backend::ManagerReadWrite,
{
    /// Loads a word or a double from the address in `rs1`, places the
    /// sign-extended value in `rd`, and registers a reservation set for
    /// that address.
    /// See also [crate::machine_state::reservation_set].
    pub(super) fn run_lr<T: backend::Elem>(
        &mut self,
        rs1: XRegister,
        rd: XRegister,
        from: fn(T) -> u64,
    ) -> Result<(), Exception> {
        let address_rs1 = self.hart.xregisters.read(rs1);

        // "The A extension requires that the address held in rs1 be naturally
        // aligned to the size of the operand (i.e., eight-byte aligned for
        // 64-bit words and four-byte aligned for 32-bit words). If the address
        // is not naturally aligned, an address-misaligned exception or
        // an access-fault exception will be generated."
        if address_rs1 % mem::size_of::<T>() as u64 != 0 {
            return Err(Exception::LoadAccessFault(address_rs1));
        }

        // Load the value from address in rs1
        let value_rs1: T = self.read_from_address(address_rs1)?;

        // Register a reservation set for the address in rs1 and write
        // the value at that address to rd
        self.hart.reservation_set.set(address_rs1);
        self.hart.xregisters.write(rd, from(value_rs1));

        Ok(())
    }

    /// `SC.W` R-type instruction
    ///
    /// Conditionally writes a word in `rs2` to the address in `rs1`.
    /// SC.W succeeds only if the reservation is still valid and
    /// the reservation set contains the bytes being written.
    /// In case of success, write 0 in `rd`, otherwise write 1.
    /// See also [crate::machine_state::reservation_set].
    /// The `aq` and `rl` bits specify additional memory constraints in
    /// multi-hart environments so they are currently ignored.
    pub(super) fn run_sc<T: backend::Elem>(
        &mut self,
        rs1: XRegister,
        rs2: XRegister,
        rd: XRegister,
        to: fn(u64) -> T,
    ) -> Result<(), Exception> {
        let address_rs1 = self.hart.xregisters.read(rs1);

        // "The A extension requires that the address held in rs1 be naturally
        // aligned to the size of the operand (i.e., eight-byte aligned for
        // 64-bit words and four-byte aligned for 32-bit words). If the address
        // is not naturally aligned, an address-misaligned exception or
        // an access-fault exception will be generated."
        if address_rs1 % mem::size_of::<T>() as u64 != 0 {
            self.hart.reservation_set.reset();
            return Err(Exception::StoreAMOAccessFault(address_rs1));
        }

        if self.hart.reservation_set.test_and_unset(address_rs1) {
            // If the address in rs1 belongs to a valid reservation, write
            // the value in rs2 to this address and return success.
            let value_rs2 = to(self.hart.xregisters.read(rs2));
            self.hart.xregisters.write(rd, SC_SUCCESS);
            self.write_to_address(address_rs1, value_rs2)
        } else {
            // If the address in rs1 does not belong to a valid reservation or
            // there is no valid reservation set on the hart, do not write to
            // memory and return failure.
            self.hart.xregisters.write(rd, SC_FAILURE);
            Ok(())
        }
    }

    /// Generic implementation of any atomic memory operation, implementing
    /// read-modify-write operations for multi-processor synchronisation
    /// (Section 8.4)
    fn run_amo<T: backend::Elem>(
        &mut self,
        rs1: XRegister,
        rs2: XRegister,
        rd: XRegister,
        f: fn(T, T) -> T,
        to: fn(u64) -> T,
        from: fn(T) -> u64,
    ) -> Result<(), Exception> {
        let address_rs1 = self.hart.xregisters.read(rs1);

        // "The A extension requires that the address held in rs1 be naturally
        // aligned to the size of the operand (i.e., eight-byte aligned for
        // 64-bit words and four-byte aligned for 32-bit words). If the address
        // is not naturally aligned, an address-misaligned exception or
        // an access-fault exception will be generated."
        if address_rs1 % mem::size_of::<T>() as u64 != 0 {
            return Err(Exception::StoreAMOAccessFault(address_rs1));
        }

        // Load the value from address in rs1
        let value_rs1: T = self.read_from_address(address_rs1)?;

        // Apply the binary operation to the loaded value and the value in rs2
        let value_rs2 = to(self.hart.xregisters.read(rs2));
        let value = f(value_rs1, value_rs2);

        // Write the value read fom the address in rs1 in rd
        self.hart.xregisters.write(rd, from(value_rs1));

        // Store the resulting value to the address in rs1
        self.write_to_address(address_rs1, value)
    }

    /// Generic implementation of an atomic memory operation which works on
    /// 32-bit values
    pub(super) fn run_amo_w(
        &mut self,
        rs1: XRegister,
        rs2: XRegister,
        rd: XRegister,
        f: fn(i32, i32) -> i32,
    ) -> Result<(), Exception> {
        self.run_amo(rs1, rs2, rd, f, |x| x as i32, |x| x as u64)
    }

    /// Generic implementation of an atomic memory operation which works on
    /// 64-bit values
    pub(super) fn run_amo_d(
        &mut self,
        rs1: XRegister,
        rs2: XRegister,
        rd: XRegister,
        f: fn(u64, u64) -> u64,
    ) -> Result<(), Exception> {
        self.run_amo(rs1, rs2, rd, f, |x| x, |x| x)
    }
}

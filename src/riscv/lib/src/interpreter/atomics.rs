// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Core logic for atomic instructions

use std::mem;

use crate::instruction_context::ICB;
use crate::instruction_context::LoadStoreWidth;
use crate::instruction_context::arithmetic::Arithmetic;
use crate::machine_state::MachineCoreState;
use crate::machine_state::memory;
use crate::machine_state::registers::XRegister;
use crate::state_backend as backend;
use crate::traps::Exception;

pub const SC_SUCCESS: u64 = 0;
pub const SC_FAILURE: u64 = 1;

impl<MC, M> MachineCoreState<MC, M>
where
    MC: memory::MemoryConfig,
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

/// Generic implementation of any atomic memory operation which works on 64-bit values,
/// implementing read-modify-write operations for multi-processor synchronisation
/// (Section 8.4)
fn run_x64_atomic<I: ICB>(
    icb: &mut I,
    rs1: XRegister,
    rs2: XRegister,
    rd: XRegister,
    f: fn(I::XValue, I::XValue, &mut I) -> I::XValue,
) -> I::IResult<()> {
    let address_rs1 = icb.xregister_read(rs1);

    // Handle the case where the address is not aligned.
    let result = icb.atomic_access_fault_guard(address_rs1, LoadStoreWidth::Double);

    // Continue with the operation if the address is aligned.
    let val_rs1_result = I::and_then(result, |_| {
        icb.main_memory_load(address_rs1, false, LoadStoreWidth::Double)
    });

    // Continue with the operation if the load was successful.
    I::and_then(val_rs1_result, |val_rs1| {
        // Apply the binary operation to the loaded value and the value in rs2
        let val_rs2 = icb.xregister_read(rs2);
        let res = f(val_rs1, val_rs2, icb);

        // Write the value read fom the address in rs1 in rd
        icb.xregister_write(rd, val_rs1);

        // Store the resulting value to the address in rs1
        icb.main_memory_store(address_rs1, res, LoadStoreWidth::Double)
    })
}

/// Loads in rd the value from the address in rs1 and stores the result of
/// adding it to val(rs2) back to the address in rs1.
#[expect(
    unused_variables,
    reason = "The `aq` and `rl` bits specify additional memory constraints 
    in multi-hart environments so they are currently ignored."
)]
pub fn run_x64_atomic_add<I: ICB>(
    icb: &mut I,
    rs1: XRegister,
    rs2: XRegister,
    rd: XRegister,
    aq: bool,
    rl: bool,
) -> I::IResult<()> {
    run_x64_atomic(icb, rs1, rs2, rd, |x, y, icb| x.add(y, icb))
}

#[cfg(test)]
mod test {
    use proptest::prelude::*;

    use crate::backend_test;
    use crate::machine_state::MachineCoreState;
    use crate::machine_state::registers::a0;
    use crate::machine_state::registers::a1;
    use crate::machine_state::registers::a2;

    macro_rules! test_atomic {
        ($(#[$m:meta])* $name: ident, $instr: path, $f: expr, $align: expr, $t: ty) => {
            backend_test!($name, F, {
                use $crate::machine_state::memory::M4K;
                use $crate::state::NewState;

                let state = MachineCoreState::<M4K, _>::new(&mut F::manager());
                let state_cell = std::cell::RefCell::new(state);

                proptest!(|(
                    r1_addr in (0..1023_u64/$align).prop_map(|x| x * $align),
                    r1_val in any::<u64>(),
                    r2_val in any::<u64>(),
                )| {
                    let mut state = state_cell.borrow_mut();
                    state.reset();
                    state.main_memory.set_all_readable_writeable();

                    state.hart.xregisters.write(a0, r1_addr);
                    state.write_to_bus(0, a0, r1_val)?;
                    state.hart.xregisters.write(a1, r2_val);
                    match $instr(&mut *state, a0, a1, a2, false, false) {
                        Ok(_) => {}
                        Err(e) => panic!("Error: {:?}", e),
                    }
                    let res: $t = state.read_from_address(r1_addr)?;

                    prop_assert_eq!(
                        state.hart.xregisters.read(a2) as $t, r1_val as $t);

                    let f = $f;
                    let expected = f(r1_val as $t, r2_val as $t);
                    prop_assert_eq!(res, expected);
                })
            });

        }
    }

    test_atomic!(
        test_run_x64_atomic_add,
        super::run_x64_atomic_add,
        u64::wrapping_add,
        8,
        u64
    );
}

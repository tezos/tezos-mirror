//! Implementation of RV_64_I extension for RISC-V
//!
//! Chapter 5 - Unprivileged spec

use crate::{
    backend,
    registers::{XRegister, XRegisters},
};

impl<M> XRegisters<M>
where
    M: backend::Manager,
{
    /// run "addiw" instruction
    pub fn run_addiw(&mut self, imm: i64, rs1: XRegister, rd: XRegister) {
        // Perform addition only on the lower 32 bits, ignoring the upper 32 bits.
        // We do not need to explicitly truncate for the lower bits since wrapping_add
        // has the same semantics & result on the lower 32 bits irrespective of bit width
        let rval = self.read(rs1);
        let result = rval.wrapping_add(imm as u64);
        // Truncate result to use only the lower 32 bits, then sign-extend to 64 bits.
        let result = result as i32 as u64;
        self.write(rd, result);
    }
}

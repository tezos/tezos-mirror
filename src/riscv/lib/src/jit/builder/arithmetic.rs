// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of arithmetic instructions in JIT mode.

use cranelift::codegen::ir::InstBuilder;

use super::Builder;
use super::X32;
use super::X64;
use crate::instruction_context::Shift;
use crate::instruction_context::arithmetic::Arithmetic;
use crate::jit::state_access::JitStateAccess;
use crate::machine_state::memory::MemoryConfig;

impl<MC: MemoryConfig, JSA: JitStateAccess> Arithmetic<Builder<'_, MC, JSA>> for X64 {
    fn add(self, other: Self, icb: &mut Builder<'_, MC, JSA>) -> Self {
        let res = icb.builder.ins().iadd(self.0, other.0);
        X64(res)
    }

    fn sub(self, other: Self, icb: &mut Builder<'_, MC, JSA>) -> Self {
        let res = icb.builder.ins().isub(self.0, other.0);
        X64(res)
    }

    fn and(self, other: Self, icb: &mut Builder<'_, MC, JSA>) -> Self {
        let res = icb.builder.ins().band(self.0, other.0);
        X64(res)
    }

    fn or(self, other: Self, icb: &mut Builder<'_, MC, JSA>) -> Self {
        let res = icb.builder.ins().bor(self.0, other.0);
        X64(res)
    }

    fn xor(self, other: Self, icb: &mut Builder<'_, MC, JSA>) -> Self {
        let res = icb.builder.ins().bxor(self.0, other.0);
        X64(res)
    }

    fn mul(self, other: Self, icb: &mut Builder<'_, MC, JSA>) -> Self {
        let res = icb.builder.ins().imul(self.0, other.0);
        X64(res)
    }

    fn div_signed(self, other: Self, icb: &mut Builder<'_, MC, JSA>) -> Self {
        let res = icb.builder.ins().sdiv(self.0, other.0);
        X64(res)
    }

    fn negate(self, icb: &mut Builder<'_, MC, JSA>) -> Self {
        X64(icb.builder.ins().ineg(self.0))
    }

    fn shift(self, shift: Shift, amount: Self, icb: &mut Builder<'_, MC, JSA>) -> Self {
        match shift {
            Shift::Left => X64(icb.builder.ins().ishl(self.0, amount.0)),
            Shift::RightUnsigned => X64(icb.builder.ins().ushr(self.0, amount.0)),
            Shift::RightSigned => X64(icb.builder.ins().sshr(self.0, amount.0)),
        }
    }

    fn modulus(self, other: Self, icb: &mut Builder<'_, MC, JSA>) -> Self {
        X64(icb.builder.ins().srem(self.0, other.0))
    }
}

impl<MC: MemoryConfig, JSA: JitStateAccess> Arithmetic<Builder<'_, MC, JSA>> for X32 {
    fn add(self, other: Self, icb: &mut Builder<'_, MC, JSA>) -> Self {
        let res = icb.builder.ins().iadd(self.0, other.0);
        X32(res)
    }

    fn sub(self, other: Self, icb: &mut Builder<'_, MC, JSA>) -> Self {
        let res = icb.builder.ins().isub(self.0, other.0);
        X32(res)
    }

    fn and(self, other: Self, icb: &mut Builder<'_, MC, JSA>) -> Self {
        let res = icb.builder.ins().band(self.0, other.0);
        X32(res)
    }

    fn or(self, other: Self, icb: &mut Builder<'_, MC, JSA>) -> Self {
        let res = icb.builder.ins().bor(self.0, other.0);
        X32(res)
    }

    fn xor(self, other: Self, icb: &mut Builder<'_, MC, JSA>) -> Self {
        let res = icb.builder.ins().bxor(self.0, other.0);
        X32(res)
    }

    fn mul(self, other: Self, icb: &mut Builder<'_, MC, JSA>) -> Self {
        let res = icb.builder.ins().imul(self.0, other.0);
        X32(res)
    }

    fn div_signed(self, other: Self, icb: &mut Builder<'_, MC, JSA>) -> Self {
        let res = icb.builder.ins().sdiv(self.0, other.0);
        X32(res)
    }

    fn negate(self, icb: &mut Builder<'_, MC, JSA>) -> Self {
        X32(icb.builder.ins().ineg(self.0))
    }

    fn shift(self, shift: Shift, amount: Self, icb: &mut Builder<'_, MC, JSA>) -> Self {
        match shift {
            Shift::Left => X32(icb.builder.ins().ishl(self.0, amount.0)),
            Shift::RightUnsigned => X32(icb.builder.ins().ushr(self.0, amount.0)),
            Shift::RightSigned => X32(icb.builder.ins().sshr(self.0, amount.0)),
        }
    }

    fn modulus(self, other: Self, icb: &mut Builder<'_, MC, JSA>) -> Self {
        X32(icb.builder.ins().srem(self.0, other.0))
    }
}

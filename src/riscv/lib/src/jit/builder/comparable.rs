// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of comparison operations in JIT mode.

use cranelift::codegen::ir::InstBuilder;

use super::Builder;
use crate::instruction_context::ICB;
use crate::instruction_context::Predicate;
use crate::instruction_context::comparable::Comparable;
use crate::jit::state_access::JitStateAccess;
use crate::machine_state::memory::MemoryConfig;

impl<'a, MC: MemoryConfig, JSA: JitStateAccess> Comparable<Builder<'a, MC, JSA>>
    for <Builder<'a, MC, JSA> as ICB>::XValue
{
    // icmp returns 1 if the condition holds, 0 if it does not.
    //
    // This matches the required semantics of bool - namely that it coerces to XValue with
    // - true => 1
    // - false => 0
    //
    // See
    // <https://docs.rs/cranelift-codegen/0.117.2/cranelift_codegen/ir/trait.InstBuilder.html#method.icmp>
    fn compare(
        self,
        other: Self,
        comparison: Predicate,
        icb: &mut Builder<'a, MC, JSA>,
    ) -> <Builder<'a, MC, JSA> as ICB>::Bool {
        icb.builder.ins().icmp(comparison, self.0, other.0)
    }
}

impl<'a, MC: MemoryConfig, JSA: JitStateAccess> Comparable<Builder<'a, MC, JSA>>
    for <Builder<'a, MC, JSA> as ICB>::XValue32
{
    // icmp returns 1 if the condition holds, 0 if it does not.
    //
    // This matches the required semantics of bool - namely that it coerces to XValue with
    // - true => 1
    // - false => 0
    //
    // See
    // <https://docs.rs/cranelift-codegen/0.117.2/cranelift_codegen/ir/trait.InstBuilder.html#method.icmp>
    fn compare(
        self,
        other: Self,
        comparison: Predicate,
        icb: &mut Builder<'a, MC, JSA>,
    ) -> <Builder<'a, MC, JSA> as ICB>::Bool {
        icb.builder.ins().icmp(comparison, self.0, other.0)
    }
}

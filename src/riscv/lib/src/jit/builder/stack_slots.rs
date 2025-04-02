// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Loading and storing of temporary values to/from the stack.

use cranelift::codegen::ir::InstBuilder;
use cranelift::codegen::ir::StackSlot;
use cranelift::codegen::ir::StackSlotData;
use cranelift::codegen::ir::StackSlotKind;
use cranelift::codegen::ir::Type;
use cranelift::codegen::ir::Value;
use cranelift::codegen::ir::types::I64;
use cranelift::frontend::FunctionBuilder;

use super::X64;
use crate::machine_state::memory::Address;

const INSTR_PC_SIZE: u32 = std::mem::size_of::<Address>() as u32;
const INSTR_PC_ALIGN: u8 = std::mem::align_of::<Address>() as u8;

const INSTR_PC_STACK_SLOT_DATA: StackSlotData = StackSlotData {
    kind: StackSlotKind::ExplicitSlot,
    size: INSTR_PC_SIZE,
    align_shift: INSTR_PC_ALIGN,
};

const INSTR_PC_TYPE: Type = I64;

/// Certain information is occassionally required to be on the stack, when executing
/// a JIT-compiled block.
///
/// See for example [`handle_exception`]:
///
/// ```no_run
/// # use octez_riscv::state_backend::ManagerBase;
/// # use octez_riscv::machine_state::MachineCoreState;
/// # use octez_riscv::machine_state::memory::Address;
/// # use octez_riscv::machine_state::memory::MemoryConfig;
/// # use octez_riscv::traps::EnvironException;
/// # use octez_riscv::traps::Exception;
///
/// # trait JitStateAccess: ManagerBase {
/// extern "C" fn handle_exception<MC: MemoryConfig>(
///     core: &mut MachineCoreState<MC, Self>,
///     current_pc: &mut Address,
///     exception: &Option<Exception>,
///     result: &mut Result<(), EnvironException>,
/// ) -> bool;
/// # }
/// ```
///
/// The boolean return indicates whether the function has failed. If it has, then an exception will
/// have been written to the place in memory specified by `exception`. `current_pc` is also an out
/// pointer - if the exception is successfully handled, an updated pc is returned.
///
/// The most natural place to point to in memory, is the function stack. Certain values can safely
/// live in the registers, and so can be loaded from/stored to the stack.
///
/// Larger/more complex types cannot be moved between registers and the stack safely. Exceptions
/// are a good example of this. For now, we choose to pass pointers from values created in rust
/// code, for safety.
///
/// # Program Counter
///
/// The program counter can be safely loaded from/stored to the stack.
///
/// [`handle_exception`]: crate::jit::state_access::JitStateAccess::handle_exception
pub struct StackSlots {
    ptr_type: Type,
    /// Stack slot for program counter
    instr_pc: StackSlot,
    /// Pointer to the `instr_pc` stack slot.
    ///
    /// `None` indicates that the stack slot has not been initialised.
    instr_pc_ptr: Option<Value>,
}

impl StackSlots {
    /// Construct a new Stack Manager, given the native pointer type for the current platform.
    pub fn new(ptr_type: Type, builder: &mut FunctionBuilder<'_>) -> Self {
        let instr_pc = builder.create_sized_stack_slot(INSTR_PC_STACK_SLOT_DATA);

        Self {
            ptr_type,
            instr_pc,
            instr_pc_ptr: None,
        }
    }

    /// The address of an instruction program counter stored on the stack.
    ///
    /// Zero-initialises the value on the stack if the stack slot hasn't been created before.
    pub fn instr_pc_ptr(&mut self, builder: &mut FunctionBuilder<'_>) -> Value {
        if let Some(ptr) = self.instr_pc_ptr {
            return ptr;
        }

        let ptr = builder.ins().stack_addr(self.ptr_type, self.instr_pc, 0);
        let zero = builder.ins().iconst(INSTR_PC_TYPE, 0);
        builder.ins().stack_store(zero, self.instr_pc, 0);

        self.instr_pc_ptr = Some(ptr);

        ptr
    }

    /// Load a program counter from its place on the stack.
    ///
    /// If the stack slot has not been initialised, returns 0.
    pub fn instr_pc_load(&mut self, builder: &mut FunctionBuilder<'_>) -> X64 {
        let val = match self.instr_pc_ptr {
            None => builder.ins().iconst(INSTR_PC_TYPE, 0),
            Some(_) => builder.ins().stack_load(INSTR_PC_TYPE, self.instr_pc, 0),
        };

        X64(val)
    }

    /// Store a program counter to its place on the stack.
    ///
    /// Marks the stack slot as initialised.
    pub fn instr_pc_store(&mut self, builder: &mut FunctionBuilder<'_>, current_pc: X64) {
        builder.ins().stack_store(current_pc.0, self.instr_pc, 0);

        if self.instr_pc_ptr.is_none() {
            let ptr = builder.ins().stack_addr(INSTR_PC_TYPE, self.instr_pc, 0);

            self.instr_pc_ptr = Some(ptr);
        }
    }
}
